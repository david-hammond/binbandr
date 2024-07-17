#' Get linearly scaled variable based on optimal bins
#'
#' Finds outliers and then bands between 0 and 1 on optimal bins of
#' non-outlier data
#' @importFrom dlookr binning extract
#' @importFrom R6 R6Class
#' @importFrom dplyr mutate left_join group_by ungroup filter summarise relocate distinct
#' @importFrom tidyr separate fill
#' @importFrom scales rescale
#' @importFrom readr parse_number
#' @importFrom rlang .data
#' @importFrom RSQLite dbConnect dbDisconnect SQLite
#' @importFrom data.tree as.Node SetFormat FormatPercent FormatFixedDecimal
#' @importFrom dm decompose_table dm dm_add_pk dm_add_fk dm_flatten_to_tbl dm_draw copy_dm_to
#' @importFrom mice mice
#' @importFrom vctrs vec_duplicate_detect
#'
#' @export
#'



newIndex <- R6::R6Class("newIndex",
                            lock_objects = FALSE,
                            public = list(
                              name = NULL,
                              start_year = NULL,
                              end_year = NULL,
                              indicators = list(),
                              tree = NULL,
                              corpus = NULL,
                              dm = NULL,
                              indexflatdata = NULL,
                              initialize = function(name, start_year, end_year){
                                self$name = name
                                self$start_year = start_year
                                self$end_year = end_year
                              },
                              addIndicator = function(df,
                                                      domain,
                                                      weight,
                                                      cd){
                                options(error = NULL)
                                df = df %>% distinct()
                                test1 = setdiff(required_cols, names(df))
                                test2 = vec_duplicate_detect(df %>% select(geocode, variablename, year))
                                if(length(test1) > 0){
                                  stop(paste("Your data frame is missing the following required columns:", paste(test1, collapse = ",")))
                                }
                                if(sum(test2) > 0){
                                  print(df[test2,])
                                  stop("You have duplicated data in your data.frame, check the above entires, fix and retry")
                                }
                                df = df %>% select(required_cols)
                                df$geoparent = substr(df$geocode,1,3)
                                df = df %>% relocate(geoparent)
                                self$indicators = c(self$indicators, newIndicator$new(df %>%
                                                                                      filter(between(year, self$start_year, self$end_year)),
                                                                                      domain,
                                                                                      weight,
                                                                                      cd))
                                df = last(self$indicators)$data
                                df$ismorebetter = cd$polarity
                                df$weight = weight
                                df$domain = domain
                                df$year_earliest = last(self$indicators)$year_earliest
                                df$year_latest = last(self$indicators)$year_latest
                                df$number_of_geos = last(self$indicators)$number_of_geos
                                self$corpus = self$corpus %>% rbind(df)
                                tmp = decompose_table(self$corpus,
                                                      vuid,
                                                      domain,
                                                      variablename,
                                                      source,
                                                      ismorebetter,
                                                      admin_level,
                                                      weight,
                                                      year_earliest,
                                                      year_latest,
                                                      number_of_geos)$parent_table
                                tmp$pathString = paste(self$name,
                                                     tmp$domain,
                                                     tmp$variablename,
                                                     sep = "/")
                                self$tree = as.Node(tmp)
                                self$dm = private$raw_dm(self$corpus)
                                names(self$indicators) = self$dm$meta$vcode

                              },
                              viewStructure = function(){
                                SetFormat(self$tree, "weight", formatFun = function(x) FormatFixedDecimal(x, digits = 3))
                                SetFormat(self$tree, "year_earliest", formatFun = function(x) FormatFixedDecimal(x, digits = 0))
                                SetFormat(self$tree, "year_latest", formatFun = function(x) FormatFixedDecimal(x, digits = 0))
                                SetFormat(self$tree, "number_of_geos", formatFun = function(x) FormatFixedDecimal(x, digits = 0))
                                print(self$tree, "source", "weight", "year_earliest", "year_latest", "number_of_geos")
                              },
                              calculateIndex = function(export_to_db = FALSE){
                                self$dm = private$raw_dm(self$corpus)
                                self$dm = private$imputed_dm()
                                self$indexflatdata = private$getFlatData()
                                if(export_to_db){
                                  con <- DBI::dbConnect(RSQLite::SQLite(), paste0(self$name, ".sqlite"))
                                  copy_dm_to(con, self$dm, set_key_constraints = TRUE, temporary = F)
                                  dbDisconnect(con)
                                }


                              },
                              viewERDiagram = function(){
                                self$dm %>% dm_draw()
                              }
                            ),
                        private = list(
                          fill_missing_values = function(df) {
                            message("Interpolating...")
                            df = df %>%
                              group_by(vuid, guid) %>%
                              complete(year = full_seq(year, 1)) %>%
                              mutate(interpolated = zoo::na.approx(banded, na.rm = FALSE)) %>%
                              fill(interpolated, .direction =  "updown") %>% #this doesn't extrapolate
                              ungroup() %>%
                              mutate(imputed = ifelse(is.na(banded), 1, 0),
                                     banded = interpolated) %>%
                              select(-interpolated) %>%
                              complete(guid, vuid, year = full_seq(year, 1))


                            raw_pivot =  df %>% group_by(vuid, guid, year) %>% summarise(banded = mean(banded)) %>%
                              ungroup() %>% spread(vuid, banded)
                            names(raw_pivot) = make.names(names(raw_pivot))
                            message("Imputing...")
                            imp = mice(raw_pivot[-c(1:2)], print = FALSE)
                            raw_pivot[-c(1:2)] = complete(imp)
                            raw_pivot = raw_pivot %>% gather(vuid, mice, -c(guid, year)) %>%
                              mutate(vuid = parse_number(vuid))

                            df = df %>% left_join(raw_pivot)
                            df = df %>% mutate(imputed = ifelse(is.na(banded), 2, imputed),
                                               banded = ifelse(is.na(banded), mice, banded)) %>%
                              select(-mice)
                            return(df)
                          },
                          raw_dm = function(df){
                            x = decompose_table(df, vuid,
                                                domain,
                                                variablename,
                                                source,
                                                ismorebetter,
                                                admin_level,
                                                weight,
                                                lowercutoff,
                                                uppercutoff,
                                                banding_method,
                                                year_earliest,
                                                year_latest,
                                                number_of_geos)
                            meta = x$parent_table
                            meta$vcode = toupper(paste(meta$domain, meta$variablename))
                            meta$vcode = make.unique(abbreviate(abbreviate(toupper(meta$vcode), 3),3) )
                            x = decompose_table(x$child_table, guid, geoparent, geocode)
                            geo = x$parent_table
                            data = x$child_table
                            x = dm(meta, geo, data) %>%
                              dm_add_pk(meta, vuid) %>%
                              dm_add_pk(geo, guid) %>%
                              dm_add_fk(data, vuid, meta) %>%
                              dm_add_fk(data, guid, geo)

                            return(x)

                          },
                          imputed_dm = function(){
                            imputed_data = private$fill_missing_values(self$dm$data)
                            world_average = imputed_data %>% group_by(vuid, year) %>%
                              summarise(mean_value = mean(value, na.rm = T),
                                        mean_banded = mean(banded, na.rm = T)) %>%
                              ungroup()
                            imputed_pc = imputed_data %>% group_by(guid) %>%
                              summarise(pc_imputed = sum(imputed > 1)/n())

                            x = dm(meta = self$dm$meta, geo = self$dm$geo,
                                   data = imputed_data, world_average) %>%
                              dm_add_pk(meta, vuid) %>%
                              dm_add_pk(geo, guid) %>%
                              dm_add_fk(data, vuid, meta) %>%
                              dm_add_fk(data, guid, geo) %>%
                              dm_add_fk(world_average, vuid, meta)
                            message("Calculating Index...")
                            overall_scores = private$calculate_index(x)
                            imp_key = data.frame(imputed = c(0,1,2),
                                                 imptype = c("Original", "Interpolated/Extrapolated", "Multi Chain Imputation"),
                                                 impcategory = c("None", "Cold Deck", "Hot Deck"))
                            x = dm(meta = self$dm$meta, geo = self$dm$geo,
                                   data = imputed_data, world_average,
                                   overall_scores, imp_key, imputed_pc) %>%
                              dm_add_pk(meta, vuid) %>%
                              dm_add_pk(geo, guid) %>%
                              dm_add_pk(imp_key, imputed) %>%
                              dm_add_fk(data, vuid, meta) %>%
                              dm_add_fk(data, guid, geo) %>%
                              dm_add_fk(data, imputed, imp_key) %>%
                              dm_add_fk(world_average, vuid, meta) %>%
                              dm_add_fk(overall_scores, guid, geo) %>%
                              dm_add_fk(imputed_pc, guid, geo) %>%
                              dm_add_fk(overall_scores, imputed, imp_key)
                            message("Done...")
                            return(x)

                          },
                          calculate_index = function(x){
                            domain_scores = x %>% dm_flatten_to_tbl(.start = data) %>%
                              group_by(guid, domain, year) %>%
                              mutate(banded = weight * banded) %>%
                              summarise(banded = sum(banded)/sum(weight), weight = sum(weight)) %>%
                              ungroup() %>% mutate(domain = paste(domain,"Overall Score")) %>%
                              rename(variablename = domain)
                            overall_score = domain_scores %>%
                              group_by(guid, year) %>%
                              summarise(banded = sum(banded)/sum(weight), weight = sum(weight)) %>%
                              mutate(variablename = paste(ni$name, "Overall Score")) %>%
                              ungroup() %>%
                              select(guid, variablename, year, weight, banded)
                            all = domain_scores %>% rbind(overall_score) %>%
                              mutate(domain = self$name, value = banded, imputed = 0) %>%
                              select(domain, variablename, guid, year, value, banded, imputed)
                            return(all)
                          },
                          getFlatData = function(){
                            x = self$dm %>% dm_flatten_to_tbl(.start = overall_scores) %>%
                              select(-guid, -imputed)
                            y = self$dm %>% dm_flatten_to_tbl(.start = data)
                            z = x %>% rbind(y[,names(x)])
                            y = self$dm %>% dm_flatten_to_tbl(.start = imputed_pc) %>%
                              select(-guid)
                            z = z %>% left_join(y) %>%
                              relocate(geoparent, geocode)
                            return(z)
                          }

                        )

)

# Class Defintion ---------------------------------------------------------
# This class is used to represent a "task" in our R program.

newIndicator <- R6::R6Class("newIndicator",
                            lock_objects = FALSE,
                            public = list(
                              domain = NULL,
                              variablename = NULL,
                              source = NULL,
                              ismorebetter = NULL,
                              geocode = NULL,
                              region = NULL,
                              year = NULL,
                              value = NULL,
                              weight = NULL,
                              normalised = NULL,
                              interpolated = NULL,
                              initialize = function(domain,
                                                    variablename,
                                                    source,
                                                    ismorebetter,
                                                    geocode,
                                                    region,
                                                    year,
                                                    value,
                                                    weight,
                                                    normaliser,
                                                    start_year,
                                                    end_year){
                                self$domain = unique(domain)
                                self$ismorebetter = ismorebetter
                                self$variablename <- unique(variablename)
                                self$source <- unique(source)
                                self$weight <- weight
                                self$year_earliest = min(year)
                                self$year_latest = max(year)
                                self$number_of_geos = length(unique(geocode))
                                self$fitted_distribution = normaliser$fitted_distribution
                                self$normaliser = normaliser
                                ### get data
                                data <- self$interpolate(geocode, region, year, value,
                                                    start_year, end_year)
                                self$value = data$value
                                self$interpolated = data$interpolated
                                self$normalised = normaliser$applyto(data$value)
                                self$geocode = data$geocode
                                self$region = data$region
                                self$year = data$year
                                tmp = self$split_data()
                                self$meta = tmp$meta
                                self$regional = tmp$regional
                                self$data = tmp$data
                              },
                              combine_data = function(){
                                data.frame(domain = self$domain,
                                           variablename = self$variablename,
                                           source = self$source,
                                           ismorebetter = self$ismorebetter,
                                           weight = self$weight,
                                           fitted_distr = self$fitted_distribution,
                                           bins = paste(levels(cut(
                                             self$normaliser$breaks,
                                             self$normaliser$breaks,
                                             include.lowest = T)),
                                             collapse = '\n'),
                                           earliest_year = self$year_earliest,
                                           latest_year = self$year_latest,
                                           num_geos = self$number_of_geos,
                                           geocode = self$geocode,
                                           region = self$region,
                                           value = self$value,
                                           year = self$year,
                                           banded = self$normalised$normalised_data,
                                           interpolated = self$interpolated) %>%
                                  group_by(variablename, region, year) %>%
                                  mutate(regional_mean_raw = mean(value),
                                          regional_mean_banded = mean(banded)) %>%
                                  ungroup()


                              },
                              split_data = function(){
                                x = self$combine_data()
                                tmp = decompose_table(x, vid,
                                                      domain,
                                                      variablename,
                                                      source,
                                                      ismorebetter,
                                                      weight,
                                                      fitted_distr,
                                                      bins,
                                                      earliest_year,
                                                      latest_year,
                                                      num_geos)
                                meta = tmp$parent_table
                                tmp = decompose_table(tmp$child_table, rid, vid, region, regional_mean_raw,
                                                       regional_mean_banded)
                                regional_data = tmp$parent_table
                                data = tmp$child_table
                                return(list(meta = meta, regional = regional_data, data = data))
                              },


                              interpolate = function(geocode, region, year, value,
                                                     start_year, end_year){
                                tmp <- data.frame(geocode = geocode,
                                                  region = region,
                                                  year = year,
                                                  value = value) %>%
                                  complete(nesting(geocode, region), year = start_year:end_year) %>%
                                  mutate(interpolated = zoo::na.approx(value, na.rm = FALSE)) %>%
                                  fill(interpolated, .direction =  "updown") %>% #this doesn't extrapolate
                                  ungroup()
                                interp = is.na(tmp$value)
                                tmp = tmp %>%
                                  mutate(value = ifelse(is.na(value), interpolated, value))
                                tmp$interpolated = ifelse(interp, "*", "")
                                return(tmp)
                              }
                            )
)


required_cols = c("geocode", "admin_level", "variablename", "year", "value", "source")

