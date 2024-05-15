#' Get linearly scaled variable based on optimal bins
#'
#' Finds outliers and then bands between 0 and 1 on optimal bins of
#' non-outlier data
#'
#'
#' @importFrom R6 R6Class
#' @importFrom dlookr binning extract
#' @importFrom dplyr mutate left_join group_by ungroup filter summarise relocate
#' @importFrom tidyr separate fill
#' @importFrom scales rescale
#' @importFrom readr parse_number
#' @importFrom rlang .data
#' @importFrom RSQLite dbConnect dbDisconnect SQLite
#' @importFrom data.tree as.Node SetFormat FormatPercent
#' @importFrom dm decompose_table dm dm_add_pk dm_add_fk dm_flatten_to_tbl dm_draw
#' @importFrom mice mice
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
                                                      ismorebetter,
                                                      weight,
                                                      manual_min_outlier_cutoff = NULL,
                                                      manual_max_outlier_cutoff = NULL,
                                                      banding_method = "optimal"){
                                df = df %>% select(geocode, geoname, admin_level, variablename, year, value, source)
                                df$geoparent = substr(df$geocode,1,3)
                                df = df %>% relocate(geoparent)
                                self$indicators = c(self$indicators, newIndicator$new(df %>%
                                                                                      filter(between(year, self$start_year, self$end_year)),
                                                                                      domain,
                                                                                      ismorebetter,
                                                                                      weight,
                                                                                      manual_min_outlier_cutoff,
                                                                                      manual_max_outlier_cutoff,
                                                                                      banding_method))
                                df = last(self$indicators)$data
                                df$ismorebetter = ismorebetter
                                df$weight = weight
                                df$domain = domain
                                self$corpus = self$corpus %>% rbind(df)
                                tmp = decompose_table(self$corpus, vuid, domain, variablename, source, ismorebetter, admin_level, weight)$parent_table
                                tmp$pathString = paste(self$name,
                                                     tmp$domain,
                                                     tmp$variablename,
                                                     sep = "/")
                                self$tree = as.Node(tmp)
                                self$dm = private$raw_dm(self$corpus)
                                names(self$indicators) = self$dm$meta$vcode

                              },
                              viewStructure = function(){
                                SetFormat(self$tree, "weight", formatFun = FormatPercent)
                                print(self$tree, "source", "weight")
                              },
                              calculateIndex = function(){
                                self$dm = private$raw_dm(self$corpus)
                                self$dm = private$imputed_dm()
                                self$indexflatdata = private$getFlatData()
                                # con <- DBI::dbConnect(RSQLite::SQLite(), paste0(self$name, ".sqlite"))
                                # copy_dm_to(con, self$dm, set_key_constraints = TRUE, temporary = F)
                                # dbDisconnect(con)

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
                            x = decompose_table(df, vuid, domain, variablename, source, ismorebetter, admin_level, weight)
                            meta = x$parent_table
                            meta$vcode = toupper(paste(meta$domain, meta$variablename))
                            meta$vcode = make.unique(abbreviate(abbreviate(toupper(meta$vcode), 3),3) )
                            x = decompose_table(x$child_table, guid, geoparent, geocode, geoname)
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
                              relocate(geoparent, geocode, geoname)
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
                              admin_level = NULL,
                              variablename = NULL,
                              source = NULL,
                              ismorebetter = NULL,
                              optimal_bins = NULL,
                              auto_min_outlier_cutoff = NULL,
                              auto_max_outlier_cutoff = NULL,
                              manual_min_outlier_cutoff = NULL,
                              manual_max_outlier_cutoff = NULL,
                              data = NULL,
                              weight = NULL,
                              banding_method = NULL,
                              imputation_type = NULL,
                              initialize = function(df,
                                                    domain,
                                                    ismorebetter,
                                                    weight,
                                                    manual_min_outlier_cutoff = NULL,
                                                    manual_max_outlier_cutoff = NULL,
                                                    banding_method = "optimal",
                                                    imputation_type = "mice"){
                                self$domain = domain
                                self$ismorebetter = ismorebetter
                                self$manual_min_outlier_cutoff = manual_min_outlier_cutoff #need to implement
                                self$manual_max_outlier_cutoff = manual_max_outlier_cutoff
                                self$variablename <- unique(df$variablename)
                                self$admin_level <- unique(df$admin_level)
                                self$source <- unique(df$source)
                                self$weight <- weight
                                self$banding_method <- banding_method
                                tmp = private$get_capping(df$value)
                                df$capped = tmp$values
                                df = private$binband(df)
                                self$optimal_bins <- dlookr::binning(df$value)
                                self$auto_min_outlier_cutoff <- tmp$caps[1]
                                self$auto_max_outlier_cutoff <- tmp$caps[2]
                                self$data = df
                              },
                              distribution = function(){
                                plot(self$optimal_bins)
                              }
                            ),
                            private = list(
                              binband = function(df,
                                                 target = "value"){
                                df = df %>%
                                  mutate(bins = binning(.data$capped) %>% extract()) %>%
                                  separate(.data$bins, c("lower", "upper"), sep = ",", remove = F) %>%
                                  mutate(lower = parse_number(.data$lower),
                                         upper = parse_number(.data$upper),
                                         bins = as.numeric(.data$bins))
                                nbins = max(df$bins)
                                nbins = data.frame(bins = seq(1:nbins)) %>%
                                  mutate(       bin_min_threshold = (.data$bins-1)/nbins,
                                                bin_max_threshold = (.data$bins/nbins))
                                df = df %>% left_join(nbins)
                                df = df %>% group_by(.data$bins) %>%
                                  mutate(banded = rescale(.data$capped, from = c(.data$lower[1], .data$upper[1]),
                                                          to = c(.data$bin_min_threshold[1], .data$bin_max_threshold[1]))) %>%
                                  ungroup() %>%
                                  mutate(banded = round(.data$banded, 3))
                                if(self$ismorebetter){
                                  df$banded = 1 - df$banded
                                }
                                return(df)
                              },
                              get_capping = function(data, cap_ntiles = c(0.01, 0.99)) {
                                hinges <- quantile(data, probs = c(0.25, 0.75), na.rm = TRUE)
                                caps <- quantile(data, probs = cap_ntiles, na.rm = TRUE)

                                whisker <- 1.5 * diff(hinges)

                                if(!is.null(self$manual_min_outlier_cutoff)){
                                  data[data < self$manual_min_outlier_cutoff] <- self$manual_min_outlier_cutoff
                                }else{
                                  data[data < (hinges[1] - whisker)] <- caps[1]
                                }
                                if(!is.null(self$manual_max_outlier_cutoff)){
                                  data[data > self$manual_max_outlier_cutoff] <- self$manual_max_outlier_cutoff
                                }else{
                                  data[data > (hinges[2] + whisker)] <- caps[2]
                                }
                                list(values = data, caps = caps)
                              }
                            )
)






