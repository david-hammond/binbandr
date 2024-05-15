#' Get linearly scaled variable based on optimal bins
#'
#' Finds outliers and then bands between 0 and 1 on optimal bins of
#' non-outlier data
#'
#'
#' @importFrom R6 R6Class
#' @importFrom dlookr binning extract
#' @importFrom dplyr mutate left_join group_by ungroup filter summarise relocate
#' @importFrom tidyr separate
#' @importFrom scales rescale
#' @importFrom readr parse_number
#' @importFrom rlang .data
#' @importFrom RSQLite dbConnect dbDisconnect SQLite
#' @importFrom data.tree as.Node
#' @importFrom dm decompose_table dm dm_add_pk dm_add_fk
#' @importFrom mice mice
#'
#' @export
#'



newindex <- R6::R6Class("newindex",
                            lock_objects = FALSE,
                            public = list(
                              name = NULL,
                              start_year = NULL,
                              end_year = NULL,
                              tree = NULL,
                              indicators = list(),
                              corpus = NULL,
                              dm = NULL,
                              imputed = NULL,
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
                                df$geoparent = ifelse(parse_number(df$admin_level[1])  == 0, df$geocode, substr(df$geocode,1,3))
                                df = df %>% relocate(geoparent)
                                self$indicators = c(self$indicators, newindicator$new(df %>%
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
                              structure = function(){
                                SetFormat(self$tree, "weight", formatFun = FormatPercent)
                                print(self$tree, "source", "weight")
                              },
                              calcIndex = function(){
                                self$dm = private$raw_dm(self$corpus)
                                self$dm = private$imputed_dm()
                                # con <- DBI::dbConnect(RSQLite::SQLite(), paste0(self$name, ".sqlite"))
                                # copy_dm_to(con, self$dm, set_key_constraints = TRUE, temporary = F)
                                # dbDisconnect(con)

                              }
                            ),
                        private = list(
                          fill_missing_values = function(df) {
                            df = df %>%
                              group_by(vuid, guid) %>%
                              complete(year = full_seq(year, 1)) %>%
                              mutate(interpolated = zoo::na.approx(banded, na.rm = FALSE)) %>%
                              fill(interpolated, .direction =  "updown") %>% #this doesn't extrapolate
                              ungroup() %>%
                              mutate(imputed = ifelse(is.na(banded), 1, imputed),
                                     banded = interpolated) %>%
                              select(-interpolated) %>%
                              complete(guid, vuid, year = full_seq(year, 1))


                            raw_pivot =  df %>% group_by(vuid, guid, year) %>% summarise(banded = mean(banded)) %>%
                              ungroup() %>% spread(vuid, banded)
                            names(raw_pivot) = make.names(names(raw_pivot))
                            imp = mice(raw_pivot[-c(1:2)],)
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
                            imputed_data = fill_missing_values(self$dm$data)
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

                            overall_scores = calculate_index(x)
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
                              dm_add_fk(imputed_pc, guid, geo)
                            return(x)

                          },
                          calculate_index = function(x){
                            domain_scores = x %>% dm_squash_to_tbl(data) %>%
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
                            all = domain_scores %>% rbind(overall_score)
                            return(all)
                          }

                        )

)


