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
#' @importFrom tidymodlr tidymodl
#'
#' @export
#'



newIndex <- R6::R6Class("newIndex",
                            lock_objects = FALSE,
                            public = list(
                              name = NULL,
                              start_year = NULL,
                              end_year = NULL,
                              peg_year = NULL,
                              indicators = list(),
                              tree = NULL,
                              meta = NULL,
                              data = NULL,
                              regional = NULL,
                              dm = NULL,
                              indexflatdata = NULL,
                              initialize = function(name, start_year, end_year, peg_year){
                                self$name = name
                                self$start_year = start_year
                                self$end_year = end_year
                                self$peg_year = peg_year
                              },
                              addIndicator = function(domain,
                                                      variablename,
                                                      source,
                                                      ismorebetter,
                                                      geocode,
                                                      #region,
                                                      year,
                                                      value,
                                                      weight){
                                ###TODO MAKE ALL CHARACTERS
                                options(error = NULL)
                                df = data.frame(geocode, year, value)
                                region = countrycode(geocode, "iso3c", "region")
                                test = duplicated(df)
                                if(sum(test) > 0){
                                  print(df[test,])
                                  stop("You have duplicated data in your data.frame, check the above entires, fix and retry")
                                }
                                included_years = between(year, self$start_year, self$end_year)
                                cmd <- sprintf("self$indicators  <- c(self$indicators , '%s' = newIndicator$new(domain = unique(domain),
                                                                                      variablename = unique(variablename),
                                                                                      source = unique(source),
                                                                                      ismorebetter = ismorebetter,
                                                                                      geocode = geocode[included_years],
                                                                                      region = region[included_years],
                                                                                      year = year[included_years],
                                                                                      value = value[included_years],
                                                                                      weight = weight,
                                                                                      start_year =self$start_year,
                                                                                      end_year = self$end_year,
                                                                                      peg_year = self$peg_year))",
                                               unique(variablename))
                                eval(parse(text = cmd))
                               tmp = last(self$indicators)$split_data()
                               if(is.null(self$meta)){
                                  self$meta = tmp$meta
                                  self$data = tmp$data
                                  self$regional = tmp$regional

                               }else{
                                 vid_max = max(self$meta$vid)
                                 self$meta = self$meta %>% rbind(tmp$meta %>% mutate(vid = vid + vid_max))
                                 self$regional = self$regional%>% rbind(tmp$regional %>% mutate(vid = vid + vid_max))
                                 self$data = self$data %>% rbind(tmp$data %>%
                                                                           mutate(vid = vid + vid_max))
                               }

                            },
                            impute = function(){
                              y = self$data
                              y = y %>% complete(geocode, year, vid) %>%
                                group_by(geocode) %>%
                                fill(region, .direction = 'updown') %>%
                                ungroup() %>%
                                left_join(self$regional) %>%
                                mutate(regional_average = ifelse(is.na(value), "*", "")) %>%
                                mutate(value = ifelse(is.na(value), regional_mean_raw, value),
                                       banded = ifelse(is.na(banded), regional_mean_banded, banded)) %>%
                                select(-regional_mean_raw, -regional_mean_banded) %>%
                                mutate(interpolated = replace_na(interpolated, ""))
                              #Still gotta mice
                              #y = tidymodlr::tidymodl$new(x$imputed %>% select(geocode, year, vid, banded) %>% filter(!is.na(banded)), "vid", "banded")
                              #this has duplications!!!!
                              self$imputed = y
                            },
                            calculate_index = function(){
                              self$impute()
                              domain_scores = self$imputed %>% left_join(self$meta) %>%
                                group_by(geocode, domain, year) %>%
                                mutate(banded = weight * banded) %>%
                                summarise(banded = sum(banded)/sum(weight), weight = sum(weight)) %>%
                                ungroup() %>% mutate(domain = paste(domain,"Overall Score")) %>%
                                rename(variablename = domain)
                              overall_score = domain_scores %>%
                                mutate(banded = weight * banded) %>%
                                group_by(geocode, year) %>%
                                summarise(banded = sum(banded)/sum(weight), weight = sum(weight)) %>%
                                mutate(variablename = paste(self$name, "Overall Score")) %>%
                                ungroup() %>%
                                select(geocode, variablename, year, weight, banded)
                              all = domain_scores %>% rbind(overall_score) %>%
                                mutate(domain = self$name, value = banded, imputed = 0) %>%
                                select(domain, variablename, geocode, year, value, weight)
                              self$index = all
                        }),
                        private = list(

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
                              normaliser = NULL,
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
                                                    start_year,
                                                    end_year,
                                                    peg_year,
                                                    classint_preference = "jenks",
                                                    num_classes_pref = NULL){
                                self$domain = unique(domain)
                                self$ismorebetter = ismorebetter
                                self$variablename <- unique(variablename)
                                self$source <- unique(source)
                                self$weight <- weight
                                self$year_earliest = min(year)
                                self$year_latest = max(year)
                                self$number_of_geos = length(unique(geocode))

                                ### get data
                                data <- self$interpolate(geocode, region, year, value,
                                                    start_year, end_year)
                                normaliser = distrnorm$new(data$value[data$year == peg_year],
                                                                polarity = ismorebetter,
                                                                classint_preference = classint_preference,
                                                                num_classes_pref = num_classes_pref)
                                self$fitted_distribution = normaliser$fitted_distribution
                                self$value = data$value
                                self$interpolated = data$interpolated
                                self$normaliser = normaliser$applyto(data$value)
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
                                           normalisation_technique = self$normaliser$normalisation,
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
                                           banded = self$normaliser$normalised_data,
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
                                                      normalisation_technique,
                                                      bins,
                                                      earliest_year,
                                                      latest_year,
                                                      num_geos)
                                meta = tmp$parent_table
                                # tmp = decompose_table(tmp$child_table, rid, region, regional_mean_raw,
                                #                        regional_mean_banded)
                                regional = tmp$child_table %>%
                                  select(vid, year, region, regional_mean_raw, regional_mean_banded) %>%
                                  distinct()
                                data = tmp$child_table %>%
                                  select(-regional_mean_raw, -regional_mean_banded) %>%
                                  distinct()
                                return(list(meta = meta, regional = regional, data = data))
                              },


                              interpolate = function(geocode, region, year, value,
                                                     start_year, end_year){
                                tmp <- data.frame(geocode = geocode,
                                                  region = region,
                                                  year = year,
                                                  value = value) %>%
                                  complete(nesting(geocode, region), year = start_year:end_year) %>%
                                  group_by(geocode, region) %>%
                                  mutate(interpolated = zoo::na.approx(value, na.rm = FALSE)) %>%
                                  fill(interpolated, .direction =  "updown") %>%
                                  ungroup()
                                interp = is.na(tmp$value)
                                tmp = tmp %>%
                                  mutate(value = ifelse(is.na(value), interpolated, value))
                                tmp$interpolated = ifelse(interp, "*", "")
                                return(tmp)
                              }
                            )
)




