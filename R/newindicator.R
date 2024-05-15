
# Class Defintion ---------------------------------------------------------
# This class is used to represent a "task" in our R program.

newindicator <- R6::R6Class("newindicator",
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
                                  caps[1] = self$manual_min_outlier_cutoff
                                }
                                if(!is.null(self$manual_max_outlier_cutoff)){
                                  caps[2] = self$manual_max_outlier_cutoff
                                }

                                data[data < (hinges[1] - whisker)] <- caps[1] #this inst right
                                data[data > (hinges[2] + whisker)] <- caps[2]
                                list(values = data, caps = caps)
                              }
                            )
)





