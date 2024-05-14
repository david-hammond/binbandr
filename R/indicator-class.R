# Class Defintion ---------------------------------------------------------
# This class is used to represent a "task" in our R program.
#' @importFrom R6 R6Class
newindicator <- R6::R6Class("newindicator",
                    lock_objects = FALSE,
                    public = list(
                      domain = NULL,
                      variablename = NULL,
                      source = NULL,
                      ismorebetter = NULL,
                      optimal_bins = NULL,
                      auto_min_outlier_cutoff = NULL,
                      auto_max_outlier_cutoff = NULL,
                      manual_min_outlier_cutoff = NULL,
                      manual_max_outlier_cutoff = NULL,
                      data = NULL,
                      initialize = function(df, ismorebetter, manual_min_outlier_cutoff = NULL, manual_max_outlier_cutoff = NULL){
                        ## manual calcs
                        tmp = get_capping(df$value)
                        df$capped = tmp$values
                        df = binband(df)
                        self$domain = unique(df$domain)
                        self$variablename <- unique(df$variablename)
                        self$source <- unique(df$source)
                        self$ismorebetter = ismorebetter
                        self$optimal_bins <- dlookr::binning(df$value)
                        self$auto_min_outlier_cutoff <- tmp$caps[1]
                        self$auto_min_outlier_cutoff <- tmp$caps[2]
                        self$manual_min_outlier_cutoff = manual_min_outlier_cutoff
                        self$manual_max_outlier_cutoff = manual_max_outlier_cutoff
                        self$data = df
                      }
                    )
)


get_capping <- function(data, cap_ntiles = c(0.01, 0.99)) {
  hinges <- quantile(data, probs = c(0.25, 0.75), na.rm = TRUE)
  caps <- quantile(data, probs = cap_ntiles, na.rm = TRUE)

  whisker <- 1.5 * diff(hinges)

  data[data < (hinges[1] - whisker)] <- caps[1]
  data[data > (hinges[2] + whisker)] <- caps[2]
  list(values = data, caps = caps)
}
