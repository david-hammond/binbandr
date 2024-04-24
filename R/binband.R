#' Get linearly scaled variable based on optimal bins
#'
#' Finds outliers and then bands between 0 and 1 on optimal bins of
#' non-outlier data
#'
#' @param df string, search term for the World Risk Poll questions
#' @param target Target column
#'
#' @importFrom EnvStats rosnerTest
#' @importFrom dlookr binning extract
#' @importFrom dplyr mutate left_join group_by ungroup filter
#' @importFrom tidyr separate
#' @importFrom scales rescale
#' @importFrom readr parse_number
#' @importFrom rlang .data
#'
#' @return Banded data frame
#' @examples
#' binband(mtcars, target = "mpg")
#' @export
#'

binband = function(df, target = "value"){
  outliers = rosnerTest(df[, target], k = floor(0.1*nrow(df)),
                                  alpha = 0.01)
  outliers = outliers$all.stats %>%
    filter(.data$Outlier)
  df$capped = df[, target]
  df$outliers = FALSE
  df$outliers[outliers$Obs.Num] = TRUE
  df$capped[df$outlier] = ifelse(df[, target][df$outlier] > max(df[, target][!df$outlier]),
                                   max(df[, target][!df$outlier]),
                                   min(df[, target][!df$outlier]))
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
  return(df)
}
