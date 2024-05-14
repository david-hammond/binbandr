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


# Class Defintion ---------------------------------------------------------
# This class is used to represent a "task" in our R program.
#' @importFrom R6 R6Class
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
                              initialize = function(df, manual_min_outlier_cutoff = NULL, manual_max_outlier_cutoff = NULL){
                                ## manual calcs
                                tmp = get_capping(df$value)
                                df$capped = tmp$values
                                df = binband(df)
                                self$domain = unique(df$domain)
                                self$ismorebetter = as.logical(unique(df$ismorebetter))
                                self$variablename <- unique(df$variablename)
                                self$admin_level <- unique(df$admin_level)
                                self$source <- unique(df$source)
                                self$weight <- unique(df$weight)
                                self$optimal_bins <- dlookr::binning(df$value)
                                self$auto_min_outlier_cutoff <- tmp$caps[1]
                                self$auto_max_outlier_cutoff <- tmp$caps[2]
                                self$manual_min_outlier_cutoff = manual_min_outlier_cutoff #need to implement
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


newindex <- R6::R6Class("newindex",
                            lock_objects = FALSE,
                            public = list(
                              name = NULL,
                              structure = NULL,
                              indicators = list(),
                              corpus = NULL,
                              dm = NULL,
                              initialize = function(name){
                                self$name = name
                              },
                              addIndicator = function(df){
                                self$indicators = c(self$indicators, newindicator$new(df))
                                self$corpus = self$corpus %>% rbind(last(self$indicators)$data)
                                self$dm = dm_decompose(self$corpus)
                                tmp = self$dm$meta
                                tmp$pathString = paste(self$name,
                                                       tmp$domain,
                                                       tmp$variablename,
                                                       sep = "/")
                                self$structure = as.Node(tmp)
                              },
                              export_to_db = function(dbname){
                                con <- DBI::dbConnect(RSQLite::SQLite(), dbname)
                                copy_dm_to(con, self$dm, set_key_constraints = TRUE, temporary = F)
                                dbDisconnect(con)
                              }
                            )

)

dm_decompose = function(df){
  x = decompose_table(df, vuid, domain, variablename, source, ismorebetter, admin_level, weight)
  meta = x$parent_table
  meta$vcode = toupper(paste(meta$domain, meta$variablename))
  meta$vcode = make.unique(abbreviate(abbreviate(toupper(meta$vcode), 1),1) )
  x = decompose_table(x$child_table, guid, parent_geo, geocode, geoname)
  geo = x$parent_table
  x = x$child_table
  x = x %>% decompose_table(calcuid, vuid, guid, year, value, banded)
  data = x$parent_table
  calculation_data = x$child_table
  world_average = data %>% group_by(vuid, year) %>%
    summarise(mean_value = mean(value), mean_banded = mean(banded))
  x = dm(meta, geo, data, calculation_data, world_average) %>%
    dm_add_pk(meta, vuid) %>%
    dm_add_pk(geo, guid) %>%
    dm_add_pk(data, calcuid) %>%
    dm_add_fk(data, vuid, meta) %>%
    dm_add_fk(data, guid, geo) %>%
    dm_add_fk(world_average, vuid, meta) %>%
    dm_add_fk(calculation_data, calcuid, data)
  overall_scores = calculate_index(x)
  x = dm(meta, geo, data, calculation_data, world_average, overall_scores) %>%
    dm_add_pk(meta, vuid) %>%
    dm_add_pk(geo, guid) %>%
    dm_add_pk(data, calcuid) %>%
    dm_add_fk(data, vuid, meta) %>%
    dm_add_fk(data, guid, geo) %>%
    dm_add_fk(world_average, vuid, meta) %>%
    dm_add_fk(calculation_data, calcuid, data) %>%
    dm_add_fk(overall_scores, guid, geo)
  return(x)

}

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
