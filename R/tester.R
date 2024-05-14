library(tidyverse)
library(dlookr)
library(scales)
all = rio::import("C:/Users/DavidHammond/OneDrive - Institute for Economics and Peace/github/normandy-peace-index/data/processed/all-banded.xlsx") %>%
  mutate(domain = variablename, admin_level = 0, parent_geo = geocode)

ni = newindex$new("David", start_year = 2015, end_year = 2024)
tmp = split(all, factor(all$variablename))
for (i in tmp){
  ni$addIndicator(i,
                  domain = unique(i$variablename),
                  ismorebetter = 1,
                  weight = 1,
                  manual_min_outlier_cutoff = NULL,
                  manual_max_outlier_cutoff = NULL,
                  banding_method = "optimal")
}
df = ni$dm

ni$export_to_db("test.sqlite")


tmp = all %>% filter(domain == "crime")
ni$addIndicator(tmp)
ni$structure
ni$dm %>% dm_draw()
con <- DBI::dbConnect(RSQLite::SQLite(), "test.sqlite")
copy_dm_to(con, ni$dm, set_key_constraints = TRUE, temporary = F)

fill_missing_values <- function(df) {
  df = df %>%
    group_by(vuid, guid) %>%
    complete(year = full_seq(year, 1)) %>%
    mutate(interpolated = zoo::na.approx(banded, na.rm = FALSE)) %>%
    ungroup() %>%
    mutate(imputed = ifelse(is.na(banded), 1, 0),
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
}

z = fill_missing_values(data)
missing_data = z %>% group_by(guid) %>%
  summarise(pc_missing = sum(is.na(banded))/n()) %>%
  ungroup() %>%
  mutate(include = ifelse(pc_missing < 0.3, TRUE, FALSE))
z = z %>% filter(guid %in% missing_data$guid[missing_data$include])
y = z %>% group_by(vuid, guid, year) %>% summarise(banded = mean(banded)) %>%
  ungroup() %>% spread(vuid, banded)
raw_pivot = y %>% select( -guid, -year)
x = mice(raw_pivot)
names(raw_pivot) = make.names(names(raw_pivot))
set.seed(2022)
n <- nrow(raw_pivot)
idx <- sample(1:n, size = round(0.7 * n), replace = FALSE)
train.data <- raw_pivot[idx, ]
test.data <- raw_pivot[-idx, ]
params <- list(max_depth = 100, subsample = 0.7, nthread = 2)
cv.results <- mixgb_cv(data = train.data, nrounds = 100,
                       xgb.params = params, verbose = FALSE)
mixgb.obj <- mixgb(data = train.data, m = 1, xgb.params = params, nrounds = 10, save.models = TRUE)

# obtain m imputed datasets for train.data
train.imputed <- mixgb.obj$imputed.data

test.imputed <- impute_new(object = mixgb.obj, newdata = test.data)
##----------------------------------------------------------------
##                            Do mixgb                           -
##----------------------------------------------------------------


mixgb.obj <- mixgb(data = train.data, m = 1, xgb.params = params, nrounds = 10, save.models = TRUE)
library(mixgb)
