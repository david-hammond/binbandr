# # library(binbandr)
# library(tidyverse)
# library(distrnorm)
# library(dm)
# all = rio::import("./data/all-banded.xlsx") %>% mutate(admin_level = "level0")
#
# test = all %>% filter(variablename == "climate change")
#
# x = newIndicator$new(domain = test$variablename,
#                  variablename = test$variablename,
#                  source = test$source,
#                  ismorebetter = -1,
#                  geocode = test$geocode,
#                  region = test$geoname,
#                  year = test$year,
#                  value = test$value,
#                  weight = 1,
#                  start_year = 2014,
#                  end_year = 2024,
#                  peg_year = 2016)
# x$normaliser$plot()
# x$split_data()
# test = all %>% filter(variablename == "climate change")
# x = newIndex$new("myindex", 2014, 2024, 2015)
#
# test = test %>% filter(geocode != 'MKD')
#
# x$addIndicator(domain = test$variablename,
#                variablename = test$variablename,
#                source = test$source,
#                ismorebetter = -1,
#                geocode = test$geocode,
#                #region = test$geoname,
#                year = test$year,
#                value = test$value,
#                weight = 1)
# test = test %>% filter(geocode != 'AFG')
# x$addIndicator(domain = test$variablename,
#                variablename = "david",
#                source = test$source,
#                ismorebetter = -1,
#                geocode = test$geocode,
#                #region = test$geoname,
#                year = test$year,
#                value = test$value,
#                weight = 1)
# x$calculate_index()
# y = x$data
# y = y %>% complete(geocode, year, vid) %>%
#   group_by(geocode) %>%
#   fill(region, .direction = 'updown') %>%
#   ungroup() %>%
#   left_join(x$regional) %>%
#   mutate(regional_average = ifelse(is.na(value), "*", "")) %>%
#   mutate(value = ifelse(is.na(value), regional_mean_raw, value),
#          banded = ifelse(is.na(banded), regional_mean_banded, banded)) %>%
#   select(-regional_mean_raw, -regional_mean_banded) %>%
#   mutate(interpolated = replace_na(interpolated, "")) %>%
#   left_join(x$meta)

