# library(binbandr)
library(tidyverse)
all = rio::import("./data/all-banded.xlsx") %>% mutate(admin_level = "level0")

test = all %>% filter(variablename == "climate change")
nm = distrnorm$new(test$value[test$year == 2015])
x = newIndicator$new(domain = test$variablename,
                 variablename = test$variablename,
                 source = test$source,
                 ismorebetter = 1,
                 geocode = test$geocode,
                 region = test$geoname,
                 year = test$year,
                 value = test$value,
                 weight = 1,
                 normaliser = nm,
                 start_year = 2014,
                 end_year = 2024)
x$data
#
# ni = newIndex$new("David", start_year = 2015, end_year = 2024)
# tmp = split(all, factor(all$variablename))
# for (i in tmp){
#   i = i %>% group_by(geocode, variablename, year, admin_level, source) %>%
#     summarise(value = mean(value)) %>%
#     ungroup()
#   ni$addIndicator(i,
#                   domain = unique(i$variablename),
#                   ismorebetter = 1,
#                   weight = 1,
#                   manual_min_outlier_cutoff = NULL,
#                   manual_max_outlier_cutoff = NULL,
#                   banding_method = "optimal",
#                   peg_year = 2022)
# }
# ni$viewStructure()
# ni$calculateIndex()
# ni$viewERDiagram()
#
#
