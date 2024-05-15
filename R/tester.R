# library(binbandr)
# library(tidyverse)
# all = rio::import("./data/all-banded.xlsx") %>% mutate(admin_level = "level0")
#
# ni = newIndex$new("David", start_year = 2015, end_year = 2024)
# tmp = split(all, factor(all$variablename))
# for (i in tmp){
#   ni$addIndicator(i,
#                   domain = unique(i$variablename),
#                   ismorebetter = 1,
#                   weight = 1,
#                   manual_min_outlier_cutoff = NULL,
#                   manual_max_outlier_cutoff = NULL,
#                   banding_method = "optimal")
# }
# ni$viewStructure()
# ni$calculateIndex()
# ni$viewERDiagram()
#
#
# birds <- Node$new("Aves", vulgo = "Bird")
# birds$AddChild("Neognathae", vulgo = "New Jaws", species = 10000)
# birds$AddChild("Palaeognathae", vulgo = "Old Jaws", species = 60)
# print(birds, "vulgo", "species")
#
# birds$species <- function(self) sum(sapply(self$children, function(x) x$species))
# print(birds, "species")
