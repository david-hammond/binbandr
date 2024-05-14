library(tidyverse)
library(dlookr)
library(scales)
all = rio::import("C:/Users/DavidHammond/OneDrive - Institute for Economics and Peace/github/normandy-peace-index/data/processed/all-banded.xlsx") %>%
  mutate(domain = variablename, ismorebetter = 1, admin_level = 0, parent_geo = geocode, weight = 1)
tmp = all %>% filter(domain == "climate change")
dlookr::binning(tmp$value)
tmp$value[nrow(tmp)] = 100000000
tmp$value[1] = -1000000

x = newindicator$new(tmp)

ni = newindex$new("David")
tmp = split(all, factor(all$variablename))
for (i in tmp){
  ni$addIndicator(i)
}
ni$export_to_db("test.sqlite")


tmp = all %>% filter(domain == "crime")
ni$addIndicator(tmp)
ni$structure
ni$dm %>% dm_draw()
con <- DBI::dbConnect(RSQLite::SQLite(), "test.sqlite")
copy_dm_to(con, ni$dm, set_key_constraints = TRUE, temporary = F)
