library(sf)
library(dplyr)
library(ggplot2)
library(here)

# get spatial datasets
precincts_shape <- st_read(dsn = here("data", "sf_precinct_shp"),
                     layer = "SF_DOE_Precincts_2017",
                     stringsAsFactors = FALSE) %>%
  st_transform(7132) %>%
  select(PREC_2017, Shape_Area)
census_shape <- st_read(dsn = here("data", "ca_census_shp"), stringsAsFactors = FALSE) %>%
  filter(COUNTY == "075") %>%
  st_transform(7132) %>% # foot-unit projection designed for SF, so it's the one I'll use
  filter(lengths(st_within(st_centroid(.), precincts_shape)) > 0) %>% # pulls uninhabited Farallon Islands
  select(TRACT, BLKGRP) %>%
  mutate(BLKGRP = as.numeric(BLKGRP))
#ggplot() + geom_sf(data = census, color = "blue", fill = NA) +
#  geom_sf(data = precincts, color = "red", fill = NA)

precincts_bound <- st_union(precincts_shape)
census_bound <- st_union(census_shape)

overall <- st_intersection(precincts_bound, census_bound) # great!

precincts <- st_intersection(precincts_shape, overall)
census <- st_intersection(census_shape, overall)


census_data <- readr::read_csv(here('data', 'planning_database.csv')) %>%
  select(1:9, 14:16, 29:78, 128:135, 190:195, 202:251) %>% # need more specs on which columns to use
  filter(State == "06", County == "075") %>%
  select(GIDBG, TRACT = Tract, BLKGRP = Block_group, AREA = LAND_AREA,
         ends_with('CEN_2010'), -starts_with('pct'))

census <- left_join(census, census_data, by = c('TRACT', 'BLKGRP')) %>%
  select(-TRACT, -BLKGRP, -GIDBG, -AREA)

join <- st_interpolate_aw(census, to = precincts, extensive = TRUE) %>%
  as.data.frame() %>%
  select(-geometry)

demo_data <- precincts %>%
  mutate(Group.1 = 1:nrow(.)) %>%
  left_join(join, by = 'Group.1') %>%
  mutate_at(vars(ends_with('CEN_2010'), -Tot_Population_CEN_2010), function(x) x / .$Tot_Population_CEN_2010)
# validation for this join? idk

# validation - the distribution of heavily Asian regions lines up between the two methods
#ggplot(census) + geom_sf(aes(fill = pct_NH_Asian_alone_CEN_2010))
#ggplot(demo_data) + geom_sf(aes(fill = pct_NH_Asian_alone_CEN_2010))
