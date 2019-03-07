library(sf)
library(dplyr)
library(ggplot2)
library(here)

# get spatial datasets
precincts <- st_read(dsn = here("data", "sf_precinct_shp"),
                     layer = "SF_DOE_Precincts_2017",
                     stringsAsFactors = FALSE) %>%
  st_transform(7132) %>%
  select(PREC_2017, Shape_Area)
census <- st_read(dsn = here("data", "ca_census_shp"), stringsAsFactors = FALSE) %>%
  filter(COUNTY == "075") %>%
  st_transform(7132) %>% # foot-unit projection designed for SF, so it's the one I'll use
  filter(lengths(st_within(st_centroid(.), precincts)) > 0) %>% # pulls uninhabited Farallon Islands
  select(TRACT, BLKGRP) %>%
  mutate(BLKGRP = as.numeric(BLKGRP))
ggplot() + geom_sf(data = census, color = "blue", fill = NA) +
  geom_sf(data = precincts, color = "red", fill = NA)

census_data <- readr::read_csv(here('data', 'planning_database.csv')) %>%
  select(1:9, 14:16, 29:78, 128:135, 190:195, 202:251) %>% # need more specs on which columns to use
  filter(State == "06", County == "075") %>%
  select(GIDBG, TRACT = Tract, BLKGRP = Block_group, AREA = LAND_AREA, ends_with('CEN_2010'))

census <- left_join(census, census_data, by = c('TRACT', 'BLKGRP'))

# it's okay to drop the labels on these because it's just feeding into the join, so labels disappear
# extensive variables - things that apply to the whole unit, not any given point
census_ext <- select(census, 4:19)
# intensive variables - things that apply to any given point in the region
census_int <- select(census, 20:35)

# maybe redo this with the 'areal' package? combines a bit
ext_join <- st_interpolate_aw(census_ext, to = precincts, extensive = TRUE) %>%
  as.data.frame() %>%
  select(-geometry)
int_join <- st_interpolate_aw(census_int, to = precincts, extensive = FALSE) %>%
  as.data.frame() %>%
  select(-geometry) %>%
  mutate_at(vars(starts_with('pct')), function(x) x / 100)

demo_data <- precincts %>%
  mutate(Group.1 = 1:nrow(.)) %>%
  left_join(ext_join, by = 'Group.1') %>%
  left_join(int_join, by = 'Group.1')
# validation for this join? idk
# other validation - does using counts versus proportions matter?
# that is - if I calculate proportions at the end, does it line up?

# validation - the distribution of heavily Asian regions lines up between the two methods
ggplot(census) + geom_sf(aes(fill = pct_NH_Asian_alone_CEN_2010))
ggplot(demo_data) + geom_sf(aes(fill = pct_NH_Asian_alone_CEN_2010))
