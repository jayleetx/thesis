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
census_regions <- st_intersection(census_shape, overall)

# okay we're gonna use the ACS because it has education info
census_data <- readr::read_csv(here('data', 'planning_database.csv')) %>%
  select(GIDBG, State, State_name, County, County_name,
         TRACT = Tract, BLKGRP = Block_group,
         population = Tot_Population_ACS_12_16,
         women = Females_ACS_12_16,
         pop_18_24 = Pop_18_24_ACS_12_16,
         pop_25_44 = Pop_25_44_ACS_12_16,
         pop_45_64 = Pop_45_64_ACS_12_16,
         pop_65_up = Pop_65plus_ACS_12_16,
         hispanic = Hispanic_ACS_12_16,
         white = NH_White_alone_ACS_12_16,
         black = NH_Blk_alone_ACS_12_16,
         native = NH_AIAN_alone_ACS_12_16,
         asian = NH_Asian_alone_ACS_12_16,
         pac_islander = NH_NHOPI_alone_ACS_12_16,
         other_race = NH_SOR_alone_ACS_12_16,
         education_denom = Pop_25yrs_Over_ACS_12_16,
         no_hs = Not_HS_Grad_ACS_12_16,
         college = College_ACS_12_16,
         poverty_denom = Pov_Univ_ACS_12_16,
         poverty = Prs_Blw_Pov_Lev_ACS_12_16,
         no_english = ENG_VW_ACS_12_16,
         language_denom = Tot_Occp_Units_ACS_12_16) %>%
  filter(State == "06", County == "075")

census <- left_join(census_regions, census_data, by = c('TRACT', 'BLKGRP')) %>%
  select(-TRACT, -BLKGRP, -GIDBG, -State, -State_name, -County, -County_name)

census_props <- census %>%
  mutate_at(vars(women:other_race), function(x) x / .$population) %>%
  mutate_at(vars(no_hs:college), function(x) x / .$education_denom) %>%
  mutate(poverty = poverty / poverty_denom,
         no_english = no_english / language_denom) %>%
  select(-population, -education_denom, -poverty_denom, -language_denom)

join <- st_interpolate_aw(census, to = precincts, extensive = TRUE) %>%
  as.data.frame() %>%
  select(-geometry)

demo_data <- precincts %>%
  mutate(Group.1 = 1:nrow(.)) %>%
  left_join(join, by = 'Group.1') %>%
  mutate_at(vars(women:other_race), function(x) x / .$population) %>%
  mutate_at(vars(no_hs:college), function(x) x / .$education_denom) %>%
  mutate(poverty = poverty / poverty_denom,
         no_english = no_english / language_denom) %>%
  select(-c(education_denom, poverty_denom, language_denom, Group.1, Shape_Area))

# test intensive case to show off that plot

join_int <- st_interpolate_aw(census_props, to = precincts, extensive = FALSE) %>%
  as.data.frame() %>%
  select(-geometry)

demo_data_int <- precincts %>%
  mutate(Group.1 = 1:nrow(.)) %>%
  left_join(join_int, by = 'Group.1') %>%
  select(-Group.1, -Shape_Area) %>%
  as.data.frame() %>%
  select(-geometry)
colnames(demo_data_int) <- paste0(colnames(demo_data_int), "_wrong")
