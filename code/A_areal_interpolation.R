# this script takes in raw shapefiles and census data,
# makes a few intermediate plots for explanation in the methods section,
# and performs the areal interpolation to get demographic info about election precincts

library(sf)
library(dplyr)
library(ggplot2)
library(patchwork)
library(here)

# get spatial datasets #####
precincts_shape <- st_read(dsn = here("raw_data", "sf_precinct_shp"),
                     layer = "SF_DOE_Precincts_2017",
                     stringsAsFactors = FALSE) %>%
  st_transform(7132) %>%
  select(PREC_2017, Shape_Area)
census_shape <- st_read(dsn = here("raw_data", "ca_census_shp"), stringsAsFactors = FALSE) %>%
  filter(COUNTY == "075") %>%
  st_transform(7132) %>% # foot-unit projection designed for SF, so it's the one I'll use
  filter(lengths(st_within(st_centroid(.), precincts_shape)) > 0) %>% # pulls uninhabited Farallon Islands
  select(TRACT, BLKGRP) %>%
  mutate(BLKGRP = as.numeric(BLKGRP))

# get boundary intersection #####

precincts_bound <- st_union(precincts_shape)
census_bound <- st_union(census_shape)

overall <- st_intersection(precincts_bound, census_bound) # great!

precincts <- st_intersection(precincts_shape, overall)
census_regions <- st_intersection(census_shape, overall)

# plots

joint_out <- ggplot(overall) + geom_sf() + theme_void() + labs(title = 'Overall') +
  theme(plot.title = element_text(hjust = 0.5))
x.range <- joint_out$plot_env$x.range
y.range <- joint_out$plot_env$y.range

prec_out <- ggplot(precincts_bound) + geom_sf() + theme_void() + labs(title = 'Precincts') + 
  coord_sf(xlim = x.range, ylim = y.range) +
  theme(plot.title = element_text(hjust = 0.5))

cen_out <- ggplot(census_bound) + geom_sf() + theme_void() + labs(title = 'Census') + 
  coord_sf(xlim = x.range, ylim = y.range) +
  theme(plot.title = element_text(hjust = 0.5))

all <- prec_out + cen_out - joint_out + plot_layout(ncol = 1, heights = c(4,6))

# the plot saves take some time, but it's better than running the entire script every knit
ggsave(here('img', 'boundary_intersection.png'), all)

# and now the full internal boundaries, clipped

precincts_post <- ggplot(precincts) + geom_sf() + theme_void() +
  labs(title = 'Precincts - bounded') +
  theme(plot.title = element_text(hjust = 0))

census_post <- ggplot(census_regions) + geom_sf() + theme_void() +
  labs(title = 'Census - bounded') +
  theme(plot.title = element_text(hjust = 1))
x.range <- census_post$plot_env$x.range
y.range <- census_post$plot_env$y.range

precincts_pre <- ggplot(precincts_shape) + geom_sf() + theme_void() +
  labs(title = 'Precincts - original') + 
  coord_sf(xlim = x.range, ylim = y.range) +
  theme(plot.title = element_text(hjust = 0))

census_pre <- ggplot(census_shape) + geom_sf() + theme_void() +
  labs(title = 'Census - original') + 
  coord_sf(xlim = x.range, ylim = y.range) +
  theme(plot.title = element_text(hjust = 1))

clipped_internals <- (precincts_pre + census_pre) - (precincts_post + census_post) + plot_layout(ncol = 1)

ggsave(here('img', 'clipped_internals.png'), clipped_internals)

# clean census data #####

# okay we're gonna use the ACS because it has education info
census_data <- readr::read_csv(here('raw_data', 'planning_database.csv')) %>%
  select(GIDBG, State, State_name, County, County_name,
         TRACT = Tract, BLKGRP = Block_group,
         population = Tot_Population_ACS_12_16,
         female = Females_ACS_12_16,
         pop_18_24 = Pop_18_24_ACS_12_16,
         pop_25_44 = Pop_25_44_ACS_12_16,
         pop_45_64 = Pop_45_64_ACS_12_16,
         pop_65_plus = Pop_65plus_ACS_12_16,
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
  mutate_at(vars(female:other_race), function(x) x / .$population) %>%
  mutate_at(vars(no_hs:college), function(x) x / .$education_denom) %>%
  mutate(poverty = poverty / poverty_denom,
         no_english = no_english / language_denom) %>%
  select(-population, -education_denom, -poverty_denom, -language_denom)

# join precincts and census #####

join <- st_interpolate_aw(census, to = precincts, extensive = TRUE) %>%
  as.data.frame() %>%
  select(-geometry)
save(join, file = here('data', 'intermediate_join.RData'))

demo_data <- precincts %>%
  mutate(Group.1 = 1:nrow(.)) %>%
  left_join(join, by = 'Group.1') %>%
  mutate_at(vars(female:other_race), function(x) x / .$population) %>%
  mutate_at(vars(no_hs:college), function(x) x / .$education_denom) %>%
  mutate(poverty = poverty / poverty_denom,
         no_english = no_english / language_denom) %>%
  select(-c(education_denom, poverty_denom, language_denom, Group.1, Shape_Area))

# save demo_data to use in the precinct_math.R script

st_write(demo_data, here('data', 'demographic_data', 'demo_data.shp'), delete_dsn = TRUE)

# make intensive case to show off that plot

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
save(demo_data_int, file = here('data', 'demo_data_int.RData'))

sf_wrong <- left_join(demo_data, demo_data_int, by = c('PREC_2017' = 'PREC_2017_wrong'))

# interpolation verification plots #####

# checking interpolation

age_pre <- ggplot(census_props) + geom_sf(aes(fill = pop_25_44), lwd = 0) + theme_void() + ggtitle('Pre-interpolation (block groups)')
age_post <- ggplot(demo_data) + geom_sf(aes(fill = pop_25_44), lwd = 0) + theme_void() + ggtitle('Post-interpolation (precincts)')

age <- age_pre + age_post + plot_layout(ncol = 2)

ggsave(here('img', 'interpolation_check.png'), age)

# error in intensive methods

scatter <- ggplot(sf_wrong, aes(x = pop_25_44,
                                y = pop_25_44_wrong,
                                col = abs(pop_25_44 - pop_25_44_wrong),
                                size = population)) +
  geom_point(shape = 1) +
  labs(x = "Calculated Percentage",
       y = "Reported Percentage",
       col = "Absolute Error") +
  scale_colour_gradient(high = "#132B43", low = "#56B1F7") +
  theme_bw()
# maybe pick a new variable that really exemplifies the issue

hist <- ggplot(sf_wrong, aes(x = pop_25_44 - pop_25_44_wrong)) + geom_histogram() +
  theme_bw() +
  labs(x = 'Error in calculated percentage',
       y = 'Count')

intensive_error <- scatter + hist + plot_layout(ncol = 2)

ggsave(here('img', 'intensive_error.png'), intensive_error)

# okay now combining the double precincts for the back section #####

demo_counts <- precincts %>%
  mutate(Group.1 = 1:nrow(.)) %>%
  left_join(join, by = 'Group.1')

st_write(demo_counts, here('data', 'demographic_data', 'demo_counts.shp'), delete_dsn = TRUE)
