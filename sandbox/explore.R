# this starts to analyze data from the June 2018 SF mayoral elex

library(rcv)
library(dplyr)

# load & transform data #####

ballot_link <- "http://www.sfelections.org/results/20180605/data/20180627/20180627_ballotimage.txt"
lookup_link <- "http://www.sfelections.org/results/20180605/data/20180627/20180627_masterlookup.txt"

sf <- clean_ballot(ballot_link, b_header = FALSE,
                   lookup_link, l_header = FALSE,
                   format = 'WinEDS')

sf_wide <- readable(sf)

sf_no_vote <- sf %>%
  filter(contest == "Mayor") %>%
  group_by(pref_voter_id) %>%
  summarize(precinct = unique(precinct),
            over = sum(as.numeric(over_vote)) > 0,
            under = sum(as.numeric(under_vote)) > 0)

# some basic results

mean(sf_no_vote$over) # 0.4% of mayoral voters overvoted, which is (as expected) small
mean(sf_no_vote$under) # 25% of mayoral voters undervoted, which is pretty large (compare this to other SF elections...?)

by_precinct <-  sf_no_vote %>%
  group_by(precinct) %>%
  summarize(over_pct = mean(over),
            under_pct = mean(under),
            turnout = n())
  

# spatial things below the break #####

library(sf)
library(ggplot2)
library(fuzzyjoin)
library(stringr)

# data from https://sfelections.sfgov.org/sites/default/files/Documents/Maps/2017lines.zip
precinct_shp <- st_read(dsn = "data/sf_precinct_shp", layer = "SF_DOE_Precincts_2017", stringsAsFactors = FALSE)

sf_precincts <- by_precinct %>%
  fuzzy_full_join(precinct_shp, by = c("precinct" = "PREC_2017"), match_fun = str_detect) %>%
  mutate(SupDist = factor(as.numeric(SupDist))) %>%
  select(1:5, 10, 14:16)

# intermittent error with this plot - known bug
# https://github.com/tidyverse/ggplot2/issues/2252
ggplot(sf_precincts) +
  geom_sf(aes(fill = log(over_pct))) +
  scale_fill_gradient(low = "white") +
  theme_void()
# what's up with that one precinct with like 4% overvotes?

ggplot(sf_precincts) +
  geom_sf(aes(fill = under_pct)) +
  scale_fill_gradient(low = 'white') +
  theme_void()


# start using the census ####

census <- readr::read_csv('data/planning_database.csv') %>%
  select(1:9, 14:16, 29:78, 128:135, 190:195, 202:251) %>% # need more specs on which columns to use
  filter(State == "06", County == "075")

# map from https://www.census.gov/geo/maps-data/data/cbf/cbf_blkgrp.html
cali_block_groups <- st_read(dsn = "data/ca_census_shp", stringsAsFactors = FALSE) %>%
  filter(COUNTY == "075") %>%
  mutate(BLKGRP = as.numeric(BLKGRP)) %>%
  full_join(census, by = c("TRACT" = "Tract", "BLKGRP" = "Block_group")) %>%
  mutate(GEO_ID = str_extract(GEO_ID, "\\d+$"),
         check = GEO_ID == GIDBG)

match <- select(cali_block_groups,
               GEO_ID, GIDBG, TRACT, BLKGRP,
               GIDBG, check)
# one thing in the census is missing from the shape file,
#which is fine because it's a flag (nobody lives there, zero area)

ggplot(match) +
  geom_sf() +
  theme_void()
# FUCK the Farallon Islands
