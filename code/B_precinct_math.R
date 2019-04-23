# this script collects and summarizes the raw ballot data,
# then joins it with the precinct demographic info

library(dplyr)
library(rcv)
library(fuzzyjoin)
library(stringr)
library(sf)
library(here)

# load & transform ballot data #####

ballot_link <- "http://www.sfelections.org/results/20180605/data/20180627/20180627_ballotimage.txt"
lookup_link <- "http://www.sfelections.org/results/20180605/data/20180627/20180627_masterlookup.txt"

sf <- clean_ballot(ballot_link, b_header = FALSE,
                   lookup_link, l_header = FALSE,
                   format = 'WinEDS')

save(sf, file = here('data', 'sf_ballot.RData'))

sf_no_vote_old <- sf %>%
  filter(contest == "Mayor") %>%
  group_by(pref_voter_id) %>%
  filter(sum(is.na(candidate)) < 3) %>%
  summarize(precinct = unique(precinct),
            over = sum(as.numeric(over_vote)) > 0,
            under = sum(as.numeric(under_vote)) > 0)

# this is the version that drops duplicates as undervotes, about 6 points of extra voters
sf_no_vote <- sf %>%
  filter(contest == "Mayor") %>%
  group_by(pref_voter_id) %>%
  filter(sum(is.na(candidate)) < 3) %>%
  summarize(precinct = unique(precinct),
            over = sum(as.numeric(over_vote)) > 0,
            under = length(unique(na.omit(candidate))) < 3)


# some basic results

mean(sf_no_vote$over) # 0.3% of mayoral voters overvoted, which is (as expected) small
mean(sf_no_vote$under) # 31% of mayoral voters undervoted, which is pretty large (compare this to other SF elections...?)


by_precinct <-  sf_no_vote %>%
  group_by(precinct) %>%
  summarize(over_count = sum(over),
            no_over_count = n() - over_count,
            under_count = sum(under),
            no_under_count = n() - under_count)

# join in demographic data #####

demo_data <- st_read(dsn = here("data", "demographic_data"),
                     layer = "demo_data",
                     stringsAsFactors = FALSE)
# reset column names because saving the shapefile messes them up
colnames(demo_data) <- c('PREC_2017', 'population', 'female', 'pop_18_24', 'pop_25_44',
                         'pop_45_64', 'pop_65_plus', 'hispanic', 'white', 'black', 'native',
                         'asian', 'pac_islander', 'other_race', 'no_hs', 'college', 'poverty',
                         'no_english', 'geometry')

joined <- by_precinct %>%
  fuzzy_full_join(demo_data, by = c("precinct" = "PREC_2017"), match_fun = str_detect) %>%
  st_as_sf()

# deal with this double/consolidated precinct thing #####

count_cols <- c('over_count', 'no_over_count', 'under_count', 'no_under_count')

# mutate double cases and get precinct turnout
sf_precincts <- joined %>%
  group_by(precinct) %>%
  mutate(weight = ifelse(str_detect(precinct, '/'), population / sum(population), 1)) %>%
  ungroup() %>%
  mutate_at(count_cols, function(x) round(x * .$weight)) %>%
  mutate(overvote_rate = over_count / (over_count + no_over_count),
         undervote_rate = under_count / (under_count + no_under_count),
         turnout = under_count + no_under_count,
         no_turnout = round(population) - turnout,
         turnout_rate = turnout / population) %>%
  na.omit() %>%
  filter(turnout_rate < 1)

st_write(sf_precincts, here('data', 'sf_precincts', 'sf_precincts.shp'), delete_dsn = TRUE)

# and get the double cases for the methods section #####

sf_unadjusted <- filter(joined, str_detect(precinct, '/')) %>%
  as.data.frame() %>%
  select(1,6,2:5) %>%
  mutate(type = 'unweighted',
         weight = 1)

double_cases <- filter(joined, str_detect(precinct, '/')) %>%
  group_by(precinct) %>%
  mutate(weight = population / sum(population)) %>%
  mutate_at(count_cols, round) %>%
  as.data.frame() %>%
  select(1,6,weight, 2:5) %>%
  mutate(type = 'weighted')

doubles_for_table <- bind_rows(sf_unadjusted, double_cases) %>%
  mutate_at(vars(ends_with('_count')), function(x) round(x * .$weight))

save(doubles_for_table, file = here('data', 'double_precincts.RData'))

# okay but now combine the double precincts for dealing with the ballot data in part 2 #####

demo_counts <- st_read(dsn = here("data", "demographic_data"),
                       layer = "demo_counts",
                       stringsAsFactors = FALSE) %>%
  as.data.frame() %>%
  select(-geometry)

colnames(demo_counts) <- c('PREC_2017', 'shape_area', 'group1', 'population', 'female', 
                           'pop_18_24', 'pop_25_44', 'pop_45_64', 'pop_65_plus', 'hispanic', 
                           'white', 'black', 'native', 'asian', 'pac_islander', 'other_race', 
                           'education_denom', 'no_hs', 'college', 'poverty_denom', 'poverty', 
                           'no_english', 'language_denom')

joined <- by_precinct %>%
  fuzzy_full_join(demo_counts, by = c("precinct" = "PREC_2017"), match_fun = str_detect)

combined_pcts <- joined %>%
  select(-ends_with('_count'), -PREC_2017, -shape_area, -group1) %>%
  group_by(precinct) %>%
  mutate_at(vars(-group_cols()), sum) %>%
  distinct() %>%
  ungroup() %>%
  mutate_at(vars(female:other_race), function(x) x / .$population) %>%
  mutate_at(vars(no_hs:college), function(x) x / .$education_denom) %>%
  mutate(poverty = poverty / poverty_denom,
         no_english = no_english / language_denom) %>%
  select(-c(education_denom, poverty_denom, language_denom))

save(combined_pcts, file = here('data', 'combined_precincts.RData'))
