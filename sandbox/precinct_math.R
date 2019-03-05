library(dplyr)
library(rcv)
library(ggplot2)

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
