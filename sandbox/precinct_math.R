library(dplyr)
library(rcv)
library(ggplot2)
library(fuzzyjoin)
library(stringr)
library(here)

source(here('sandbox', 'areal_interpolation.R'))

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
  summarize(over_count = sum(over),
            no_over_count = n() - over_count,
            under_count = sum(under),
            no_under_count = n() - under_count)

# alright there's 9 messed up cases after this so we gotta deal with that at some point
sf_precincts <- by_precinct %>%
  fuzzy_full_join(demo_data, by = c("precinct" = "PREC_2017"), match_fun = str_detect)

sf_unadjusted <- sf_precincts

# okay now this is a weird thing
# because some of the precincts get consolidated
# so we're gonna assume that proportions are equivalent on each half and the turnout count gets split
# ACTUALLY that was a bad assumption because some are super unbalanced
# we're gonna assume the pop split is the same as in 2010 and weight that way
double_cases <- filter(sf_precincts, str_detect(precinct, '/')) %>%
  group_by(precinct) %>%
  mutate(weight = Tot_Population_CEN_2010 / sum(Tot_Population_CEN_2010))
count_cols <- c('over_count', 'no_over_count', 'under_count', 'no_under_count')
sf_precincts[str_detect(sf_precincts$precinct, '/') %in% TRUE, count_cols] <- round(sf_precincts[str_detect(sf_precincts$precinct, '/') %in% TRUE, count_cols] * double_cases$weight)
# this gives much nicer results

# and get precinct turnout
sf_precincts <- mutate(sf_precincts,
                       turnout = under_count + no_under_count,
                       turnout_rate = turnout / Tot_Population_CEN_2010)

# every precinct should see inflated turnout due to population growth since 2010
# also error due to the joining process and the doubles, whatevs
# some will even be above 1
ggplot(filter(sf_precincts, turnout_rate <= 1)) +
  geom_sf(aes(fill = turnout_rate)) +
  scale_fill_gradient(low = "white", high = "blue")

# intermittent error with this plot - known bug
# https://github.com/tidyverse/ggplot2/issues/2252
# should be fixed as of updating RStudio
ggplot(sf_precincts) +
  geom_sf(aes(fill = log(over_pct))) +
  scale_fill_gradient(low = "white") +
  theme_void()
# what's up with that one precinct with like 4% overvotes?

ggplot(sf_precincts) +
  geom_sf(aes(fill = under_pct)) +
  scale_fill_gradient(low = 'white') +
  theme_void()

# regressions?
# dig out the stat learning book and do some better model selection

naive_over <- lm((over_count / (over_count + no_over_count)) ~
#                   + pct_Pop_18_24_CEN_2010
#                   + pct_Pop_25_44_CEN_2010
#                   + pct_Pop_45_64_CEN_2010
#                   + pct_Pop_65plus_CEN_2010
                   + pct_Tot_GQ_CEN_2010 # people in group quarters
#                   + pct_Inst_GQ_CEN_2010 # people in institutionalized group quarters - prison, hospital, etc. (probably not voting)
                   + pct_Non_Inst_GQ_CEN_2010 # people in noninstitutional GQs - military, dorm, etc. (potential voters)
                   + pct_Hispanic_CEN_2010
                   + pct_NH_White_alone_CEN_2010
                   + pct_NH_Blk_alone_CEN_2010
#                   + pct_NH_AIAN_alone_CEN_2010
#                   + pct_NH_Asian_alone_CEN_2010
#                   + pct_NH_NHOPI_alone_CEN_2010
#                   + pct_NH_SOR_alone_CEN_2010
,
                 data = sf_precincts)
summary(naive_over)
# more overvoting: pop in GQs, Hispanic, Black
# less overvoting: non-institutional GQs, White
# GQs, NIGQs, and Black are the most important

naive_under <- lm((under_count / (under_count + no_under_count)) ~
                   pct_Pop_18_24_CEN_2010
                   + pct_Pop_25_44_CEN_2010
                   + pct_Pop_45_64_CEN_2010
#                   + pct_Pop_65plus_CEN_2010
                   + pct_Tot_GQ_CEN_2010
#                   + pct_Inst_GQ_CEN_2010
                   + pct_Non_Inst_GQ_CEN_2010
                   + pct_Hispanic_CEN_2010
                   + pct_NH_White_alone_CEN_2010
                   + pct_NH_Blk_alone_CEN_2010
#                   + pct_NH_AIAN_alone_CEN_2010
                   + pct_NH_Asian_alone_CEN_2010
                   + pct_NH_NHOPI_alone_CEN_2010
#                   + pct_NH_SOR_alone_CEN_2010
                 ,
                 data = sf_precincts)
summary(naive_under)
# more undervoting: Hispanic, White, Black, Asian, Pacific Islander, NIGQs
# less undervoting: all except the oldest age group
# the ethnic groups are the most important, followed by age low to high with GQ thrown in the bottom mix


# logistic regressions

logit_over <- glm(cbind(over_count, no_over_count) ~
#                    pct_Pop_18_24_CEN_2010
                  + pct_Pop_25_44_CEN_2010
#                  + pct_Pop_45_64_CEN_2010
                  + pct_Pop_65plus_CEN_2010
#                  + pct_Tot_GQ_CEN_2010
#                  + pct_Inst_GQ_CEN_2010
#                  + pct_Non_Inst_GQ_CEN_2010
#                  + pct_Hispanic_CEN_2010
                  + pct_NH_White_alone_CEN_2010
#                  + pct_NH_Blk_alone_CEN_2010
#                  + pct_NH_AIAN_alone_CEN_2010
                  + pct_NH_Asian_alone_CEN_2010
#                  + pct_NH_NHOPI_alone_CEN_2010
#                  + pct_NH_SOR_alone_CEN_2010
                  ,
                  data = sf_precincts, family = binomial)
# oldest age group, 25-44yo (less significant) increase chances of overvoting
# Whites, Asians decrease chances of overvoting


logit_under <- glm(cbind(under_count, no_under_count) ~
                  pct_Pop_18_24_CEN_2010
                  + pct_Pop_25_44_CEN_2010
                  + pct_Pop_45_64_CEN_2010
#                  + pct_Pop_65plus_CEN_2010
                  + pct_Tot_GQ_CEN_2010
#                  + pct_Inst_GQ_CEN_2010
                  + pct_Non_Inst_GQ_CEN_2010
                  + pct_Hispanic_CEN_2010
                  + pct_NH_White_alone_CEN_2010
                  + pct_NH_Blk_alone_CEN_2010
                  + pct_NH_AIAN_alone_CEN_2010
#                  + pct_NH_Asian_alone_CEN_2010
#                  + pct_NH_NHOPI_alone_CEN_2010
#                  + pct_NH_SOR_alone_CEN_2010
                  ,
                  data = sf_precincts, family = binomial)
# all age groups except for the oldest less likely to undervote
# people in GQs less likely to undervote, institutional GQs more likely to undervote
# Black and White more likely to undervote, Native and Hispanic less likely to undervote
