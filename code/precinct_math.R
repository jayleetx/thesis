library(dplyr)
library(rcv)
library(ggplot2)
library(fuzzyjoin)
library(stringr)
library(here)
library(leaps)

source(here('code', 'areal_interpolation.R'))

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
                       overvote_rate = over_count / (over_count + no_over_count),
                       undervote_rate = under_count / (under_count + no_under_count),
                       turnout = under_count + no_under_count,
                       turnout_rate = turnout / Tot_Population_CEN_2010) %>%
  na.omit()


# regressions?
# dig out the stat learning book and do some better model selection

training <- sample_frac(sf_precincts, 0.5)
test <- sf_precincts[!(sf_precincts$PREC_2017 %in% training$PREC_2017), ]

# linear model for overvoting #####

over_formula <- overvote_rate ~ Pop_18_24_CEN_2010 + Pop_25_44_CEN_2010 + Pop_45_64_CEN_2010 +
  Pop_65plus_CEN_2010 + Hispanic_CEN_2010 + NH_White_alone_CEN_2010 + NH_Blk_alone_CEN_2010 +
  NH_AIAN_alone_CEN_2010 + NH_Asian_alone_CEN_2010 + NH_NHOPI_alone_CEN_2010 +
  NH_SOR_alone_CEN_2010

linear_over_backward <- regsubsets(over_formula, data = training, method = 'backward', nvmax = 11)
linear_over_forward <- regsubsets(over_formula, data = training, method = 'forward', nvmax = 11)
# backward selection
test.mat <- model.matrix(over_formula, data = test)
errors <- rep(NA, 11)
for (i in 1:11) {
  coefi <- coef(linear_over_backward, id = i)
  pred <- test.mat[ ,names(coefi)]%*%coefi
  errors[i] <- mean((test$overvote_rate - pred)^2)
}
coef(linear_over_backward, which.min(errors))
# forward selection
test.mat <- model.matrix(over_formula, data = test)
errors <- rep(NA, 11)
for (i in 1:11) {
  coefi <- coef(linear_over_forward, id = i)
  pred <- test.mat[ ,names(coefi)]%*%coefi
  errors[i] <- mean((test$overvote_rate - pred)^2)
}
coef(linear_over_forward, which.min(errors))
# they agree here!
best_linear_over <- lm(overvote_rate ~ Pop_65plus_CEN_2010 +
                         Hispanic_CEN_2010 +
                         NH_Blk_alone_CEN_2010,
                       data = sf_precincts)

# linear model for undervoting #####
under_formula <- undervote_rate ~ Pop_18_24_CEN_2010 + Pop_25_44_CEN_2010 + Pop_45_64_CEN_2010 +
  Pop_65plus_CEN_2010 + Hispanic_CEN_2010 + NH_White_alone_CEN_2010 + NH_Blk_alone_CEN_2010 +
  NH_AIAN_alone_CEN_2010 + NH_Asian_alone_CEN_2010 + NH_NHOPI_alone_CEN_2010 +
  NH_SOR_alone_CEN_2010

linear_under_backward <- regsubsets(under_formula, data = training, method = 'backward', nvmax = 11)
linear_under_forward <- regsubsets(under_formula, data = training, method = 'forward', nvmax = 11)
# backward selection
test.mat <- model.matrix(under_formula, data = test)
errors <- rep(NA, 11)
for (i in 1:11) {
  coefi <- coef(linear_under_backward, id = i)
  pred <- test.mat[ ,names(coefi)]%*%coefi
  errors[i] <- mean((test$undervote_rate - pred)^2)
}
coef(linear_under_backward, which.min(errors))
# forward selection
test.mat <- model.matrix(under_formula, data = test)
errors <- rep(NA, 11)
for (i in 1:11) {
  coefi <- coef(linear_under_forward, id = i)
  pred <- test.mat[ ,names(coefi)]%*%coefi
  errors[i] <- mean((test$undervote_rate - pred)^2)
}
coef(linear_under_forward, which.min(errors))
# they agree here!
best_linear_under <- lm(overvote_rate ~ Pop_18_24_CEN_2010 +
                          Pop_25_44_CEN_2010 +
                          Pop_45_64_CEN_2010 +
                          Pop_65plus_CEN_2010 +
                          Hispanic_CEN_2010 +
                          NH_White_alone_CEN_2010 +
                          NH_Blk_alone_CEN_2010 +
                          NH_AIAN_alone_CEN_2010 +
                          NH_Asian_alone_CEN_2010 +
                          NH_NHOPI_alone_CEN_2010 +
                          NH_SOR_alone_CEN_2010,
                        data = sf_precincts)
# not sure at all why the full model had the best MSE - very spooky
# none of them are even significant
# do cross validation instead?

# logistic regressions

logit_over <- glm(cbind(over_count, no_over_count) ~
#                    Pop_18_24_CEN_2010
                  + Pop_25_44_CEN_2010
#                  + Pop_45_64_CEN_2010
                  + Pop_65plus_CEN_2010
#                  + Tot_GQ_CEN_2010
#                  + Inst_GQ_CEN_2010
#                  + Non_Inst_GQ_CEN_2010
#                  + Hispanic_CEN_2010
                  + NH_White_alone_CEN_2010
#                  + NH_Blk_alone_CEN_2010
#                  + NH_AIAN_alone_CEN_2010
                  + NH_Asian_alone_CEN_2010
#                  + NH_NHOPI_alone_CEN_2010
#                  + NH_SOR_alone_CEN_2010
                  ,
                  data = sf_precincts, family = binomial)
# oldest age group, 25-44yo (less significant) increase chances of overvoting
# Whites, Asians decrease chances of overvoting


logit_under <- glm(cbind(under_count, no_under_count) ~
                  Pop_18_24_CEN_2010
                  + Pop_25_44_CEN_2010
                  + Pop_45_64_CEN_2010
#                  + Pop_65plus_CEN_2010
                  + Tot_GQ_CEN_2010
#                  + Inst_GQ_CEN_2010
                  + Non_Inst_GQ_CEN_2010
                  + Hispanic_CEN_2010
                  + NH_White_alone_CEN_2010
                  + NH_Blk_alone_CEN_2010
                  + NH_AIAN_alone_CEN_2010
#                  + NH_Asian_alone_CEN_2010
#                  + NH_NHOPI_alone_CEN_2010
#                  + NH_SOR_alone_CEN_2010
                  ,
                  data = sf_precincts, family = binomial)
# all age groups except for the oldest less likely to undervote
# people in GQs less likely to undervote, institutional GQs more likely to undervote
# Black and White more likely to undervote, Native and Hispanic less likely to undervote
