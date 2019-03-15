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
  mutate(weight = population / sum(population))
count_cols <- c('over_count', 'no_over_count', 'under_count', 'no_under_count')
sf_precincts[str_detect(sf_precincts$precinct, '/') %in% TRUE, count_cols] <- round(sf_precincts[str_detect(sf_precincts$precinct, '/') %in% TRUE, count_cols] * double_cases$weight)
# this gives much nicer results

# and get precinct turnout
sf_precincts <- mutate(sf_precincts,
                       overvote_rate = over_count / (over_count + no_over_count),
                       undervote_rate = under_count / (under_count + no_under_count),
                       turnout = under_count + no_under_count,
                       no_turnout = population - turnout,
                       turnout_rate = turnout / population) %>%
  na.omit()

sf_wrong <- left_join(sf_precincts, demo_data_int, by = c('PREC_2017' = 'PREC_2017_wrong'))

# linear turnout #####

set.seed(314159265)
turnout_formula <- turnout_rate ~ female + pop_18_24 + pop_25_44 + pop_45_64 +
  pop_65_up + hispanic + white + black + native + asian + pac_islander +
  other_race + no_hs + college + poverty + no_english
nvars <- 16

iters <- 100
turnout_models <- vector("list", length = iters)
turnout_bic <- vector(iters)

for (i in seq_len(iters)) {
  train_index <- sample(c(TRUE, FALSE), size = nrow(sf_precincts), replace = TRUE)
  training <- filter(sf_precincts, train_index)
  test <- filter(sf_precincts, !train_index)
  
  linear_turnout <- regsubsets(turnout_formula, data = training, method = 'exhaustive', nvmax = nvars)
  # best subset selection
  turnout_models[[i]] <- coef(linear_turnout, which.min(summary(linear_turnout)$bic))
  turnout_bic[i] <- min(summary(linear_turnout)$bic)
}

turnout_models[order(turnout_bic)]

# so this isn't a very mathematical heuristic 
# but the best few models consistently have
# pop_18_24, pop_45_64, college
# then white, no_hs, female
# then black, poverty, pop_65_up

best_linear_turnout <- lm(turnout_rate ~ pop_18_24 + pop_45_64 + college, data = sf_precincts)



# linear model for overvoting #####
# bootstrap these instead? to get different splits
over_formula <- overvote_rate ~ female + pop_18_24 + pop_25_44 + pop_45_64 +
  pop_65_up + hispanic + white + black + native + asian + pac_islander +
  other_race + no_hs + college + poverty + no_english
nvars <- 16

linear_over_backward <- regsubsets(over_formula, data = training, method = 'backward', nvmax = nvars)
linear_over_forward <- regsubsets(over_formula, data = training, method = 'forward', nvmax = nvars)
# backward selection
test.mat <- model.matrix(over_formula, data = test)
back_errors <- rep(NA, nvars)
for (i in 1:nvars) {
  coefi <- coef(linear_over_backward, id = i)
  pred <- test.mat[ ,names(coefi)]%*%coefi
  back_errors[i] <- mean((test$overvote_rate - pred)^2)
}
coef(linear_over_backward, which.min(back_errors))
# forward selection
test.mat <- model.matrix(over_formula, data = test)
for_errors <- rep(NA, nvars)
for (i in 1:nvars) {
  coefi <- coef(linear_over_forward, id = i)
  pred <- test.mat[ ,names(coefi)]%*%coefi
  for_errors[i] <- mean((test$overvote_rate - pred)^2)
}
coef(linear_over_forward, which.min(for_errors))
# they agree here!
best_linear_over <- lm(overvote_rate ~ hispanic + black + no_english, data = sf_precincts)

# linear model for undervoting #####
under_formula <- undervote_rate ~ female + pop_18_24 + pop_25_44 + pop_45_64 +
  pop_65_up + hispanic + white + black + native + asian + pac_islander +
  other_race + no_hs + college + poverty + no_english

linear_under_backward <- regsubsets(under_formula, data = training, method = 'backward', nvmax = nvars)
linear_under_forward <- regsubsets(under_formula, data = training, method = 'forward', nvmax = nvars)
# backward selection
test.mat <- model.matrix(under_formula, data = test)
back_errors <- rep(NA, nvars)
for (i in 1:nvars) {
  coefi <- coef(linear_under_backward, id = i)
  pred <- test.mat[ ,names(coefi)]%*%coefi
  back_errors[i] <- mean((test$undervote_rate - pred)^2)
}
coef(linear_under_backward, which.min(back_errors))
# forward selection
test.mat <- model.matrix(under_formula, data = test)
for_errors <- rep(NA, nvars)
for (i in 1:nvars) {
  coefi <- coef(linear_under_forward, id = i)
  pred <- test.mat[ ,names(coefi)]%*%coefi
  for_errors[i] <- mean((test$undervote_rate - pred)^2)
}
coef(linear_under_forward, which.min(for_errors))
# they do not agree here! backwards has a lower test MSE
best_linear_under <- lm(overvote_rate ~ pop_18_24 + pop_25_44 + pop_45_64 +
                          white + black + no_hs + college, data = sf_precincts)


# logistic regressions

logit_over <- glm(cbind(over_count, no_over_count) ~
#                  + pop_18_24
#                  + pop_25_44
#                  + pop_45_64
#                  + pop_65_up
                  + hispanic
#                  + white
                  + black
#                  + native
#                  + asian
#                  + pac_islander
#                  + other_race
#                  + no_hs
#                  + college
#                  + poverty
                  + no_english
                  ,
                  data = sf_precincts, family = binomial)


logit_under <- glm(cbind(under_count, no_under_count) ~
                  + pop_18_24
                  + pop_25_44
                  + pop_45_64
#                  + pop_65_up
                  + hispanic
                  + white
                  + black
#                  + native
                  + asian
#                  + pac_islander
#                  + other_race
                  + no_hs
                  + college
#                  + poverty
#                   + no_english
                   ,
                   data = sf_precincts, family = binomial)
