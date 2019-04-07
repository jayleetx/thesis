# this script runs subset selection to pick linear and logistic models
# the output isn't linked to the thesis doc,
# so you have to change the model specification there if anything changes

# this also requires rJava throuth glmulti,
# so I've been running it on the server to do logistic selection,
# because rJava doesn't work on my laptop
# another part of why it doesn't link directly to the thesis doc

library(sf)
library(leaps)
library(dplyr)
library(here)

sf_precincts <- st_read(dsn = here("data", "sf_precincts"), stringsAsFactors = FALSE)
colnames(sf_precincts) <- c('precinct', 'over_count', 'no_over_count', 'under_count',
                            'no_under_count', 'PREC_2017', 'population', 'female',
                            'pop_18_24', 'pop_25_44', 'pop_45_64', 'pop_65_plus',
                            'hispanic', 'white', 'black', 'native', 'asian', 'pac_islander',
                            'other_race', 'no_hs', 'college', 'poverty', 'no_english',
                            'weight', 'overvote_rate', 'undervote_rate', 'turnout',
                            'no_turnout', 'turnout_rate', 'geometry')

# linear turnout #####

set.seed(314159265)
turnout_formula <- turnout_rate ~ female + pop_18_24 + pop_25_44 + pop_45_64 +
  pop_65_plus + hispanic + white + black + native + asian + pac_islander +
  other_race + no_hs + college + poverty + no_english
nvars <- 16

iters <- 100
turnout_models <- vector("list", length = iters)
turnout_bic <- vector(length = iters)

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


# linear overvoting #####

over_formula <- overvote_rate ~ female + pop_18_24 + pop_25_44 + pop_45_64 +
  pop_65_plus + hispanic + white + black + native + asian + pac_islander +
  other_race + no_hs + college + poverty + no_english
nvars <- 16

iters <- 100
over_models <- vector("list", length = iters)
over_bic <- vector(length = iters)

for (i in seq_len(iters)) {
  train_index <- sample(c(TRUE, FALSE), size = nrow(sf_precincts), replace = TRUE)
  training <- filter(sf_precincts, train_index)
  test <- filter(sf_precincts, !train_index)
  
  linear_over <- regsubsets(over_formula, data = training, method = 'exhaustive', nvmax = nvars)
  # best subset selection
  over_models[[i]] <- coef(linear_over, which.min(summary(linear_over)$bic))
  over_bic[i] <- min(summary(linear_over)$bic)
}

over_models[order(over_bic)]

# so this isn't a very mathematical heuristic 
# but the best few models consistently have
# black, college


# linear undervoting #####

under_formula <- undervote_rate ~ female + pop_18_24 + pop_25_44 + pop_45_64 +
  pop_65_plus + hispanic + white + black + native + asian + pac_islander +
  other_race + no_hs + college + poverty + no_english
nvars <- 16

iters <- 100
under_models <- vector("list", length = iters)
under_bic <- vector(length = iters)

for (i in seq_len(iters)) {
  train_index <- sample(c(TRUE, FALSE), size = nrow(sf_precincts), replace = TRUE)
  training <- filter(sf_precincts, train_index)
  test <- filter(sf_precincts, !train_index)
  
  linear_under <- regsubsets(over_formula, data = training, method = 'exhaustive', nvmax = nvars)
  # best subset selection
  under_models[[i]] <- coef(linear_under, which.min(summary(linear_under)$bic))
  under_bic[i] <- min(summary(linear_under)$bic)
}

under_models[order(under_bic)]

# so this isn't a very mathematical heuristic 
# but the best few models consistently have
# black, no_hs


# logistic regressions #####

