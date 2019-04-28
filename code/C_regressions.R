# this script runs subset selection to pick linear and logistic models
# the output isn't linked to the thesis doc,
# so you have to change the model specification there if anything changes

# this also requires rJava throuth glmulti,
# so I've been running it on the Reed RStudio server to do logistic selection,
# because rJava doesn't work on my laptop
# another part of why it doesn't link directly to the thesis doc

library(sf)
library(leaps)
library(dplyr)
library(here)
library(glmulti)

sf_precincts <- st_read(dsn = here("data", "sf_precincts"), stringsAsFactors = FALSE)
colnames(sf_precincts) <- c('precinct', 'over_count', 'no_over_count', 'under_count',
                            'no_under_count', 'PREC_2017', 'population', 'female',
                            'pop_18_24', 'pop_25_44', 'pop_45_64', 'pop_65_plus',
                            'hispanic', 'white', 'black', 'native', 'asian', 'pac_islander',
                            'other_race', 'no_hs', 'college', 'poverty', 'no_english',
                            'weight', 'overvote_rate', 'undervote_rate', 'turnout',
                            'no_turnout', 'turnout_rate', 'geometry')
set.seed(314159265)

# linear turnout #####

turnout_formula <- log(turnout_rate) ~ female + pop_18_24 + pop_25_44 + pop_45_64 +
  pop_65_plus + hispanic + white + black + native + asian + pac_islander +
  other_race + no_hs + college + poverty + no_english
nvars <- 16

iters <- 100
turnout_models <- vector("list", length = iters)
turnout_bic <- vector(length = iters)

for (i in seq_len(iters)) {
  bootstrap <- sample_n(sf_precincts, nrow(sf_precincts), replace = TRUE)
  # best subset selection
  linear_turnout <- regsubsets(turnout_formula, data = bootstrap, method = 'exhaustive', nvmax = nvars)
  turnout_models[[i]] <- coef(linear_turnout, which.min(summary(linear_turnout)$bic))
  turnout_bic[i] <- min(summary(linear_turnout)$bic)
}

lin_turnout_vars <- sort(table(unlist(lapply(turnout_models, names))), decreasing = TRUE)

# okay so now we're saying things that show up more often are better
# arbitrary number - more than half the time is Good

# pop_18_24
# no_hs
# college
# pop_45_64
# white



# linear overvoting #####

over_formula <- overvote_rate ~ female + pop_18_24 + pop_25_44 + pop_45_64 +
  pop_65_plus + hispanic + white + black + native + asian + pac_islander +
  other_race + no_hs + college + poverty + no_english
nvars <- 16

iters <- 100
over_models <- vector("list", length = iters)
over_bic <- vector(length = iters)

for (i in seq_len(iters)) {
  bootstrap <- sample_n(sf_precincts, nrow(sf_precincts), replace = TRUE)
  # best subset selection
  linear_over <- regsubsets(over_formula, data = bootstrap, method = 'exhaustive', nvmax = nvars)
  over_models[[i]] <- coef(linear_over, which.min(summary(linear_over)$bic))
  over_bic[i] <- min(summary(linear_over)$bic)
}

lin_over_vars <- sort(table(unlist(lapply(over_models, names))), decreasing = TRUE)

# okay so now we're saying things that show up more often are better
# arbitrary number - more than half the time is Good

# black


# linear undervoting #####

under_formula <- undervote_rate ~ female + pop_18_24 + pop_25_44 + pop_45_64 +
  pop_65_plus + hispanic + white + black + native + asian + pac_islander +
  other_race + no_hs + college + poverty + no_english
nvars <- 16

iters <- 100
under_models <- vector("list", length = iters)
under_bic <- vector(length = iters)

for (i in seq_len(iters)) {
  bootstrap <- sample_n(sf_precincts, nrow(sf_precincts), replace = TRUE)
  # best subset selection
  linear_under <- regsubsets(under_formula, data = bootstrap, method = 'exhaustive', nvmax = nvars)
  under_models[[i]] <- coef(linear_under, which.min(summary(linear_under)$bic))
  under_bic[i] <- min(summary(linear_under)$bic)
}

lin_under_vars <- sort(table(unlist(lapply(under_models, names))), decreasing = TRUE)

# okay so now we're saying things that show up more often are better
# arbitrary number - more than half the time is Good

# black
# pop_18_24
# pop_25_44
# pop_45_64
# college
# pac_islander
# white
# no_hs


# logistic turnout #####

logit_turnout_formula <- cbind(turnout, no_turnout) ~ female + pop_18_24 +
  pop_25_44 + pop_45_64 + pop_65_plus + hispanic + white + black + native +
  asian + pac_islander + other_race + no_hs + college + poverty + no_english

iters <- 100
logit_turnout_models <- vector("list", length = iters)
logit_turnout_bic <- vector(length = iters)

for (i in seq_len(iters)) {
  bootstrap <- sample_n(sf_precincts, nrow(sf_precincts), replace = TRUE)
  # best subset selection
  logit_turnout <- glmulti(logit_turnout_formula, data = bootstrap,
                           level = 1,               # No interaction considered
                           method = "g",            # Genetic algorithm approach
                           crit = "bic",            # BIC as criteria
                           confsetsize = 1,         # Keep best model
                           plotty = F, report = F,  # No plot or interim reports
                           fitfunction = "glm",     # glm function
                           family = binomial)       # binomial family for logistic regression
  logit_turnout_models[[i]] <- coef(logit_turnout)
  logit_turnout_bic[i] <- logit_turnout@crits
}

logit_turnout_vars <- sort(table(unlist(lapply(logit_turnout_models, names))), decreasing = TRUE)

# okay so now we're saying things that show up more often are better
# arbitrary number - more than half the time is Good

# college
# no_hs
# pop_18_24
# pop_45_64
# poverty
# asian
# pac_islander
# pop_65_plus
# female
# native
# no_english
# hispanic
# white
# black
# pop_25_44
# other_race


# logistic overvoting #####

logit_over_formula <- cbind(over_count, no_over_count) ~ female + pop_18_24 +
  pop_25_44 + pop_45_64 + pop_65_plus + hispanic + white + black + native +
  asian + pac_islander + other_race + no_hs + college + poverty + no_english

iters <- 100
logit_over_models <- vector("list", length = iters)
logit_over_bic <- vector(length = iters)

for (i in seq_len(iters)) {
  bootstrap <- sample_n(sf_precincts, nrow(sf_precincts), replace = TRUE)
  # best subset selection
  logit_over <- glmulti(logit_over_formula, data = bootstrap,
                        level = 1,               # No interaction considered
                        method = "g",            # Genetic algorithm approach
                        crit = "bic",            # BIC as criteria
                        confsetsize = 1,         # Keep best model
                        plotty = F, report = F,  # No plot or interim reports
                        fitfunction = "glm",     # glm function
                        family = binomial)       # binomial family for logistic regression
  logit_over_models[[i]] <- coef(logit_over)
  logit_over_bic[i] <- logit_over@crits
}

logit_over_vars <- sort(table(unlist(lapply(logit_over_models, names))), decreasing = TRUE)

# okay so now we're saying things that show up more often are better
# arbitrary number - more than half the time is Good

# black

# logistic undervoting #####

logit_under_formula <- cbind(under_count, no_under_count) ~ female + pop_18_24 +
  pop_25_44 + pop_45_64 + pop_65_plus + hispanic + white + black + native +
  asian + pac_islander + other_race + no_hs + college + poverty + no_english

iters <- 100
logit_under_models <- vector("list", length = iters)
logit_under_bic <- vector(length = iters)

for (i in seq_len(iters)) {
  bootstrap <- sample_n(sf_precincts, nrow(sf_precincts), replace = TRUE)
  # best subset selection
  logit_under <- glmulti(logit_under_formula, data = bootstrap,
                         level = 1,               # No interaction considered
                         method = "g",            # Genetic algorithm approach
                         crit = "bic",            # BIC as criteria
                         confsetsize = 1,         # Keep best model
                         plotty = F, report = F,  # No plot or interim reports
                         fitfunction = "glm",     # glm function
                         family = binomial)       # binomial family for logistic regression
  logit_under_models[[i]] <- coef(logit_under)
  logit_under_bic[i] <- logit_under@crits
}

logit_under_vars <- sort(table(unlist(lapply(logit_under_models, names))), decreasing = TRUE)

# okay so now we're saying things that show up more often are better
# arbitrary number - more than half the time is Good

# black
# college
# pop_18_24
# pop_25_44
# pop_45_64
# white
# asian
# hispanic
# no_hs
# pac_islander

