# this script performs multiple imputation on the ballot data
# you can change the number of iterations on line 16 and write to the data files
# heads up that this takes so so much time, you can barely get 200 draws overnight

# remotes::install_github('ds-elections/rcv')
library(hot.deck)
library(here)
library(dplyr)
library(tidyr)
library(rcv)
library(simputation)
library(ggplot2)
library(sf)
library(nnet)
options(simputation.hdbackend="VIM")
its <- 10

load(here('data', 'sf_ballot.RData'))

orig_wide <- readable(sf) %>% filter(contest == 'Mayor')

precincts <- distinct(sf, pref_voter_id, precinct)

imputation_counts <- data.frame()
imputation_results <- data.frame()

# so this just organizes the data so that duplicates are dropped and non-NAs moved to the front
sf_wide <- sf %>%
  filter(contest == 'Mayor') %>%
  arrange(pref_voter_id, vote_rank) %>%
  filter(!is.na(candidate)) %>%
  distinct(pref_voter_id, candidate, .keep_all = TRUE) %>%
  group_by(pref_voter_id) %>%
  mutate(vote_rank = row_number()) %>%
  readable() %>%
  left_join(precincts, by = 'pref_voter_id') %>%
  ungroup() %>%
  rename(x1 = `1`,
         x2 = `2`,
         x3 = `3`)

# originalcounts & results #####
counts <- sf_wide %>%
  rename(`1` = x1,
         `2` = x2,
         `3` = x3) %>%
  count(`1`, `2`, `3`) %>%
  arrange(desc(n)) %>%
  mutate(prop = n / sum(n),
         impute = 'original',
         trial = 1)

results <- rcv_tally(sf, 'Mayor') %>%
  mutate(impute = 'original',
         trial = 1,
         rank = row_number())

imputation_counts <- bind_rows(imputation_counts, counts)
imputation_results <- bind_rows(imputation_results, results)

# listwise deletion #####
listwise <- sf_wide %>%
  filter(complete.cases(.)) %>%
  rename(`1` = x1,
         `2` = x2,
         `3` = x3)

long <- listwise %>%
  gather(key = 'vote_rank', value = 'candidate', `1`, `2`, `3`) %>%
  arrange(pref_voter_id)

counts <- count(listwise, `1`, `2`, `3`) %>%
  arrange(desc(n)) %>%
  mutate(prop = n / sum(n),
         impute = 'listwise',
         trial = 1)

results <- rcv_tally(long) %>%
  mutate(impute = 'listwise',
         trial = 1,
         rank = row_number())

imputation_counts <- bind_rows(imputation_counts, counts)
imputation_results <- bind_rows(imputation_results, results)

# imputed on nothing - selection from a random row #####
# this is gonna have a ton of weird combinations that don't show up in the data that often

unique_cands <- na.omit(unique(sf$candidate[sf$contest == "Mayor"]))

for (cycle in seq_len(its)) {
  imputed <- sf_wide
  for (i in unique_cands) {
    missing_rows <- filter(imputed, is.na(x2) & (x1 == i))
    donor_rows <- filter(imputed, !is.na(x2) & (x2 != i))
    imputed$x2[is.na(imputed$x2) &
                 (imputed$x1 == i)] <- sample(donor_rows$x2,
                                              nrow(missing_rows))
    
    for (j in unique_cands[unique_cands != i]) {
      missing_rows <- filter(imputed, is.na(x3) & (x1 == i) & (x2 == j))
      donor_rows <- filter(imputed, !is.na(x3) & !(x3 %in% c(i,j)))
      imputed$x3[is.na(imputed$x3) &
                   (imputed$x1 == i) &
                   (imputed$x2 == j)] <- sample(donor_rows$x2,
                                                nrow(missing_rows)) 
    }
  }
  
  imputed_done <-  rename(imputed,
                     `1` = x1,
                     `2` = x2,
                     `3` = x3)
  
  long <- gather(imputed_done, key = 'vote_rank', value = 'candidate', `1`, `2`, `3`) %>%
    arrange(pref_voter_id)
  
  counts <- count(imputed_done, `1`, `2`, `3`) %>%
    arrange(desc(n)) %>%
    mutate(new_prop = n / sum(n)) %>%
    mutate(impute = 'nothing',
           trial = cycle)

  results <- rcv_tally(long) %>%
    mutate(impute = 'nothing',
           trial = cycle,
           rank = row_number())
  
  imputation_counts <- bind_rows(imputation_counts, counts)
  imputation_results <- bind_rows(imputation_results, results)
}

# imputed on vote choice alone #####

for (i in seq_len(its)){
  sf_wide_rand <- sample_frac(sf_wide)
  imputed <- impute_rhd(sf_wide, x2 ~ x1) %>%
    impute_rhd(x3 ~ x1 + x2) %>%
    impute_rhd(x3 ~ x1) %>%
    rename(`1` = x1,
           `2` = x2,
           `3` = x3)
  
  long <- gather(imputed, key = 'vote_rank', value = 'candidate', `1`, `2`, `3`) %>%
    arrange(pref_voter_id)
  
  counts <- count(imputed, `1`, `2`, `3`) %>%
    arrange(desc(n)) %>%
    mutate(new_prop = n / sum(n)) %>%
    mutate(impute = 'vote_choice',
           trial = i)

  results <- rcv_tally(long) %>%
    mutate(impute = 'vote_choice',
           trial = i,
           rank = row_number())
  
  imputation_counts <- bind_rows(imputation_counts, counts)
  imputation_results <- bind_rows(imputation_results, results)
}

 extra <- imputation_results %>%
   filter(impute == 'vote_choice') %>%
   filter(candidate != 'NA') %>%
   group_by(trial) %>%
   mutate(prop = round1 / sum(round1, na.rm = TRUE)) %>%
   slice(1:5)

# hey what in the WORLD is going on when you iterate the rounds forward from 2 to 3
 ggplot(extra, aes(x = prop)) + 
   geom_histogram() + facet_wrap(~candidate, scales = 'free')


# so the MOST that any group is off (from original listwise) by here is 1 percentage point
# and almost all are within a fifth of a point
# maybe when I build the model on more things it wil change more
# I think this "difference" in proportional leaf sizes is a good way of measuring change

# another interesting metric - how many people are "exhausted" by the end when you do this?
# it should super decrease

# OH and maybe see how when you impute multiple times the order / variability changes
# simulating multiple times to get a "distribution" among different candidate placements

# using MSE (or another case-wise metric) on each one to get an accuracy measure isn't great
# because the methods we're using are so coarse, all we can hope for is distributional accuracy

# another metric - London Breed's vote share?
# and how does this decrease the percentage of votes that are exhausted
# probably compare all of the methods to just regular

# imputed on vote choice and precinct #####

for (i in seq_len(its)){
  imputed <- impute_rhd(sf_wide, x2 ~ x1 + precinct) %>%
    impute_rhd(x2 ~ x1) %>%
    impute_rhd(x3 ~ x1 + x2 + precinct) %>%
    impute_rhd(x3 ~ x1 + x2) %>%
    impute_rhd(x3 ~ x1) %>%
    rename(`1` = x1,
           `2` = x2,
           `3` = x3)
  # you have to use these fill-in steps to make sure everything is completed
  # an interesting thing - see how many cases are uncompleted by each of the methods
  
  long <- gather(imputed, key = 'vote_rank', value = 'candidate', `1`, `2`, `3`) %>%
    arrange(pref_voter_id)
  
  counts <- count(imputed, `1`, `2`, `3`) %>%
    arrange(desc(n)) %>%
    mutate(new_prop = n / sum(n)) %>%
    mutate(impute = 'vote_choice_precinct',
           trial = i)

  results <- rcv_tally(long) %>%
    mutate(impute = 'vote_choice_precinct',
           trial = i,
           rank = row_number())
  
  imputation_counts <- bind_rows(imputation_counts, counts)
  imputation_results <- bind_rows(imputation_results, results)
}

# multinomial model #####

load(here('data', 'combined_precincts.RData'))
whole_data <- left_join(sf_wide, combined_pcts, by = 'precinct')

# doing this just based on first vote choice should be somewhat identical to hot deck

# do this on demos plus vote choice
# demos are:
# race (asian and black, bc two of the major candidates were as such)
# female
# age (youngest and oldest, bc they vote the least and most respectively)
# education, via % with no HS degree (or college, but only do one)
# poverty
# and not speaking english


complete_12 <- filter(whole_data, !is.na(x2))
complete_123 <- filter(whole_data, !is.na(x3))
missing2 <- filter(whole_data, is.na(x2))
# complete_123 is up here so the model for x3 isn't built on x2, mostly to save computing time
# HOWEVER it is going to predict based on the imputed x2 later, to make the rows work out

multi_model2 <- multinom(x2 ~ x1
                       + female 
                       + pop_18_24 
                       + pop_65_plus 
                       + black 
                       + asian 
                       + no_hs 
                       + poverty 
                       + no_english, data = complete_12, maxit = 200)
# doing demographics alone can cause overlap, so we have to include first vote choice
# I could get around it by figuring out how to exclude earlier cases, but no time...

multi_model3 <- multinom(x3 ~ x1 + x2
                         + female 
                         + pop_18_24 
                         + pop_65_plus 
                         + black 
                         + asian 
                         + no_hs 
                         + poverty 
                         + no_english, data = complete_123, maxit = 200)

for (i in seq_len(its)) {
  imputed <- whole_data
  
  # round 2
  probs <- predict(multi_model2, type = "probs", newdata = missing2) %>% data.frame()
  
  pred <- apply(probs, 1, sample, x=colnames(probs), size=1, replace = TRUE) %>%
    stringr::str_replace_all('\\.', ' ') %>% # this puts spaces back in
    stringr::str_replace_all('  ', '\\. ') %>% # this fixes middle initials
    stringr::str_replace_all('WRITE IN', 'WRITE-IN') # this fixes write-ins
  
  imputed$x2[is.na(imputed$x2)] <- pred
  
  # round 3
  missing3 <- filter(imputed, is.na(x3))
  probs <- predict(multi_model3, type = "probs", newdata = missing3) %>% data.frame()
  
  pred <- apply(probs, 1, sample, x=colnames(probs), size=1, replace = TRUE) %>%
    stringr::str_replace_all('\\.', ' ') %>%
    stringr::str_replace_all('  ', '\\. ') %>%
    stringr::str_replace_all('WRITE IN', 'WRITE-IN')
  imputed$x3[is.na(imputed$x3)] <- pred
  
  imputed <- select(imputed, colnames(sf_wide)) %>%
    rename(`1` = x1,
           `2` = x2,
           `3` = x3)
  
  long <- gather(imputed, key = 'vote_rank', value = 'candidate', `1`, `2`, `3`) %>%
    arrange(pref_voter_id)
  
  counts <- count(imputed, `1`, `2`, `3`) %>%
    arrange(desc(n)) %>%
    mutate(new_prop = n / sum(n)) %>%
    mutate(impute = 'multinomial',
           trial = i)
  
  results <- rcv_tally(long) %>%
    mutate(impute = 'multinomial',
           trial = i,
           rank = row_number())
  
  imputation_counts <- bind_rows(imputation_counts, counts)
  imputation_results <- bind_rows(imputation_results, results)
}

# summary #####

# these three things differ in their 2nd / 3rd ranking!
# orig_results, listwise_results, imputed_results

# I expected the by-precinct one to make more of a difference, bc some precincts are so under-represented
if (!file.exists(here('data', 'imputation_counts.csv'))) {
  readr::write_csv(imputation_counts, here('data', 'imputation_counts.csv'))
} else {
  readr::write_csv(imputation_counts, here('data', 'imputation_counts.csv'), append = TRUE)
}

if (!file.exists(here('data', 'imputation_results.csv'))) {
  readr::write_csv(imputation_results, here('data', 'imputation_results.csv'))
} else {
  readr::write_csv(imputation_results, here('data', 'imputation_results.csv'), append = TRUE)
}

