# remotes::install_github('ds-elections/rcv')
library(hot.deck)
library(here)
library(dplyr)
library(tidyr)
library(rcv)
library(simputation)
library(VIM)
options(simputation.hdbackend="VIM")

load(here('data', 'sf_ballot.RData'))

orig_wide <- readable(sf) %>% filter(contest == 'Mayor')

precincts <- distinct(sf, pref_voter_id, precinct)

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

orig_counts <- count(sf_wide, `x1`, `x2`, `x3`) %>%
  arrange(desc(n)) %>%
  mutate(prop = n / sum(n))

orig_results <- rcv_tally(sf, 'Mayor') %>%
  mutate(impute = 'original',
         trial = 1)

# listwise deletion #####
listwise <- sf_wide %>%
  filter(complete.cases(.)) %>%
  rename(`1` = x1,
         `2` = x2,
         `3` = x3)

listwise_long <- listwise %>%
  gather(key = 'vote_rank', value = 'candidate', `1`, `2`, `3`) %>%
  arrange(pref_voter_id)

listwise_counts <- count(listwise, `1`, `2`, `3`) %>%
  arrange(desc(n)) %>%
  mutate(prop = n / sum(n))

listwise_results <- rcv_tally(listwise_long) %>%
  mutate(impute = 'listwise',
         trial = 1)


# imputed on vote choice alone #####

vc_counts <- data.frame()
vc_results <- data.frame()

for (i in seq_len(1000)){
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
  vc_counts <- bind_rows(vc_counts, counts)
  
  results <- rcv_tally(long) %>%
    mutate(impute = 'vote_choice',
           trial = i,
           rank = row_number())
  vc_results <- bind_rows(vc_results, results)
}
# ¸¸¸¸Çdwddddddddddd# so weird stuff
# looping over it is giving zero variability in the metrics I'm interested in
# they're literally identical
# the imputation is being random, like we want
# maybe it's just law-of-large-numbers-ing itself?
# update I'm just dumb


vc_diff <- full_join(listwise_counts, vc_counts, by = c('1', '2', '3')) %>%
  select(-n.x, -n.y) %>%
  mutate(difference = new_prop - prop)

# so the MOST that any group is off (from original listwise) by here is 1 percentage point
# and almost all are within a fifth of a point
# maybe when I build the model on more things it wil change more
# I think this "difference" in proportional leaf sizes is a good way of measuring change

# another interesting metric - how many people are "exhausted" by the end when you do this?
# it should super decrease

# OH and maybe see how when you impute multiple times the order / variability changes
# simulating multiple times to get a "distribution" among different candidate placements

# another metric - London Breed's vote share?
# and how does this decrease the percentage of votes that are exhausted
# probably compare all of the methods to just regular

# imputed on vote choice and precinct #####
vc_pct <- impute_rhd(sf_wide, x2 ~ x1 + precinct) %>%
  impute_rhd(x2 ~ x1) %>%
  impute_rhd(x3 ~ x1 + x2 + precinct) %>%
  impute_rhd(x3 ~ x1 + x2) %>%
  impute_rhd(x3 ~ x1) %>%
  rename(`1` = x1,
         `2` = x2,
         `3` = x3)

# you have to use these fill-in steps to make sure everything is completed
# an interesting thing - see how many cases are uncompleted by each of the methods

vc_pct_long <- gather(vc_pct, key = 'vote_rank', value = 'candidate', `1`, `2`, `3`) %>%
  arrange(pref_voter_id)

# use the VIM package to do some visualizations/explorations

vc_pct_counts <- count(vc_pct, `1`, `2`, `3`) %>%
  arrange(desc(n)) %>%
  mutate(new_prop = n / sum(n))

vc_pct_diff <- full_join(listwise_counts, vc_pct_counts, by = c('1', '2', '3')) %>%
  select(-n.x, -n.y) %>%
  mutate(difference = new_prop - prop)

hist(abs(vc_pct_diff$difference))

vc_pct_results <- rcv_tally(vc_pct_long)

# summary #####

# these three things differ in their 2nd / 3rd ranking!
# orig_results, listwise_results, imputed_results

# I expected the by-precinct one to make more of a difference, bc some precincts are so under-represented
pct_under <- sf_wide %>%
  group_by(precinct) %>%
  summarize(uv_rate = mean(is.na(x3)))
