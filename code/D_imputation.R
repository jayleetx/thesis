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

orig_results <- rcv_tally(sf, 'Mayor')

# this is what listwise deletion would look like
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

listwise_results <- rcv_tally(listwise_long)

# and here's random hot deck

test <- sample_n(data, 10000)

# oh perfect this is LIGHTNING fast
# but does have one problem, with the data: if there's no matching case, it leaves NA
# which is a great move, but not what I'm going for
# this really mostly happens when the second imputed case is rare
# SO we're gonna hack this by just imputing these handful of cases based on 1 at the end
imputed <- impute_rhd(sf_wide, x2 ~ x1) %>%
  impute_rhd(x3 ~ x1 + x2) %>%
  impute_rhd(x3 ~ x1) %>%
  rename(`1` = x1,
         `2` = x2,
         `3` = x3)

imputed_long <- tidyr::gather(imputed, key = 'vote_rank', value = 'candidate', `1`, `2`, `3`) %>%
  arrange(pref_voter_id)

# use the VIM package to do some visualizations/explorations

imputed_counts <- count(imputed, `1`, `2`, `3`) %>%
  arrange(desc(n)) %>%
  mutate(new_prop = n / sum(n))

errors <- full_join(listwise_counts, imputed_counts, by = c('1', '2', '3')) %>%
  select(-n.x, -n.y) %>%
  mutate(difference = new_prop - prop)

hist(errors$difference)
# so the MOST that any group is off by here is 1 percentage point
# maybe when I build the model on more things it wil change more
# I think this "difference" in proportional leaf sizes is a good way of measuring change

# another interesting metric - how many people are "exhausted" by the end when you do this?
# it should super decrease

# so something here is breaking and we're not getting the final round calculated
# there's probably something hard-coded into the function that accounts for NAs that are now gone
# that's it and I'll fix it later
# probably change "n unique candidates minus 2" to "n unique non-NA candidates minus 1"
imputed_results <- rcv_tally(imputed_long)
# okay that's fixed now but I gotta reload it from my github repo

# these three things are all different in their 2nd / 3rd ranking!
# orig_results, listwise_results, imputed_results