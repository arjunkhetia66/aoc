#day 2 - part 1
setwd(here::here())
source("utils.R")
setwd(paste(here::here('2022')))

#load in rounds data
rounds <- readr::read_delim("day2/day2_test_input.txt", delim = " ", col_names = FALSE) %>%
  rename_columns(list(X1 = 'opponent', X2 = 'me'))
#outcomes
outcomes <- read_csv('day2/outcomes.csv')
#outcome scores
outcome_scores <- list('win' = 6, 'lose' = 0, 'draw' = 3)
#selection scores 
selection_scores <- list(X = 1, Y = 2, Z = 3)

#play out rounds
rounds_outcomes <- rounds %>%
  #get outcome
  inner_join(outcomes, by = c('opponent','me')) %>%
  #get score
  mutate(score = unlist(purrr::map2(me, win, ~outcome_scores[[.y]] + selection_scores[[.x]])))

#final score
print(sum(make_list_from_col(rounds_outcomes, 'score')))
