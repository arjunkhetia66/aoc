#day 2 - part 2
setwd(here::here())
source("utils.R")
setwd(paste(here::here('2022')))

#outcomes
outcomes <- read_csv('day2/outcomes.csv')
needed_outcome <- c(X = 'lose', Y = 'draw', Z = 'win')
#selection scores 
selection_scores <- list(X = 1, Y = 2, Z = 3)
#outcome scores
outcome_scores <- list('win' = 6, 'lose' = 0, 'draw' = 3)

#load in rounds data
rounds <- readr::read_delim("day2/day2_input.txt", delim = " ", col_names = FALSE) %>%
  rename_columns(list(X1 = 'opponent', X2 = 'win')) %>%
  mutate(win_or_lose = unlist(purrr::map(win, ~needed_outcome[[.]]))) %>% 
  inner_join(outcomes, by = c('opponent', 'win_or_lose' = 'win'))

rounds_what_play <- rounds %>%
  mutate(win_or_lose = unlist(purrr::map(win, ~needed_outcome[[.]]))) %>% 
  #get score
  mutate(score = unlist(purrr::map2(me, win_or_lose, ~outcome_scores[[.y]] + selection_scores[[.x]])))
  
#final score
print(sum(make_list_from_col(rounds_what_play, 'score')))