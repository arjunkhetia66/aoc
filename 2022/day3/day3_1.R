#day 3
setwd(here::here())
source("utils.R")
setwd(paste(here::here('2022')))

no_compartments <- 2

priorities <- tibble(item = c(letters, LETTERS), priority = 1:52)

common_string <- function(string1, string2) {
  
  string1_split <- unlist(strsplit(string1, split = ""))
  string2_split <- unlist(strsplit(string2, split = ""))
  return(intersect(string1_split, string2_split))
}

#load rucksacks
rucksacks <- read_csv('day3/day3_input.csv', col_names = FALSE) %>% 
  rename(rucksack = X1) 

sacks_split <- rucksacks
for (i in 1:no_compartments) {
  sacks_split <- sacks_split %>%
    mutate("compartment_{i}" := purrr::map(rucksack, ~substr(., (i-1)*nchar(.)/no_compartments + 1, i*nchar(.)/no_compartments)))
}

sacks_common <- sacks_split %>%
  mutate(common_item = unlist(purrr::map2(compartment_1, compartment_2, ~common_string(.x,.y)))) %>%
  #add priority
  inner_join(priorities, by = c('common_item' = 'item'))

print(sum(make_list_from_col(sacks_common, 'priority')))

