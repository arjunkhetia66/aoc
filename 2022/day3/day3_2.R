#day 3 -part 2
setwd(here::here())
source("utils.R")
setwd(paste(here::here('2022')))

priorities <- tibble(item = c(letters, LETTERS), priority = 1:52)

common_string <- function(string1, string2, string3) {
  
  string1_split <- unlist(strsplit(string1, split = ""))
  string2_split <- unlist(strsplit(string2, split = ""))
  string3_split <- unlist(strsplit(string3, split = ""))
  return(intersect(intersect(string1_split, string2_split), string3_split))
}

#load rucksacks
rucksacks <- read_csv('day3/day3_input.csv', col_names = FALSE) %>% 
  rename(rucksack = X1) 
#add elf group
rucksacks <- rucksacks %>%
  mutate(elf_group = unlist(purrr::map((1:(nrow(rucksacks)/3)), ~rep(.,3)))) 

elf_group_sacks <- split(make_list_from_col(rucksacks, 'rucksack'), make_list_from_col(rucksacks, 'elf_group'))

common_group_items <- tibble(common_group_item = unlist(purrr::map(elf_group_sacks, ~common_string(.[[1]], .[[2]], .[[3]])))) %>% 
  #add priority
  inner_join(priorities, by = c('common_group_item' = 'item'))

print(sum(make_list_from_col(common_group_items, 'priority')))
