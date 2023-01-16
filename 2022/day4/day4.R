#day 4
setwd(here::here())
source("utils.R")
setwd(paste(here::here('2022')))

common_sections <- function(sections1, sections2){
  
  sections1 <- as.integer(strsplit(sections1,'-')[[1]][1]):as.integer(strsplit(sections1,'-')[[1]][2])
  sections1_length <- length(sections1)
  
  sections2 <- as.integer(strsplit(sections2,'-')[[1]][1]):as.integer(strsplit(sections2,'-')[[1]][2])
  sections2_length <- length(sections2)
  
  common_sections_out <- lubridate::intersect(sections1, sections2)
  common_size <- length(common_sections_out)
  
  return(list('common_size' = common_size, 
              'sections1_length' = sections1_length,
              'sections2_length' = sections2_length))
}

#load list
section_pairs <- read_csv('day4/day4_input.csv', col_names = FALSE) %>% 
  rename(elf1 = X1, elf2 = X2) %>%
  mutate(common = unlist(purrr::map2(elf1, elf2, ~common_sections(.x,.y)[['common_size']])),
         elf1_size = unlist(purrr::map2(elf1, elf2, ~common_sections(.x,.y)[['sections1_length']])),
         elf2_size = unlist(purrr::map2(elf1, elf2, ~common_sections(.x,.y)[['sections2_length']])),
         complete_overlap = (common == elf1_size) | (common == elf2_size))

#part 1
print(sum(make_list_from_col(section_pairs, 'complete_overlap')))

#part 2
print(nrow(section_pairs %>% filter(common != 0)))
