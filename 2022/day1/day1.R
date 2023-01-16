#day 1 - part 1
setwd(here::here())
source("utils.R")
setwd(paste(here::here('2022')))

####################
#  !!! PART 1 !!!  #
####################
#load and transform inputs
all_calories <- readr::read_csv('day1/day1_input.csv', skip_empty_rows = FALSE)
all_calories <- make_list_from_col(all_calories, 'calories')
seperator_start_positions <-  c(1, which(is.na(all_calories)) + 1)
seperator_end_positions <- c(which(is.na(all_calories)) - 1, length(all_calories))

#seperate calories for each elf
calories_seperated <- purrr::map(1:length(seperator_start_positions), ~all_calories[seperator_start_positions[.]:seperator_end_positions[.]])

#sum of each elfs calories
calory_sums <- unlist(purrr::map(calories_seperated, ~sum(.)))

#max calories
max_calory <- max(calory_sums)
print(max_calory)

####################
#  !!! PART 2 !!!  #
####################
#sum max calories top 3
max_calory_top_3_sum <- sum((calory_sums %>% sort(decreasing = TRUE))[1:3])
print(max_calory_top_3_sum)