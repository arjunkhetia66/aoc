#day 5
setwd(here::here())
source("utils.R")
setwd(paste(here::here('2022')))

signal <- read_lines('day6/day6_input.txt') 

win_len <- 14

signal_split <- (signal %>% strsplit(''))[[1]]
unique_check <- runner::runner(signal_split, f = function(x) check_vector_uniqueness(x), k = win_len, na_pad = TRUE) 

print(which(unique_check[win_len: length(unique_check)])[[1]] + (win_len-1))
