#day 5
setwd(here::here())
source("utils.R")
setwd(paste(here::here('2022')))

move_crates <- function(crate_input, from, to, how_many) {
  
  crate_from <- crate_input[[from]]
  
  crate_out <- crate_input
  crate_out[[to]] <- c(crate_out[[to]], rev(crate_from[(length(crate_from) - how_many + 1) : length(crate_from)]))
  if ((length(crate_from) - how_many) == 1){
    crate_out[[from]] <- c('floor')
  } else {
    crate_out[[from]] <- crate_out[[from]][1 : (length(crate_from) - how_many)]
  }
  
  return(crate_out)
}

move_crates_v2 <- function(crate_input, from, to, how_many) {
  
  crate_from <- crate_input[[from]]
  
  crate_out <- crate_input
  crate_out[[to]] <- c(crate_out[[to]], (crate_from[(length(crate_from) - how_many + 1) : length(crate_from)]))
  if ((length(crate_from) - how_many) == 1){
    crate_out[[from]] <- c('floor')
  } else {
    crate_out[[from]] <- crate_out[[from]][1 : (length(crate_from) - how_many)]
  }
  
  return(crate_out)
}

crates_df <- read_table('day5/day5_input_crates.csv', col_names = FALSE)
no_stacks <- ncol(crates_df)
crates <- purrr::map(1:no_stacks, ~remove_element_from_vector(rev(make_list_from_col(crates_df, .)), exclude = ""))
crates <- purrr::map(crates, ~c('floor', .))

moves <- read_lines('day5/day5_input_moves.txt')
#keep numbers in moves
get_moves <- function(move) {
  matches <- regmatches(move, gregexpr("[[:digit:]]+", move))
  return(as.numeric(unlist(matches)))
}
all_moves <- purrr::map(moves, ~get_moves(.))

#move boxes by plan
crates_moved <- crates
print(crates)
for (i in all_moves) {
  print(paste0('move ', i[1], ' from ', i[2],' to ', i[3]))
  crates_moved <- move_crates_v2(crate_input =  crates_moved, from = i[2], to = i[3], how_many = i[1])
  print(crates_moved)
}


