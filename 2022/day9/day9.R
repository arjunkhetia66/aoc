#day 9
setwd(here::here())
source("utils.R")
setwd(paste(here::here('2022')))

get_move <- function(direction) {
  
  moves <- list(R = c(1,0),
                L = c(-1,0),
                D = c(0,-1),
                U = c(0,1))
                
  return(moves[[direction]])
}
  
move_head <- function(head_pos_in, direction_ins){
  
  head_pos_out <- head_pos_in + get_move(direction = direction_ins)
  return(head_pos_out)
}

same_row_or_col_check <- function(head_pos, tail_pos) {
  x_same <- head_pos[1] == tail_pos[1]
  y_same <- head_pos[2] == tail_pos[2]
  return(x_same | y_same)
  
}

move_tail <- function(head_pos_in, tail_pos_in) {
  #tail only moves if head is not adjacent, i.e. 1 away (including diaganol)
  difference <- head_pos_in - tail_pos_in
  if (2 %in% abs(difference)) {
    move <- TRUE
  } else {
    move <- FALSE
  }
  
  if (move) {
    if (same_row_or_col_check(head_pos_in,tail_pos_in)) {
      tail_move <- difference/2
      tail_pos_out <- tail_pos_in + tail_move 
    } else {
      #loc_2 <- match(2,abs(difference))
      #intermediate <- difference
      #intermediate[loc_2] <- intermediate[loc_2]/2
      #tail_move <- intermediate
      tail_move <- round2(difference/2,0)
      tail_pos_out <- tail_pos_in + tail_move 
  }
  } else {
    tail_pos_out <- tail_pos_in
  }
  
  return(tail_pos_out)
}

head_moves <- read_lines("day9/day9_input.txt")
head_moves <- unlist(purrr::map(head_moves, ~rep(substr(.,1,1), as.integer(substr(.,3,nchar(.))))))
  
######################
#PART 1

head_pos <- c(0,0)
tail_pos <- c(0,0)
visited_tail_positions <- list()
counter <- 1
for (i in head_moves) {
  head_pos <- move_head(head_pos_in = head_pos, direction_ins = i)
  tail_pos <- move_tail(head_pos_in = head_pos, tail_pos_in = tail_pos)
  visited_tail_positions[[counter]] <- tail_pos
  counter <- counter + 1
}  
print(length(unique(visited_tail_positions)))

##############
#PART 2

positions <- list('1' = c(0,0), '2' = c(0,0), '3' = c(0,0),  '4' = c(0,0),
                  '5' = c(0,0), '6' = c(0,0), '7' = c(0,0),  '8' = c(0,0),
                  '9' = c(0,0),  '10' = c(0,0))
visited_10_positions <- list()
counter_1 <- 1
head_position <- positions[['1']]
for (i in head_moves) {
  head_position <- move_head(head_pos_in = head_position, direction_ins = i)
  positions[['1']] <- head_position
  for (j in 2:10){
    positions[[as.character(j)]] <- move_tail(head_pos_in =  positions[[as.character(j-1)]] , tail_pos_in = positions[[as.character(j)]])
  }
  visited_10_positions[[counter_1]] <- positions[['10']]
  counter_1 <- counter_1 + 1
}  
print(length(unique(visited_10_positions)))