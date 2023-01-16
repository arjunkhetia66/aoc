#day 8
setwd(here::here())
source("utils.R")
setwd(paste(here::here('2022')))
library(Thermimage)

visibility_check <- function(tree_vec) {
  tree_vec_max_running <- unlist(purrr::map(1:length(tree_vec), ~max(tree_vec[1:.])))
  running_max_unique <- (unique(sort(tree_vec_max_running)))
  visible_positions <- unique(unlist((c(1,purrr::map(running_max_unique, ~base::match(.,tree_vec))))))
  return(visible_positions)
}

rotate <- function(x) t(apply(x, 2, rev))

trees <- purrr::map(read_lines("day8/day8_input.txt"), ~as.numeric(strsplit(., "")[[1]]))
forest_height <- length(trees)
forest_length <- length(trees[[1]])

trees_new <- unlist(trees)
tree_matrix <- matrix(trees_new, ncol = forest_height, byrow = TRUE)
tree_index <- matrix(1:forest_height^2, ncol = forest_height, byrow = TRUE)


#left to right viewing
left_tree <- tree_matrix
left_indexes <- tree_index

bottom_tree <- rotate(left_tree)
bottom_indexes <- rotate(left_indexes)

right_tree <- mirror.matrix(left_tree)
right_indexes <- mirror.matrix(left_indexes)

top_tree <- t(left_tree)
top_indexes <- t(left_indexes)

tree_views_and_indexes <- list(left = list(left_tree, left_indexes),
                               bottom = list(bottom_tree, bottom_indexes),
                               right = list(right_tree, right_indexes),
                               top = list(top_tree, top_indexes))

visible_trees <- c()
for (k in names(tree_views_and_indexes)) {
  for (i in 1:forest_height) {
    visible_row_position <- (visibility_check(tree_views_and_indexes[[k]][[1]][i,]))
    visible_trees <- c(visible_trees, (unlist(purrr::map(visible_row_position, ~tree_views_and_indexes[[k]][[2]][i,.]))))
  }
}
  
print(length(unique(visible_trees)))

#part 2
visibility_check_2 <- function(tree_vec, tree_height_ins) {
  visible <- tree_vec < tree_height_ins
  how_many_vis <- match(FALSE, visible)
  if(is.na(how_many_vis)){how_many_vis <- length(tree_vec)}
  return(how_many_vis)
}

how_many_visible <- function(i,j, tree_matrix_ins = tree_matrix, forest_length_ins = forest_length, forest_height_ins = forest_height){
  tree_height <- tree_matrix_ins[i,j]
  if(i == 1){top_view <- c(NULL)}else{top_view <- rev(tree_matrix_ins[1:(i-1),j])}
  if(j == forest_length_ins){right_view <- c(NULL)}else{right_view <- tree_matrix_ins[i,(j+1):forest_length_ins]}
  if(j == 1){left_view <- c(NULL)}else{left_view <- rev(tree_matrix_ins[i,1:(j-1)])}
  if(i == forest_height_ins){bottom_view <- c(NULL)}else{bottom_view <- tree_matrix_ins[(i+1):forest_height_ins,j]}
  score <- prod((visibility_check_2(top_view, tree_height)),
                (visibility_check_2(right_view, tree_height)),
                (visibility_check_2(left_view, tree_height)),
                (visibility_check_2(bottom_view, tree_height)))
  return(score)
}

score <- c()
for (i in 1:forest_height) {
  for (j in 1:forest_length) {
    score <- c(score, how_many_visible(i, j))
  }
}

print(max(score))
