#day 7
setwd(here::here())
source("utils.R")
setwd(paste(here::here('2022')))

browse <- trimws(read_lines('day7/day7_input.txt'))

directory = list()
visited_dirs = c()
current_dire = c()

commands <- which(1 == purrr::map(browse, ~grep('\\$', .)))

perform_cd <- function(command_ins, current_dir = current_dire, browse_ins = browse) {
  if(length(grep('\\$ cd', command_ins)) == 1) {
    cd_flag <- 1
  } else {
    cd_flag <- 0
  }
  
  if (cd_flag == 1) {
    cd_dir <- substr(command_ins, 6, nchar(command_ins))
    if (cd_dir != '..') {
        current_dir <- c(current_dir, cd_dir)
    } else {
        current_dir <- current_dir[1 : length(current_dir) - 1]
      }
  } 
  
  return(current_dir = current_dir)
}

capture_ls <- function(ls_ins, line_no_ins, browse_ins = browse, commands_ins = commands) {
  if(length(grep('\\$ ls', ls_ins)) == 1) {
    ls_flag <- 1
  } else {
    ls_flag <- 0
  }
  
  if (ls_flag == 1) {
    if (line_no_ins == commands_ins[length(commands_ins)]) {
      ls_entries <- (browse_ins[(line_no_ins + 1) : length(browse_ins)])
    } else {
      ls_entries <- (browse_ins[(line_no_ins + 1) : (commands_ins[which(commands_ins == line_no_ins) + 1] - 1)])
    }
    
    return(ls_entries)
  }
} 
  
clean_ls_capture <- function(ls_capture_ins){
  data_size <- regmatches(ls_capture_ins, gregexpr("[[:digit:]]+", ls_capture_ins))[[1]]
  if (length(data_size) !=0) {
    return(as.numeric(data_size))
  } else {
    return(ls_capture_ins)
  }
}

dir_store <- list('/' = list())
for (line_no in commands) {
  line = browse[line_no]
  current_dire <- perform_cd(command_ins = line, current_dir = current_dire, browse_ins = browse)
  ls_captures <- (capture_ls(ls_ins = line, line_no_ins =  line_no))
  if (!is.null(ls_captures)){
    #clean ls captures
    ls_captures <- unlist(purrr::map(ls_captures, ~clean_ls_capture(.)))
    dir_store_add <- list(ls_captures)
    dir_store[[current_dire]] <- dir_store_add
  }
  }

#replace dir name with it's size
#function(dir_name, dir_size, input_list)

#top level of dir
#dir_store <- dir_store[['/']]

no_level <- collapse::ldepth(dir_store)
hhh <- c("/"     ,  "hfm"    , "hfm"   ,  "fst"   ,  "nblfzrb" ,"cqjmmr"  ,"hfm")
interrogate_sub_dirs <- function(sub_dir_store_ins, dir_store_dive){
  sub_dirs <- (sub_dir_store_ins)
  sub_dirs_sums <- c()
  #for (jj in sub_dirs) {
  #  sub_dirs <- unique(c(sub_dirs, purrr::map((remove_element_from_vector(names(dir_store_dive[[jj]]), "")), ~c(jj,.))))
  #  sub_dir_sums <- (purrr::map(sub_dirs, ~(sum(as.integer(dir_store_dive[[.]][[1]])))))
  #}
  for (j in sub_dirs) {
    sub_dirs <- unique(c(sub_dirs, purrr::map((remove_element_from_vector(names(dir_store_dive[[j]]), "")), ~c(j,.))))
    sub_dir_sums <- (purrr::map(sub_dirs, ~(sum(as.integer(dir_store_dive[[.]][[1]])))))
    sum_check <- sum(as.integer(dir_store_dive[[j]][[1]]))
    j_chk <- all(j == hhh)
    if (j_chk) {
      print("NOW")
      print(dir_store_dive[[j]][[1]])
    }
    if (!is.na(sum_check)){
      if (paste0('dir ', j[length(j)]) %in% (dir_store_dive[[(j[1:length(j)-1])]][[1]])) {
        #print(j)
      dir_store_dive[[(j[1:length(j)-1])]][[1]] <- c(sum_check, remove_element_from_vector(dir_store_dive[[(j[1:length(j)-1])]][[1]], paste0('dir ', j[length(j)])))
      if (j_chk) {
        print("NOW")
        print(dir_store_dive[[j]][[1]])
      }
      #dir_store_dive[[(j[1:length(j)-1])]][[1]] <- c(sum_check, (dir_store_dive[[(j[1:length(j)-1])]][[1]])[1:(length((dir_store_dive[[(j[1:length(j)-1])]][[1]])) - 1)])
        
        } 
    } else {
        #print(j)
    }
    if (j_chk) {
      print("NOW")
      print(dir_store_dive[[j]][[1]])
    }
    
      }
    

   
  return(list('sub_dirs' = sub_dirs, 'sub_dir_sums' = sub_dir_sums, 'dir_store' = dir_store_dive))
}

sub_dir_store <- c('/')
dir_store_mod <- dir_store
for (i in 1:no_level+100) {
print(i)
 ggg <- interrogate_sub_dirs(sub_dir_store_ins = sub_dir_store, dir_store_dive = dir_store_mod)
 sub_dir_store <- ggg[['sub_dirs']]
 sub_dir_sums <- ggg[['sub_dir_sums']]
 dir_store_mod <- ggg[['dir_store']]

}

for (j in sub_dir_store) {
  sub_dir_store <- unique(c(sub_dir_store, purrr::map((remove_element_from_vector(names(dir_store_mod[[j]]), "")), ~c(j,.))))
  sub_dir_sums <- (purrr::map(sub_dir_store, ~(sum(as.integer(dir_store_mod[[.]][[1]])))))
  sum_check <- sum(as.integer(dir_store_mod[[j]][[1]]))
  j_chk <- all(j == hhh)
  #if (!is.na(sum_check)){
  #  if (paste0('dir ', j[length(j)]) %in% (dir_store_mod[[(j[1:length(j)-1])]][[1]])) {
  #    dir_store_mod[[(j[1:length(j)-1])]][[1]] <- c(sum_check, remove_element_from_vector(dir_store_mod[[(j[1:length(j)-1])]][[1]], paste0('dir ', j[length(j)])))

  #  } 
  #} else {
  #}
  
}


sub_dir_sums <- unlist(sub_dir_sums)

print(sum(unlist(sub_dir_sums[sub_dir_sums <= 100000]), na.rm = TRUE))

#unused space
print(sub_dir_sums[sub_dir_sums == min(sub_dir_sums[sub_dir_sums >= (30000000 - (70000000 - max(sub_dir_sums, na.rm = TRUE)))])])
