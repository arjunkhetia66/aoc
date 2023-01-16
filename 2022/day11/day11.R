#day 11
setwd(here::here())
source("utils.R")
setwd(paste(here::here('2022')))

#read in and transform data
monkeys <- readr::read_lines('day11/day11_test_input.txt')
no_monkeys <- (length(monkeys) + 1) / 7
monkey_business <- list()

for (i in 1:(no_monkeys)){
  items <- monkeys[7*i - 5]
  monkey_business[[as.character(i-1)]][['items']] <- as.integer(unlist(strsplit(substr(items, 19, nchar(items)), ", ")))
  
  operation <- monkeys[7*i - 4]
  monkey_business[[as.character(i-1)]][['operation']] <- substr(operation, 24, nchar(operation))
  
  test <- monkeys[7*i - 3]
  monkey_business[[as.character(i-1)]][['test_divide']] <- as.integer(substr(test, 22, nchar(test)))
  
  throw_to_true <- monkeys[7*i - 2]
  monkey_business[[as.character(i-1)]][['throw_to_true']] <- substr(throw_to_true, 30, nchar(throw_to_true))
  
  throw_to_false <- monkeys[7*i - 1]
  monkey_business[[as.character(i-1)]][['throw_to_false']] <- substr(throw_to_false, 31, nchar(throw_to_false))
  
  monkey_business[[as.character(i-1)]][['inspections']] <- 0
}
monkey_business_1 <- monkey_business

operation_handler <- function(ins, operation_ins) {
  
  operator <- substr(operation_ins,1,1)
  operand <- substr(operation_ins,3, nchar(operation_ins))
  output <- case_when(
    operator == '+' ~ ins + as.numeric(operand),
    operator == '-' ~ ins - as.numeric(operand),
    operator == '*' & operand != 'old' ~ ins * as.numeric(operand),
    operator == '*' & operand == 'old' ~ ins^2,
    TRUE ~ as.numeric(ins)
    )
 
  return(output)
}

mod_all <- prod(unlist(purrr::map(names(monkey_business), ~monkey_business[[.]][['test_divide']])))

execute_monkey_business <- function(monkey_input, part_ins) {
  for (k in names(monkey_input)) {
    #temp <- monkey_input[[k]][['items']]
    #monkey_input[[k]][['items']] <- temp[!is.na(temp)]
    i <- monkey_input[[k]]
    if (length(i[['items']]) > 0) {
      monkey_input[[k]][['inspections']] <- monkey_input[[k]][['inspections']] + length(i[['items']]) 
      if (part_ins == 1) {
        worry_level <- (floor(unlist(purrr::map(i[['items']], ~ operation_handler(.,i[['operation']]))) / 3))
      } else if (part_ins == 2) {
        worry_level <- (unlist(purrr::map(i[['items']], ~ operation_handler(.,i[['operation']])))) %% mod_all
      }
      tested <- worry_level %% i[['test_divide']] == 0
      thrown_to <- unlist(purrr::map(tested, ~ (if (.) {i[['throw_to_true']]} else {i[['throw_to_false']]})))
      for (j in 1:length(thrown_to)) {
        monkey_input[[thrown_to[j]]][['items']] <- c(monkey_input[[thrown_to[j]]][['items']], worry_level[j])
      }
      monkey_input[[k]][['items']] <- c()
    }
  }
  
  return(monkey_input)
}

#part 1
for (ii in 1:20) {
  #print(ii)
  monkey_business <- execute_monkey_business(monkey_business, 1)
}

inspection_numbers <- sort(unlist(purrr::map(names(monkey_business), ~monkey_business[[.]][['inspections']])))
print(prod(inspection_numbers[(length(inspection_numbers) - 1) : length(inspection_numbers)]))

#part 2

for (ii in 1:10000) {
  #print(ii)
  monkey_business_1 <- execute_monkey_business(monkey_business_1, 2)
}

inspection_numbers_2 <- sort(unlist(purrr::map(names(monkey_business_1), ~monkey_business_1[[.]][['inspections']])))
print(prod(inspection_numbers_2[(length(inspection_numbers_2) - 1) : length(inspection_numbers_2)]))

      