#day 10
setwd(here::here())
source("utils.R")
setwd(paste(here::here('2022')))

program <- read_lines("day10/day10_input.txt")
counter <- 1
for (i in program) {
  if((substr(i,1,4) == 'addx')) {
    program <- append(program, 'noop', after = counter-1)
    counter <- counter + 1
  }
counter <- counter + 1
}

cycle <- 1
x <- 1
signal <- 0
registers <- c(x)
for (i in 1:length(program)){
  program_item <- program[i]
  if ((cycle == 20) | ((cycle - 20) %% 40 == 0)) {
    signal <- signal + (cycle * x)
  }
  if (program_item == 'noop') {
    cycle <- cycle + 1
  } else if (substr(program_item,1,4) == 'addx') {
    add_on <- as.integer(substr(program_item,6,nchar(program_item)))
    x <- x + add_on
    cycle <- cycle + 1
  }
  registers <- c(registers, x)
}

print(signal)

#part 2
screen <- purrr::map(1:6, ~rep(0,40))
split_register <- purrr::map(1:6, ~((. - 1)*40) + 1:40)

for (j in 1:6){ 
  registers_2 <- registers[split_register[[j]]]
  for (i in 1:(length(screen[[j]]))) {
    pixels <- (registers_2[i]):(registers_2[i] + 2)
    if (i %in% pixels) {
      screen[[j]][i] <- 1
    }
  }
}

library('plot.matrix')
plot(matrix(unlist(screen),nrow = 6, byrow = TRUE))
