library(tidyverse)
library(here)
options(scipen = 999)

numbers <- as.numeric(read.csv(here("Day4", "day4_input.txt"),
                               colClasses = "character",
                               nrow = 1,
                               header = FALSE))
data <- tibble(read.table(here("Day4", "day4_input.txt"), colClasses = "character", skip = 1))

# part 1
data %>%
    mutate(card_number = rep(1:500, each = 5, length.out = nrow(.)))

rep