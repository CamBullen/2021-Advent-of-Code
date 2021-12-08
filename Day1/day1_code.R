library(tidyverse)
library(here)

data <- read_table(here("Day1", "input.txt"),
                   col_names = FALSE)

## Part 1
    # solved without googling, and then googled to solve using dplyr::lag()
data$prev <- c(NA, data$X1)[1:2000]

data
data$pre
data %>%
    mutate(direction = if_else(X1>prev, "increasing", "decreasing"),
           direction2 = if_else(X1>lag(X1), "increasing", "decreasing")) %>%
    count(direction, direction2)

#part 2
data %>%
    mutate(rolling_avg = X1+lag(X1)+lag(X1, n = 2),
           direction = if_else(rolling_avg > lag(rolling_avg), "increasing", "decreasing")) %>% View()
    count(direction)



