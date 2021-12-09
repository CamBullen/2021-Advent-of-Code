library(tidyverse)
library(here)
options(scipen = 999)


data <- tibble(read.table(here("Day3", "day3_input.txt"), colClasses = "character"))

data <- data %>%
    separate(col = V1, into = c(paste0("X", rep(1:12))), sep = rep(1:11), remove = FALSE) %>%
    mutate(dummy_ID = row_number())

# Part 1
tally_data <- data %>%
    pivot_longer(cols = X1:X12) %>%
    mutate(name = factor(name, levels = c(paste0("X", rep(1:12))))) %>%
    group_by(name) %>%
    count(value) 

g <- tally_data %>%
    slice_max(n)%>%
    select(-n) %>%
    ungroup() %>%
    pivot_wider(names_from = name, values_from = value) %>%
    unite(col = "gamma", X1:X12, sep = "")
g_out <- strtoi(g$gamma, base = 2)


e <- tally_data %>%
    slice_min(n)%>%
    select(-n) %>%
    ungroup() %>%
    pivot_wider(names_from = name, values_from = value) %>%
    unite(col = "epsilon", X1:X12, sep = "")
e_out <- strtoi(e$epsilon, base = 2)

e_out*g_out

# Part 2
data2 <- data %>% 
    select(-V1, - dummy_ID)%>%
    mutate(across(everything(), as.numeric))

for(i in 1:ncol(data2)) {
ids <- data2[, i]
n <- colSums(ids) >= nrow(ids)/2 # returns TRUE if 1 most common, FALSE if 0 most common
data2 <- data2[ids == n,] 
if(nrow(data2) == 1) break
}
oxygen <- data2

for(i in 1:ncol(data2)) {
    ids <- data2[, i]
    n <- colSums(ids) < nrow(ids)/2 # returns TRUE if 1 least common, FALSE if 0 least common
    data2 <- data2[ids == n,] 
    if(nrow(data2) == 1) break
}
CO2 <- data2

oxygen_out <- unite(oxygen, col = "out", sep = "") %>% pull(out) %>% strtoi(base =2)
CO2_out <- unite(CO2, col = "out", sep = "") %>% pull(out) %>% strtoi(base =2)

oxygen_out * CO2_out
