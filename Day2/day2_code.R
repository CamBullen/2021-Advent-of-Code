library(tidyverse)
library(here)

data <- tibble(read.table(here("Day2", "day2_input.txt")))


# part 1
unique(data$V1)
data %>%
    mutate(direction = if_else(V1 %in% c("forward"), "H", "V"),
           V2 = as.double(V2),
           V2 = if_else(V1 == "up", V2*-1, V2)) %>%
    group_by(direction) %>%
    summarise(total = sum(V2))
1032*2052

# part 2
data %>%
    mutate(direction = if_else(V1 %in% c("forward"), "H", "V"),
           V2 = as.double(V2),
           V2 = if_else(V1 == "up", V2*-1, V2),
           aim_only = if_else(direction == "H", 0, V2),
           H_only = if_else(direction == "H", V2, 0),
           current_aim = cumsum(aim_only),
           depth = H_only*current_aim,
           cum_H = cumsum(H_only),
           cum_depth = cumsum(depth)) %>%
    slice_tail()
2052*1010437
