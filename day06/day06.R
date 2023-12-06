library(tidyverse)

## Part 1

d = read_lines(here::here("day06/test.txt")) 
d = read_lines(here::here("day06/input.txt"))

d|>
  str_replace_all(" +", " ") |>
  str_remove(".*: ") |>
  str_split(" ") |>
  set_names(c("time", "dist")) |>
  do.call(cbind, args = _) |>
  apply(2, as.numeric) |>
  as_tibble() |>
  mutate(
    wait = map(time, ~ seq(0,.x)),
    moved = map2(time, wait, ~ (.x - .y) * .y),
    win = map2(moved, dist, ~ .x > .y ),
    n_win = map_int(win, sum)
  ) |>
  pull(n_win) |>
  prod()


## Part 2

d = read_lines(here::here("day06/test.txt"))
d = read_lines(here::here("day06/input.txt"))

d = d|>
  str_replace_all(" +", "") |>
  str_remove_all(".*:") |>
  str_split(" ") |>
  set_names(c("time", "dist")) |>
  do.call(cbind, args = _) |>
  apply(2, as.numeric) |>
  c()

z=(d[1] - seq(0,d[1])) * seq(0,d[1])
sum(z>d[2])
