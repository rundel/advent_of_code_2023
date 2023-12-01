library(tidyverse)

## Part 1

d = read_lines(here::here("day01/test.txt"))
d = read_lines(here::here("day01/input.txt"))

x = str_match(d, "[^0-9]*(\\d)")[,2]
y = str_match(d, "(\\d)[^0-9]*$")[,2]

paste0(x,y) |>
  as.numeric() |>
  sum()


## Part 2

to_num = function(x) {
  case_when(
    x == "one" ~ 1,
    x == "two" ~ 2,
    x == "three" ~ 3,
    x == "four" ~ 4,
    x == "five" ~ 5,
    x == "six" ~ 6,
    x == "seven" ~ 7,
    x == "eight" ~ 8,
    x == "nine" ~ 9,
    TRUE ~ as.numeric(x)
  )
}

d = read_lines(here::here("day01/test2.txt"))
d = read_lines(here::here("day01/input.txt"))

x = str_match(d, ".*?(one|two|three|four|five|six|seven|eight|nine|\\d)")[,2] |>
  to_num()
y = str_match(d, ".*(one|two|three|four|five|six|seven|eight|nine|\\d).*?$")[,2] |>
  to_num()

paste0(x,y) |>
  as.numeric() |>
  sum()
