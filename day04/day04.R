library(tidyverse)

## Part 1

d = read_lines(here::here("day04/test.txt"))
d = read_lines(here::here("day04/input.txt"))

d |>
  str_remove("Card +\\d+: ") |>
  str_replace_all("  ", " ") |>
  str_trim() |>
  str_split(" \\| ") |>
  map_dfr(
    ~ tibble(x = .x[[1]] |> str_split(" ") |> unlist()  |> list(), 
             y = .x[[2]] |> str_split(" ") |> unlist()  |> list())
  ) |> 
  pmap_dbl(
    function(x,y) {
      sum(y %in% x)
    }
  ) %>%
  {ifelse(. == 0, 0, 2^(.-1))} |>
  sum()
  


## Part 2

d = read_lines(here::here("day04/test.txt"))
d = read_lines(here::here("day04/input.txt"))

matches = d |>
  str_remove("Card +\\d+: ") |>
  str_replace_all("  ", " ") |>
  str_trim() |>
  str_split(" \\| ") |>
  map_dfr(
    ~ tibble(x = .x[[1]] |> str_split(" ") |> unlist()  |> list(), 
             y = .x[[2]] |> str_split(" ") |> unlist()  |> list())
  ) |> 
  pmap_dbl(
    function(x,y) {
      sum(y %in% x)
    }
  )

copies = rep(1, length(matches))

for(i in seq_along(copies)) {
  copies[i + seq_len(matches[i])] = copies[i + seq_len(matches[i])] + copies[i]
}

sum(copies)
