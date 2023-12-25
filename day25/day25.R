library(tidyverse)

## Part 1

d = read_lines(here::here("day25/test.txt"))
d = read_lines(here::here("day25/input.txt"))

z = d |>
  str_split(": ")

g = tibble(
  from = map(z, 1) |> unlist(),
  to = map(z,2) |> str_split(" ")
) |>
  unnest_longer(to) |>
  igraph::graph_from_data_frame(directed = FALSE)

id = igraph::tkplot(g)
z = igraph::tk_coords(id)

sum(z[,1] < 100) * sum(z[,1] > 100)
