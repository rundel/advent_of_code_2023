library(tidyverse)
library(sf)

sf::sf_use_s2(FALSE)

## Part 1

d = read_lines(here::here("day18/test.txt"))
d = read_lines(here::here("day18/input.txt"))

dirs = list(
  L = c(0,-1),
  R = c(0,1),
  U = c(-1,0),
  D = c(1,0)
)


str_match(d, "([LRUD]) (\\d+) \\((#[a-f0-9]{6})\\)")[,-1] |>
  as.data.frame() |>
  set_names(c("dir","steps", "color")) |>
  mutate(
    steps = as.numeric(steps)
  ) |>
  mutate(
    #move = map2(dir, steps, ~ rep(dirs[[.x]], c(.y,.y)) |> matrix(ncol=2) )
    move = map2(dir, steps, ~ dirs[[.x]] * .y )
  ) |>
  pull(move) |>
  do.call(rbind, args = _) |>
  (\(x) rbind(c(0,0), x))() |>
  apply(2, cumsum) |>
  list() |>
  sf::st_polygon() |> 
  sf::st_buffer(dist = 0.5, endCapStyle = "SQUARE", nQuadSegs=1, joinStyle="MITRE", mitreLimit=2) |>
  st_area()


## Part 2

d = read_lines(here::here("day18/test.txt"))
d = read_lines(here::here("day18/input.txt"))


dirs = list(
  R = c(0,1),
  D = c(1,0),
  L = c(0,-1),
  U = c(-1,0)
)

options(digits=22)

str_match(d, "([LRUD]) (\\d+) \\((#[a-f0-9]{6})\\)")[,-1] |>
  as.data.frame() |>
  set_names(c("dir","steps", "color")) |>
  mutate(
    dir = substring(color, 7, 7) |> as.numeric(),
    steps = substring(color, 2, 6) |>
      (\(x) paste0("0x", x))() |> 
      strtoi()
  ) |>
  mutate(
    #move = map2(dir, steps, ~ rep(dirs[[.x+1]], c(.y,.y)) |> matrix(ncol=2) )
    move = map2(dir, steps, ~ dirs[[.x+1]] * .y)
  ) |>
  pull(move) |>
  do.call(rbind, args = _) |>
  (\(x) rbind(c(0,0), x))() |>
  apply(2, cumsum) |>
  list() |>
  sf::st_polygon() |> 
  sf::st_buffer(dist = 0.5, endCapStyle = "SQUARE", nQuadSegs=1, joinStyle="MITRE", mitreLimit=2) |>
  st_area()

