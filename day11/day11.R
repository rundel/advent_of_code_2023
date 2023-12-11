library(tidyverse)


## Part 1

d = read_lines(here::here("day11/test.txt"))
d = read_lines(here::here("day11/input.txt"))

m = d |>
  str_split("") |>
  map(
    function(x) {
      if (all(x == ".")) {
        rbind(x,x)
      } else {
        x
      }
    }
  ) |>
  do.call(rbind, args = _) |>
  apply(2, list) |>
  map(
    function(x) {
      x = unlist(x)
      if (all(x == ".")) {
        cbind(x,x)
      } else {
        x
      }
    }
  ) |>
  do.call(cbind, args = _)

p = which(m == "#", arr.ind = TRUE) |>
  as.data.frame() |>
  mutate(
    i = row_number()
  )

expand_grid(
  i1 = seq_len(nrow(p)),
  i2 = seq_len(nrow(p))
) |>
  filter(
    i1 < i2
  ) |>
  left_join(
    p |> set_names(c("r1","c1","i")),
    by = c("i1" = "i")
  ) |>
  left_join(
    p |> set_names(c("r2","c2","i")),
    by = c("i2" = "i")
  ) |>
  mutate(
    d = abs(r1 - r2) + abs(c1 - c2)
  ) |> 
  pull(d) |>
  sum()



## Part 2

library(igraph)

d = read_lines(here::here("day11/test.txt"))
d = read_lines(here::here("day11/input.txt"))

m = d |>
  str_split("") |>
  do.call(rbind, args = _)



lr = apply(m, 1, function(x) all(x == ".")) |> which()
lc = apply(m, 2, function(x) all(x == ".")) |> which()


crd = which(!is.na(m), arr.ind = TRUE) |>
  as.data.frame() |>
  mutate(
    i = row_number(),
    val = c(m)
  )

n = 1e6

g_df = igraph::make_lattice(dim(m)) |>
  igraph::as_data_frame() |>
  left_join(
    crd, by = c("to"="i")
  ) |>
  mutate(
    weight = case_when(
      row %in% lr & col %in% lc ~ 2*n,
      row %in% lr ~ n,
      col %in% lc ~ n,
      TRUE ~ 1
    )
  )

g = g_df |>
  select(-row, -col) |>
  igraph::graph_from_data_frame()

distances(
  g,
  v = which(m == "#"),
  to = which(m == "#"),
  weights = g_df$weight
) |>
  sum() |>
  (\(x) x/2)()
