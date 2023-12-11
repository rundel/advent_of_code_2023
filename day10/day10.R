library(tidyverse)
library(igraph)

pipe_chr = c("|", "-", "L", "J", "7", "F")

pipe = list(
  "|" = list(c(1,0),c(-1,0)), 
  "-" = list(c(0,1), c(0,-1)),
  "L" = list(c(-1,0), c(0,1)), 
  "J" = list(c(-1,0), c(0,-1)), 
  "7" = list(c(1,0), c(0,-1)), 
  "F" = list(c(1,0), c(0,1))
)

## Part 1

d = read_lines(here::here("day10/test.txt"))
d = read_lines(here::here("day10/test2.txt"))
d = read_lines(here::here("day10/input.txt"))

m = d |>
  str_split("") |>
  do.call(rbind, args = _)

res = list()
for (r in seq_len(nrow(m))) {
  for (c in seq_len(ncol(m))) {
    if (m[r,c] %in% pipe_chr) {
      res[[length(res)+1]] = 
        pipe[[ m[r,c] ]] |>
          map_dfr(
            function(x) {
              tibble( 
                from = paste(c(r,c), collapse=", "),
                to   = paste(c(r,c) + x, collapse=", ")
              )
            }
          )
    }
  }
}

res2 = bind_rows(res) 
res3 = graph_from_data_frame(res2, directed = TRUE)
res4 = graph_from_data_frame(res2, directed = FALSE)

st = which(m == "S", arr.ind = TRUE) |> paste(collapse=", ")
nb = neighbors(res4, v=st) |> names()

igraph::shortest_paths(res3, from = nb[1], to = nb[2])$vpath[[1]] |> length() |> (\(x) (x+1)/2)()




## Part 2

d = read_lines(here::here("day10/test3.txt"))
d = read_lines(here::here("day10/test4.txt"))
d = read_lines(here::here("day10/test5.txt"))
d = read_lines(here::here("day10/input.txt"))


m = d |>
  str_split("") |>
  do.call(rbind, args = _)

res = list()
for (r in seq_len(nrow(m))) {
  for (c in seq_len(ncol(m))) {
    if (m[r,c] %in% pipe_chr) {
      res[[length(res)+1]] = 
        pipe[[ m[r,c] ]] |>
        map_dfr(
          function(x) {
            tibble( 
              from = paste(c(r,c), collapse=", "),
              to   = paste(c(r,c) + x, collapse=", ")
            )
          }
        )
    }
  }
}

res2 = bind_rows(res) 
res3 = graph_from_data_frame(res2, directed = TRUE)
res4 = graph_from_data_frame(res2, directed = FALSE)

st = which(m == "S", arr.ind = TRUE) |> paste(collapse=", ")
nb = neighbors(res4, v=st) |> names()

short = igraph::shortest_paths(res3, from = nb[1], to = nb[2])$vpath[[1]] |> names()

other = setdiff(V(res4) |> names(), c(short, st)) |>
  str_split(", ") |>
  map(as.integer) |>
  do.call(rbind, args = _) %>%
  rbind(
    ., 
    which(m == ".", arr.ind = TRUE) # Vertices doesn't have non-pipe locations
  ) 

path = c(st, short, st) |>
  str_split(", ") |>
  map(as.integer) |>
  do.call(rbind, args = _)

p1 = sf::st_polygon(list(path)) |> sf::st_sfc() |> sf::st_buffer(dist = 0.5, endCapStyle = "SQUARE")

p2 = other |>
  sf::st_multipoint() |> sf::st_sfc() |> sf::st_buffer(dist = 0.5, endCapStyle = "SQUARE")

sf::st_intersection(p1,p2) |>
  sf::st_area()


