library(tidyverse)

# I'm on a plane (and make poor life choices)

## Part 1

d = read_lines(here::here("day14/test.txt"))
d = read_lines(here::here("day14/input.txt"))

m = d |>
  str_split("") |>
  do.call(rbind, args = _)

for (r in seq_len(nrow(m))[-1]) {
  for (c in seq_len(ncol(m))) {
    if (m[r,c] == "O") {
      new_r = max(which(m[seq_len(r-1),c] != ".")+1, 1)
      m[new_r, c] = "O"
      if (new_r != r)
        m[r,c] = "."
    }
  }
}
m

sum(nrow(m)+1-which(m == "O", arr.ind = TRUE)[,1])


## Part 2



rotate = function(x) t(apply(x, 2, rev))

north = function(m) {
  for (r in seq_len(nrow(m))[-1]) {
    for (c in seq_len(ncol(m))) {
      if (m[r,c] == "O") {
        new_r = max(which(m[seq_len(r-1),c] != ".")+1, 1)
        m[new_r, c] = "O"
        if (new_r != r)
          m[r,c] = "."
      }
    }
  }
  
  m
}

west = function(m) {
  rotate(m) |>
    north() |>
    rotate() |>
    rotate() |>
    rotate()
}

south = function(m) {
  rotate(m) |>
    rotate() |>
    north() |>
    rotate() |>
    rotate()
}

east = function(m) {
  rotate(m) |>
    rotate() |>
    rotate() |>
    north() |>
    rotate()
}

spin = function(m) {
  m |>
    north() |>
    west() |>
    south() |>
    east()
}

load = function(m) {
  sum(nrow(m)+1-which(m == "O", arr.ind = TRUE)[,1])
}

d = read_lines(here::here("day14/test.txt"))
d = read_lines(here::here("day14/input.txt"))

m = d |>
  str_split("") |>
  do.call(rbind, args = _)

#spin(m) |> spin()

res = paste(m, collapse = "")


for (i in 2:1e4) {
  m = spin(m)
  new = paste(m, collapse = "")
  
  if (new %in% res) {
    start = which(new == res)
    end = i
    
    break
  }
  
  res[i] = new
}

cycle_len = end-start

val = 1000000000

res[(val+1 - start) %% cycle_len + start] |>
  str_split("") |>
  (\(x) x[[1]])() |>
  matrix(nrow(m),ncol(m)) |>
  load()

