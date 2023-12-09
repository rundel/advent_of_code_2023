library(tidyverse)

## Part 1

d = read_lines(here::here("day09/test.txt"))
d = read_lines(here::here("day09/input.txt"))

z = d |>
  str_split(" ") |>
  map(as.numeric)

tot = 0
for(i in seq_along(z)) {
  
  res = list(z[[i]])
  repeat {
    res[[length(res)+1]] = diff(res[[length(res)]])
    
    if (sum(res[[length(res)]]) == 0) 
      break
  }
  
  tot = tot + map_dbl(res, ~ .x[length(.x)]) |> sum()
}
tot



## Part 2

d = read_lines(here::here("day09/test.txt"))
d = read_lines(here::here("day09/input.txt"))

z = d |>
  str_split(" ") |>
  map(as.numeric)

tot = 0
for(i in seq_along(z)) {
  
  res = list(z[[i]])
  repeat {
    res[[length(res)+1]] = diff(res[[length(res)]])
    
    if (sum(res[[length(res)]]) == 0) 
      break
  }
  v = map_dbl(res, ~ .x[1])
  cur = 0
  for (x in rev(v)[-1]) {
    cur = x - cur
  }
  
  tot = tot + cur
}
tot
