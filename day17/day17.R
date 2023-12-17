library(tidyverse)

## Part 1

d = read_lines(here::here("day17/test.txt"))
d = read_lines(here::here("day17/input.txt"))

m = d |>
  str_split("") |>
  map(as.numeric) |>
  do.call(rbind, args = _)

dirs = list(N=c(-1, 0), E=c(0, 1), W=c(0, -1), S=c(1, 0))
opp_dir = c(4,3,2,1,5)

posb = tibble(
  cost = 0, 
  x = 1, 
  y = 1, 
  prev_dir = 5
)

seen = array(FALSE, dim=c(nrow(m), ncol(m), 5))
costs = array(1e50, dim=c(nrow(m), ncol(m), 5))

while (nrow(posb)) {
  cur = posb |> slice(1)
  posb = posb |> slice(-1)
  
  if (cur$x == nrow(m) & cur$y == ncol(m)) {
    print(cur$cost)
    break
  }
  
  if (seen[cur$x, cur$y, cur$prev_dir])
    next
  
  seen[cur$x, cur$y, cur$prev_dir] = TRUE

  for (dir in seq_along(dirs)) {
    cost_inc = 0
    if (dir == cur$prev_dir | dir == opp_dir[cur$prev_dir])
      next
    
    for (dist in 1:3) {
      x = cur$x + dirs[[dir]][1] * dist
      y = cur$y + dirs[[dir]][2] * dist
      if (!x %in% seq_len(nrow(m)) | !y %in% seq_len(ncol(m)))
        break
      
      cost_inc = cost_inc + m[x,y]
      new_cost = cur$cost + cost_inc
      if (costs[x, y, dir] <= new_cost)
        next
      costs[x, y, dir] = new_cost
      src[x, y, ] = c(cur$x,cur$y)
      
      posb = bind_rows(
        posb,
        list(cost = new_cost, x = x, y = y, prev_dir = dir)
      )
    }
  }
  
  posb = posb |> arrange(cost, x, y)
}



## Part 2

d = read_lines(here::here("day17/test.txt"))
d = read_lines(here::here("day17/test2.txt"))
d = read_lines(here::here("day17/input.txt"))

m = d |>
  str_split("") |>
  map(as.numeric) |>
  do.call(rbind, args = _)

dirs = list(N=c(-1, 0), E=c(0, 1), W=c(0, -1), S=c(1, 0))
opp_dir = c(4,3,2,1,5)

posb = tibble(
  cost = 0, 
  x = 1, 
  y = 1, 
  prev_dir = 5
)

seen = array(FALSE, dim=c(nrow(m), ncol(m), 5))
costs = array(1e50, dim=c(nrow(m), ncol(m), 5))

while (nrow(posb)) {
  cur = posb |> slice(1)
  posb = posb |> slice(-1)
  
  if (cur$x == nrow(m) & cur$y == ncol(m)) {
    print(cur$cost)
    break
  }
  
  if (seen[cur$x, cur$y, cur$prev_dir])
    next
  
  seen[cur$x, cur$y, cur$prev_dir] = TRUE
  
  for (dir in seq_along(dirs)) {
    cost_inc = 0
    
    if (dir == cur$prev_dir | dir == opp_dir[cur$prev_dir])
      next
    
    for (dist in 1:10) {
      x = cur$x + dirs[[dir]][1] * dist
      y = cur$y + dirs[[dir]][2] * dist
      if (!x %in% seq_len(nrow(m)) | !y %in% seq_len(ncol(m)))
        break
      
      cost_inc = cost_inc + m[x,y]
      if (dist < 4)
        next
      
      new_cost = cur$cost + cost_inc
      if (costs[x, y, dir] <= new_cost)
        next
      
      costs[x, y, dir] = new_cost
      posb = bind_rows(
        posb,
        list(cost = new_cost, x = x, y = y, prev_dir = dir)
      )
    }
  }
  
  posb = posb |> arrange(cost, x, y)
}
