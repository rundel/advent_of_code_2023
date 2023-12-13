library(tidyverse)

## Part 1

d = read_file(here::here("day13/test.txt"))
d = read_file(here::here("day13/input.txt"))

ml = str_split(d, "\n\n")[[1]] |>
  str_split("\n") |>
  map(
    ~ str_split(.x,"") |>
      do.call(rbind, args = _)
  )

find_vert_reflect = function(m) {
  n = nrow(m)
  
  ctrs = map2_lgl(
    seq_len(n)[-1],
    lag(seq_len(n))[-1],
    ~identical(m[.x,], m[.y,])
  ) |>
    which()
  
  for(ctr in ctrs) {
    i = ctr
    j = ctr + 1
    
    ok = TRUE
    while(i > 0 & j < n+1) {
      if (!identical(m[i,], m[j,])) {
        ok = FALSE
        break
      }
      
      i = i-1
      j = j+1
    }
    
    if (ok) {
      return(ctr)
    }
  }
  
  return(0)
}

find_horz_reflect = function(m) {
  find_vert_reflect(t(m))
}

v = map_dbl(ml, find_vert_reflect)
h = map_dbl(ml, find_horz_reflect)

tibble(v,h) |> 
  View()

100 * sum(v) + sum(h)



## Part 2

d = read_file(here::here("day13/test.txt"))
d = read_file(here::here("day13/input.txt"))

ml = str_split(d, "\n\n")[[1]] |>
  str_split("\n") |>
  map(
    ~ str_split(.x,"") |>
      do.call(rbind, args = _)
  )

find_vert_reflect = function(m) {
  n = nrow(m)
  
  diffs = map2_dbl(
    seq_len(n)[-1],
    lag(seq_len(n))[-1],
    ~length(m[1,]) - sum(m[.x,] ==  m[.y,])
  ) 
  
  ctrs = which(diffs %in% c(0,1))
  
  if (length(ctrs) == 0) {
    return(0)
  }
  diffs = diffs[ctrs]
  
  for(k in seq_along(ctrs)) {
    diff = 0
    i = ctrs[k]
    j = ctrs[k] + 1
    
    ok = TRUE
    while(i > 0 & j < n+1) {
      diff = diff + length(m[1,]) - sum(m[i,] ==  m[j,])
      if (diff > 1) {
        ok = FALSE
        break
      }
      
      i = i-1
      j = j+1
    }
    
    if (ok & diff == 1) {
      return(ctrs[k])
    }
  }
  
  return(0)
}

find_horz_reflect = function(m) {
  find_vert_reflect(t(m))
}

v = map_dbl(ml, find_vert_reflect)
h = map_dbl(ml, find_horz_reflect)

tibble(v,h) |> 
  View()

100 * sum(v) + sum(h)

