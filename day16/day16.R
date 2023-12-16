library(tidyverse)

## Part 1

d = read_lines(here::here("day16/test.txt"))
d = read_lines(here::here("day16/input.txt"))

m = d |>
  str_split("") |>
  do.call(rbind, args = _)

visit = m
visit[TRUE] = "."

usable_split = matrix(m %in% c("-","|"), nrow = nrow(m), ncol = ncol(m))

dir = list(c(0,1))
cur = list(c(1,0)) 

prev = character()

repeat {
  if (length(dir) == 0)
    break
      
  for (i in seq_along(dir)) {
    if (is.null(cur[i][[1]]))
      next
    
    cur[[i]] = cur[[i]] + dir[[i]]
    
    if (cur[[i]][1] < 1 | cur[[i]][1] > nrow(m) | cur[[i]][2] < 1 | cur[[i]][2] > ncol(m)) {
      cur[[i]] = c()
      dir[[i]] = c()
      next
    }
    
    visit[cur[[i]][1], cur[[i]][2]] = "#"
    
    if (m[cur[[i]][1], cur[[i]][2]] == "|" & ((dir[[i]][1] == 0 & dir[[i]][2] == 1) | (dir[[i]][1] == 0 & dir[[i]][2] == -1))) {
      
      if (usable_split[cur[[i]][1], cur[[i]][2]]) {
        dir[[i]] = c(1,0)
        dir[[length(dir) + 1]] = c(-1,0)
        cur[[length(dir)]] = cur[[i]]
        usable_split[cur[[i]][1], cur[[i]][2]] = FALSE
      } else {
        cur[[i]] = c()
        dir[[i]] = c()
        next
      }
      
    } 
    
    if (m[cur[[i]][1], cur[[i]][2]] == "-" & ((dir[[i]][1] == 1 & dir[[i]][2] == 0) | (dir[[i]][1] == -1 & dir[[i]][2] == 0))) {
      if (usable_split[cur[[i]][1], cur[[i]][2]]) {
        dir[[i]] = c(0,1)
        dir[[length(dir) + 1]] = c(0,-1)
        cur[[length(dir)]] = cur[[i]]
        usable_split[cur[[i]][1], cur[[i]][2]] = FALSE
      } else {
        cur[[i]] = c()
        dir[[i]] = c()
        next
      }
    }
    
    if (m[ cur[[i]][1], cur[[i]][2] ] == "\\") {
      if (       dir[[i]][1] == 0  & dir[[i]][2] == 1) {
        dir[[i]] = c(1,0)
      } else if (dir[[i]][1] ==  0 & dir[[i]][2] == -1) {
        dir[[i]] = c(-1,0)
      } else if (dir[[i]][1] ==  1 & dir[[i]][2] == 0) {
        dir[[i]] = c(0,1)
      } else if (dir[[i]][1] == -1 & dir[[i]][2] == 0) {
        dir[[i]] = c(0,-1)
      } 
    } 
    
    if (m[ cur[[i]][1], cur[[i]][2] ] == "/") {
      if (       dir[[i]][1] == 0  & dir[[i]][2] == -1) {
        dir[[i]] = c(1,0)
      } else if (dir[[i]][1] ==  0 & dir[[i]][2] == 1) {
        dir[[i]] = c(-1,0)
      } else if (dir[[i]][1] ==  1 & dir[[i]][2] == 0) {
        dir[[i]] = c(0,-1)
      } else if (dir[[i]][1] == -1 & dir[[i]][2] == 0) {
        dir[[i]] = c(0,1)
      } 
    }
    
  }
  
  state = sum(visit == "#")
  prev = c(state, prev)
  
  if (length(prev) > 3 && sum(duplicated(prev[1:3])) == 2)
    break
  
  #print(visit)
}

sum(visit == "#")

## Part 2

d = read_lines(here::here("day16/test.txt"))
d = read_lines(here::here("day16/input.txt"))


m = d |>
  str_split("") |>
  do.call(rbind, args = _)

move = function(cur=c(1,0), dir=c(0,1)) {
  
  visit = m
  visit[TRUE] = "."
  
  usable_split = matrix(m %in% c("-","|"), nrow = nrow(m), ncol = ncol(m))
  
  dir = list(dir)
  cur = list(cur) 
  
  prev = character()
  
  repeat {
    if (length(dir) == 0)
      break
    
    for (i in seq_along(dir)) {
      if (is.null(cur[i][[1]]))
        next
      
      cur[[i]] = cur[[i]] + dir[[i]]
      
      if (cur[[i]][1] < 1 | cur[[i]][1] > nrow(m) | cur[[i]][2] < 1 | cur[[i]][2] > ncol(m)) {
        cur[[i]] = c()
        dir[[i]] = c()
        next
      }
      
      visit[cur[[i]][1], cur[[i]][2]] = "#"
      
      if (m[cur[[i]][1], cur[[i]][2]] == "|" & ((dir[[i]][1] == 0 & dir[[i]][2] == 1) | (dir[[i]][1] == 0 & dir[[i]][2] == -1))) {
        
        if (usable_split[cur[[i]][1], cur[[i]][2]]) {
          dir[[i]] = c(1,0)
          dir[[length(dir) + 1]] = c(-1,0)
          cur[[length(dir)]] = cur[[i]]
          usable_split[cur[[i]][1], cur[[i]][2]] = FALSE
        } else {
          cur[[i]] = c()
          dir[[i]] = c()
          next
        }
        
      } 
      
      if (m[cur[[i]][1], cur[[i]][2]] == "-" & ((dir[[i]][1] == 1 & dir[[i]][2] == 0) | (dir[[i]][1] == -1 & dir[[i]][2] == 0))) {
        if (usable_split[cur[[i]][1], cur[[i]][2]]) {
          dir[[i]] = c(0,1)
          dir[[length(dir) + 1]] = c(0,-1)
          cur[[length(dir)]] = cur[[i]]
          usable_split[cur[[i]][1], cur[[i]][2]] = FALSE
        } else {
          cur[[i]] = c()
          dir[[i]] = c()
          next
        }
      }
      
      if (m[ cur[[i]][1], cur[[i]][2] ] == "\\") {
        if (       dir[[i]][1] == 0  & dir[[i]][2] == 1) {
          dir[[i]] = c(1,0)
        } else if (dir[[i]][1] ==  0 & dir[[i]][2] == -1) {
          dir[[i]] = c(-1,0)
        } else if (dir[[i]][1] ==  1 & dir[[i]][2] == 0) {
          dir[[i]] = c(0,1)
        } else if (dir[[i]][1] == -1 & dir[[i]][2] == 0) {
          dir[[i]] = c(0,-1)
        } 
      } 
      
      if (m[ cur[[i]][1], cur[[i]][2] ] == "/") {
        if (       dir[[i]][1] == 0  & dir[[i]][2] == -1) {
          dir[[i]] = c(1,0)
        } else if (dir[[i]][1] ==  0 & dir[[i]][2] == 1) {
          dir[[i]] = c(-1,0)
        } else if (dir[[i]][1] ==  1 & dir[[i]][2] == 0) {
          dir[[i]] = c(0,-1)
        } else if (dir[[i]][1] == -1 & dir[[i]][2] == 0) {
          dir[[i]] = c(0,1)
        } 
      }
      
    }
    
    state = sum(visit == "#")
    prev = c(state, prev)
    
    if (length(prev) > 10 && sum(duplicated(prev[1:10])) == 9) # Not sure why 10 is the magic number
      break
    
    #print(visit)
  }
  sum(visit == "#")
}

res = bind_rows(
  tibble(
    c = 0,
    r = seq_len(nrow(m)),
    dir = list(c(0,1))
  ),
  tibble(
    c = ncol(m) + 1,
    r = seq_len(nrow(m)),
    dir = list(c(0,-1))
  ),
  tibble(
    c = seq_len(ncol(m)),
    r = 0,
    dir = list(c(1,0))
  ),
  tibble(
    c = seq_len(ncol(m)),
    r = nrow(m) + 1,
    dir = list(c(-1,0))
  )
) |>
  mutate(
    cur = map2(r,c, ~ c(.x,.y))
  ) |>
  mutate(
    score = map2_dbl(
      cur, dir, move,
      .progress = TRUE
    )
  ) 

res |>
  pull(score) |>
  max()
