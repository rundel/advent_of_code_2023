library(tidyverse)

## Part 1

d = read_lines(here::here("day20/test.txt"))
d = read_lines(here::here("day20/test2.txt"))
d = read_lines(here::here("day20/input.txt"))

z = str_split(d, " -> ")

df = str_match(map_chr(z, 1), "^([%&])?(.*)")[,-1] |>
  as.data.frame() |>
  set_names(c("op", "name")) |>
  mutate(
    dest = str_split(map_chr(z, 2), ", ")
  ) |>
  as_tibble() |>
  mutate(
    op = ifelse(is.na(op), "", op)
  ) |>
  bind_rows(
    list(op = "", name = "button", dest = list("broadcaster"))
  )

df = full_join(
  df,
  df |> 
    unnest_longer(dest) |>
    summarize(
      cur = list(name), .by=dest
    ) |>
    rename(name = dest),
  by = "name"
) |>
  mutate(
    state = map2(
      op, cur,
      ~ if (is.na(.x)) {
        integer()
      } else if (.x == "&") {
        rep(0, length(.y)) |> set_names(.y) 
      } else if (.x == "%") {
        0
      }
    )
  ) |>
  mutate(
    op = ifelse(is.na(op), "", op)
  )
  

     
  

path = df$dest |> set_names(df$name)
state = df$state |> set_names(df$name)
type = df$op |> set_names(df$name)

sent = c(low=0, high=0)

send = function(state) {
  queue = list(list(cur="button", dest="broadcaster", signal = 0))
  
  while (length(queue) > 0) {
    dest = queue[[1]]$dest
    signal = queue[[1]]$signal
    cur = queue[[1]]$cur
    queue = queue[-1]
    
    sent[signal+1] <<- sent[signal+1] + 1 
    cat(cur, signal, " -> ", dest, "\n")
    
    if (type[dest] == "%") {
      if (signal == 0) {
        if (state[[dest]] == 0) {
          state[[dest]] = 1
          signal = 1
        } else {
          state[[dest]] = 0
          signal = 0
        }
      } else {
        next
      }
    } else if (type[dest] == "&") {
      state[[dest]][cur] = signal
      if (all(state[[dest]] == 1)) {
        signal = 0
      } else {
        signal = 1
      }
    }
      
    for (d in path[[dest]]) {
      queue[[length(queue)+1]] = list(
        cur = dest,
        dest = d,
        signal = signal
      )
    }
  }
  
  return(state)
}

state = df$state |> set_names(df$name)

for (i in 1:1000) {
  state = send(state)
}
sent |> prod()


## Part 2

d = read_lines(here::here("day20/test.txt"))
d = read_lines(here::here("day20/test2.txt"))
d = read_lines(here::here("day20/input.txt"))

z = str_split(d, " -> ")

df = str_match(map_chr(z, 1), "^([%&])?(.*)")[,-1] |>
  as.data.frame() |>
  set_names(c("op", "name")) |>
  mutate(
    dest = str_split(map_chr(z, 2), ", ")
  ) |>
  as_tibble() |>
  mutate(
    op = ifelse(is.na(op), "", op)
  ) |>
  bind_rows(
    list(op = "", name = "button", dest = list("broadcaster"))
  )

df = full_join(
  df,
  df |> 
    unnest_longer(dest) |>
    summarize(
      cur = list(name), .by=dest
    ) |>
    rename(name = dest),
  by = "name"
) |>
  mutate(
    state = map2(
      op, cur,
      ~ if (is.na(.x)) {
        integer()
      } else if (.x == "&") {
        rep(0, length(.y)) |> set_names(.y) 
      } else if (.x == "%") {
        0
      }
    )
  ) |>
  mutate(
    op = ifelse(is.na(op), "", op)
  )





path = df$dest |> set_names(df$name)
state = df$state |> set_names(df$name)
type = df$op |> set_names(df$name)

sent = c(low=0, high=0)

send_cycle = function(state, stop_node, stop_signal) {
  queue = list(list(cur="button", dest="broadcaster", signal = 0))
  
  while (length(queue) > 0) {
    dest = queue[[1]]$dest
    signal = queue[[1]]$signal
    cur = queue[[1]]$cur
    queue = queue[-1]
    
    sent[signal+1] <<- sent[signal+1] + 1 
    #cat(cur, signal, " -> ", dest, "\n")
    
    if (signal == stop_signal & cur == stop_node) {
      return(NULL)
    }
    
    if (type[dest] == "%") {
      if (signal == 0) {
        if (state[[dest]] == 0) {
          state[[dest]] = 1
          signal = 1
        } else {
          state[[dest]] = 0
          signal = 0
        }
      } else {
        next
      }
    } else if (type[dest] == "&") {
      state[[dest]][cur] = signal
      if (all(state[[dest]] == 1)) {
        signal = 0
      } else {
        signal = 1
      }
    }
    
    for (d in path[[dest]]) {
      queue[[length(queue)+1]] = list(
        cur = dest,
        dest = d,
        signal = signal
      )
    }
  }
  
  return(state)
}


# rx -> dh -> tr, xm, dr, nh
# find cycles for the latter

find_cycle = function(stop_node, stop_signal) {
  state = df$state |> set_names(df$name)
  
  n = 1
  repeat {
    state = send_cycle(state, stop_node, stop_signal)
    if (is.null(state))
      break
    n = n+1
  }
  n
}

res = c(
  find_cycle("tr", 1),
  find_cycle("xm", 1),
  find_cycle("dr", 1),
  find_cycle("nh", 1)
)

options(digits=22)
reduce(res, pracma::Lcm) 
