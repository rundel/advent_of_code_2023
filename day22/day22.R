library(tidyverse)

## Part 1

d = read_lines(here::here("day22/test.txt"))
#d = read_lines(here::here("day22/input.txt"))

state = d |> 
  str_split("~") |>
  map(~ str_split(.x, pattern=",") |> map(as.numeric) |> set_names(c("start","end"))) |>
  do.call(rbind, args = _) |>
  as_tibble() |>
  transmute(
    id = row_number(),
    block = map2(start,end, ~ expand_grid(x = .x[1]:.y[1], y = .x[2]:.y[2], z=.x[3]:.y[3]))
  ) |> 
  unnest(block) 
  #mutate(
  #  min_z = min(z),
  #  .by = id
  #) |>
  #arrange(min_z, id) |>
  #mutate(
  #  id = cur_group_id(),
  #  .by = id
  #) |>
  #select(-min_z)

prev_state = tibble(id=integer(), x=integer(), y=integer(), z=integer())

move = function(state) {
  for (i in unique(state$id)) {
    repeat {
      cur = state |> filter(id == i) |>
        mutate(z = z-1)
      
      if (any(cur$z == 0)) {
        break
      }
      
      other = state |> filter(id != i)
      
      if (nrow(semi_join(other, cur, by=c("x","y","z"))) == 0) {
        state = bind_rows(other, cur)
      } else {
        break
      }
    }
  }
  
  return(arrange(state, id))
}

n=1
repeat {
  state = move(state)
  
  check = anti_join(state, prev_state, by=c("id","x","y","z"))
  if (nrow(check) == 0)
    break
  
  prev_state = state
  n = n+1
  cat(n, "\n")
}

res = map(
  unique(state$id),
  function(i) {
    cur = state |> filter(id == i) |>
      mutate(z = z-1)
  
    other = state |> filter(id != i)
    
    semi_join(other, cur, by=c("x","y","z"))$id
  }, 
  .progress=TRUE
)

# If we are sitting on only one block can't disintegrate it
length(unique(state$id)) - ( unlist(res[map(res, length) == 1]) |> unique() |> length() )



## Part 2

d = read_lines(here::here("day22/test.txt"))
d = read_lines(here::here("day22/input.txt"))

