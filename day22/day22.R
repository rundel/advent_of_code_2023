library(tidyverse)

## Part 1

d = read_lines(here::here("day22/test.txt"))
d = read_lines(here::here("day22/input.txt"))

state = d |> 
  str_split("~") |>
  map(~ str_split(.x, pattern=",") |> map(as.numeric) |> set_names(c("start","end"))) |>
  do.call(rbind, args = _) |>
  as_tibble() |>
  mutate(
    min_z = map2_dbl(start,end, ~ min(.x[3],.y[3]))
  ) |>
  arrange(min_z) |>
  transmute(
    id = row_number(),
    block = map2(start,end, ~ expand_grid(x = .x[1]:.y[1], y = .x[2]:.y[2], z=.x[3]:.y[3]))
  ) |> 
  unnest(block)

prev_state = tibble(id=integer(), x=integer(), y=integer(), z=integer())

move = function(state) {
  ids = unique(state$id)
  cli::cli_progress_bar(total = length(ids))
  for (i in ids) {
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
    cli::cli_progress_update()
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

below = map(
  unique(state$id),
  function(i) {
    cur = state |> filter(id == i) |>
      mutate(z = z-1)
  
    other = state |> filter(id != i)
    
    semi_join(other, cur, by=c("x","y","z"))$id |>
      unique()
  }, 
  .progress=TRUE
)

above = map(
  unique(state$id),
  function(i) {
    cur = state |> filter(id == i) |>
      mutate(z = z+1)
    
    other = state |> filter(id != i)
    
    semi_join(other, cur, by=c("x","y","z"))$id |>
      unique()
  }, 
  .progress=TRUE
)

map_lgl(
  above,
  function(x) {
    
    if (length(x) == 0) {
      TRUE
    } else {
      map_lgl(
        x, function(y) {
          length(below[[y]]) > 1
        }
      ) |>
        all()
    }
  }
) |> sum()


## Part 2

below = map(
  unique(state$id),
  function(i) {
    cur = state |> filter(id == i) |>
      mutate(z = z-1)
    
    other = state |> filter(id != i)
    
    semi_join(other, cur, by=c("x","y","z"))$id |>
      unique()
  }, 
  .progress=TRUE
)

above = map(
  unique(state$id),
  function(i) {
    cur = state |> filter(id == i) |>
      mutate(z = z+1)
    
    other = state |> filter(id != i)
    
    semi_join(other, cur, by=c("x","y","z"))$id |>
      unique()
  }, 
  .progress=TRUE
)

res = map_dbl(
  unique(state$id),
  function(i) {
    queue = c(above[[i]])
    remove = c(i)
    n = 0
    while(length(queue) != 0) {
      j = queue[1]
      queue = queue[-1]
      
      if ( length(setdiff(below[[j]], remove)) == 0 ) {
        remove = c(remove, j)
        queue = setdiff( c(queue, above[[j]]), remove )
        n = n+1
      }
    }
    
    n
  },
  .progress = TRUE
)

sum(res)
