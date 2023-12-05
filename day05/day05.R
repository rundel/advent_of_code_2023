library(tidyverse)

## Part 1

d = read_file(here::here("day05/test.txt"))
d = read_file(here::here("day05/input.txt"))

z = d |>
  str_trim() |>
  str_split("\n\n") |>
  unlist() |>
  str_split(":[ \n]") |>
  tibble(z = _) |>
  hoist(
    z,
    name = 1,
    value = 2
  ) |>
  mutate(
    value = str_split(value, "\n")
  ) |>
  unnest_longer(value) |>
  mutate(
    value = map(value, ~ str_split(.x, " ") |> unlist() |> as.double())
  ) 

seeds = z[1,]$value[[1]]
steps = z[-1,] |>
  mutate(
    name = as_factor(name),
    dest_start = map_dbl(value, ~ .x[1]),
    dest_end   = map_dbl(value, ~ .x[1] + .x[3]-1),
    src_start = map_dbl(value, ~ .x[2]),
    src_end   = map_dbl(value, ~ .x[2] + .x[3]-1),
  ) |>
  group_by(name) |>
  summarize(
    o = order(src_start), 
    src_start  =  list(src_start[o]),
    src_end    =  list(src_end[o]),
    dest_start = list(dest_start[o]),
    dest_end   = list(dest_end[o])
  ) |>
  select(-o) |>
  distinct()

res = c()
for (val in seeds) {
  for (i in seq_len(nrow(steps))) {
    for (j in seq_along(steps$src_start[[i]])) {
      
      if (val >= steps$src_start[[i]][j] && val <= steps$src_end[[i]][j]) {
        val = (val - steps$src_start[[i]][j]) + steps$dest_start[[i]][j]
        break
      }
    }
    cat(val, " ")
  }
  cat("\n")
  res = c(res, val)
}
res
min(res)


    
## Part 2

d = read_file(here::here("day05/test.txt"))
d = read_file(here::here("day05/input.txt"))

z = d |>
  str_trim() |>
  str_split("\n\n") |>
  unlist() |>
  str_split(":[ \n]") |>
  tibble(z = _) |>
  hoist(
    z,
    name = 1,
    value = 2
  ) |>
  mutate(
    value = str_split(value, "\n")
  ) |>
  unnest_longer(value) |>
  mutate(
    value = map(value, ~ str_split(.x, " ") |> unlist() |> as.double())
  ) 

seeds = z[1,]$value[[1]] |>
  matrix(2, byrow = FALSE) |>
  ( function(x) {
    map2_dfr(
      x[1,], x[2,], 
      function(s,e) {
        tibble(
          start = s,
          end   = s + e-1,
        )
      }
    )
  })()

steps = z[-1,] |>
  mutate(
    name = as_factor(name),
    dest_start = map_dbl(value, ~ .x[1]),
    dest_end   = map_dbl(value, ~ .x[1] + .x[3]-1),
    src_start = map_dbl(value, ~ .x[2]),
    src_end   = map_dbl(value, ~ .x[2] + .x[3]-1),
  ) |>
  group_by(name) |>
  summarize(
    o = order(src_start), 
    src_start  =  list(src_start[o]),
    src_end    =  list(src_end[o]),
    dest_start = list(dest_start[o]),
    dest_end   = list(dest_end[o])
  ) |>
  select(-o) |>
  ungroup() |>
  distinct()

max_val = steps |> select(-name) |> unlist() |> max()


## Work backwards


future::plan(future::multisession, workers = 8)

cli::cli_progress_bar(total=20)
all_res = list()
for (n in 1:20) {
  locs = sample(1:max_val, min(1e5, max_val)) |> sort()
  res = furrr::future_map(
    locs,
    function(val) {
      start = val
      for (i in rev(seq_len(nrow(steps)))) {
        for (j in seq_along(steps$src_start[[i]])) {
          
          if (val >= steps$dest_start[[i]][j] && val <= steps$dest_end[[i]][j]) {
            val = (val - steps$dest_start[[i]][j]) + steps$src_start[[i]][j]
            break
          }
        }
      }
      
      res = list()
      for (k in seq_len(nrow(seeds))) {
        if (val >= seeds$start[k] && val <= seeds$end[k]) {
          res = list(loc = start, seed = val)
          break
        }
      }
      res
    },
    .progress = FALSE
  )
  cat("\n")
  
  all_res[[n]] = res
  cli::cli_progress_update()
}

all_res

(seed_res = map_dfr(
  all_res, 
  function(x) {
    tibble(
      loc = map_dbl(x, 1, .default=NA ),
      seed = map_dbl(x, 2, .default=NA )
    ) |>
      filter(!is.na(loc)) |>
      arrange(loc) |>
      slice(1)
  }
))







##.Now forward for the "good" seeds

prop_seeds = seed_res$seed |>
  range() |>
  (\(x) {x + c(-500000,500000)})() |>
  (\(x) {do.call(seq, as.list(x))}) ()

res = c()

#cli::cli_progress_bar("Iters", total = length(seeds))
res = furrr::future_map_dbl(
  prop_seeds, 
  function(val) {
    for (i in seq_len(nrow(steps))) {
      for (j in seq_along(steps$src_start[[i]])) {
        
        if (val >= steps$src_start[[i]][j] && val <= steps$src_end[[i]][j]) {
          val = (val - steps$src_start[[i]][j]) + steps$dest_start[[i]][j]
          break
        }
      }
    }
    val
  },
  .progress = TRUE
)
min(res)


