library(tidyverse)

## Part 1

d = read_lines(here::here("day03/test.txt"))
d = read_lines(here::here("day03/input.txt"))

m = d |> 
  str_split("") %>%
  do.call(rbind, .)

z = str_locate_all(d, "\\d+") |>
  imap_dfr(
    function(x,y) {
      as_tibble(x) |>
        mutate(
          i = y,
          idx = map2(start,end, ~ .x:.y)
        )
    }
  ) 

w = z |>
  select(
    r = i, c = idx
  ) |>
  pmap_lgl(
    function(r,c) {
      expand_grid(
        r = r + c(-1,0,1),
        c = c(c-1,c, c+1)
      ) |>
        distinct() |>
        filter(
          r >= 1 & r <= nrow(m) & c >= 1 & c <= ncol(m)
        ) |>
        pmap_chr(
          function(r,c) {
            m[r,c]
          }
        ) |>
        paste0(collapse = "") |>
        str_detect("[^0-9.]")
    }
  )

z |>
  filter(w) |>
  mutate(
    num = map2(i, idx, ~ m[.x,.y] |> paste(collapse=""))
  ) |>
  summarize(
    sum(as.numeric(num))
  )


## Part 2

d = read_lines(here::here("day03/test.txt"))
d = read_lines(here::here("day03/input.txt"))

m = d |> 
  str_split("") %>%
  do.call(rbind, .)

gears = which(m == "*", arr.ind = TRUE) |>
  as_tibble() |>
  mutate(
    gear_id = row_number(),
    nb = map2(row, col, ~ expand_grid(r = .x + c(-1,0,1), c = .y + c(-1,0,1)))
  ) |>
  unnest(nb)

z = str_locate_all(d, "\\d+") |>
  imap_dfr(
    function(x,y) {
      as_tibble(x) |>
        mutate(
          i = y,
          idx = map2(start,end, ~ .x:.y)
        )
    }
  ) |> 
  mutate(
    num = map2_chr(i, idx, ~ m[.x,.y] |> paste(collapse="")),
    id = row_number()
  ) |>
  select(-start, -end) |>
  unnest_longer(idx) |>
  rename(r = i, c = idx)

inner_join(gears, z, by = c("r", "c")) |>
  group_by(gear_id) |>
  summarize(
    n_id = length(unique(id)),
    nums = list(unique(as.numeric(num))),
    ratio = (prod(unique(as.numeric(num))))
  ) |>
  filter(n_id == 2) |>
  summarize(
    sum(ratio)
  )
  
