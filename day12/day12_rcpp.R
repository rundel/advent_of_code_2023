library(tidyverse)

## Part 1

d = read_lines(here::here("day12/test.txt"))
d = read_lines(here::here("day12/input.txt"))

tibble( 
  seq = d |>
    str_split(" ") |>
    map_chr(1L) |>
    str_split(""),
  
  codes = d|>
    str_split(" ") |>
    map_chr(2L) |>
    str_split(",") |>
    map(as.numeric),
  
  n_tot = map_dbl(codes, sum)
) |>
  mutate(
    n_fix = map_dbl(seq, ~sum(.x == "#")),
    n_free = n_tot - n_fix,
    i = row_number()
  ) |>
  mutate(
    poss = map2(
      seq, n_free,
      function(seq, n) {
        t(combn(which(seq == "?"), n)) |>
          apply(1, function(x) {
            seq[x] = "#"
            seq[seq == "?"] = "."
            list(seq)
          }) |>
          map(1)
      }  
    )
  ) |>
  unnest_longer(poss) |>
  mutate(
    rle = map(poss, function(x) {
      z = rle(x)
      as.numeric(z$lengths[z$values == "#"])
    }),
    match = map2_dbl(
      codes, rle, identical
    )
  ) |>
  summarize(
    tot = sum(match),
    .by = i
  ) |>
  pull(tot) |>
  sum()

## Part 2

d = read_lines(here::here("day12/test.txt"))
d = read_lines(here::here("day12/input.txt"))

z = tibble( 
  seq = d |>
    str_split(" ") |>
    map_chr(1L) |>
    str_split(""),
  
  codes = d |>
    str_split(" ") |>
    map_chr(2L) |>
    str_split(",") |>
    map(as.numeric),
) |>
  mutate(
    seq = map(
      seq, function(x) {
        z = rep(c(x, "?"), 5)
        z[-length(z)]
      }
    ),
    codes = map(
      codes, function(x) {
        rep(x, 5)
      }
    )
  ) 

Rcpp::sourceCpp(here::here("day12/day12.cpp"))

(z2 = z |>
    mutate(
      res = map_dbl(
        row_number(),
        function(i) {
          #cat(i, "\n")
          code = z$codes[[i]]
          seq = z$seq[[i]]
          count(seq, code, 0,0,0)
        },
        .progress = TRUE
      )
    )
)


options(digits = 20)
z2 |>
  pull(res) |>
  sum()

