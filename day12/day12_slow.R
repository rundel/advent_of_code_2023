library(tidyverse)
library(igraph)

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




rle_code = function(x, char = "#") {
  z = rle(x)
  as.numeric(z$lengths[z$values == char])
}

count = function(seq, code) {
  #cat(paste(seq, collapse = ""), code, "\n")
  if (sum(seq == "?") == 0) {
    check = identical(rle_code(seq), code)
    #if (check)
    #  cat(paste(seq,collapse=""), "\n")
    return(check)
  }
  
  if (sum(seq == "?") + sum(seq == "#") < sum(code)) return(0)
  
  i = which(seq == "?")[1]
  
  cur_rle = rle_code(seq[seq_len(i-1)])
  n = length(cur_rle)
  if (n > length(code)) return(0)
  
  code_sub = code[seq_along(cur_rle)]
  
  if (n > 0) {
    if (!identical(cur_rle[-n], code_sub[-n]) | cur_rle[n] > code_sub[n]) {
      return(0)
    }
  }
  
  if (i > 1) {
    if (seq[i-1] == "#") {
      
      if (cur_rle[n] == code_sub[n]) {
        return( count(replace(seq, i, "."), code) )
      } else {
        idx = (i-1):((i-1) + code_sub[n]-1)
        end = (i-1) + code_sub[n]
        if (! all(seq[idx] %in% c("#","?")) | ! (seq[end] %in% c("?", ".") | end > length(seq)))
          return(0)
        else {
          new_seq = replace(seq, idx, "#") |>
            replace(end, ".")
          return( count(new_seq, code) )
        }
      }
    }
  }
  
  return( 
    count(replace(seq, i, "#"), code) +
      count(replace(seq, i, "."), code)
  )
  
}

count2 = memoise::memoise(count, cache = cachem::cache_mem(max_size = 4*1024 * 1024^2))

#cli::cli_progress_bar(total = nrow(z))

(z2 = z |>
    mutate(
      res = map_dbl(
        row_number(),
        function(i) {
          cat(i, "\n")
          code = z$codes[[i]]
          seq = z$seq[[i]]
          count2(seq, code)
        },
        .progress = TRUE
      )
    )
)


