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
  #stopifnot(length(code)!=4)
  cat(paste(seq, collapse = ""), code, "\n")
  if (
    (length(seq) == 0 & length(code) == 0) |
    (all(seq %in% c(".","?")) & length(code) == 0)
  ) {
    return(1)
  }
  
  if (
    (!all(seq %in% c(".","?")) & length(code) == 0) |
    sum(code)+length(code)-1 > length(seq)
  ) {
    return(0)
  }
  
  total = 0
  if (seq[1] == ".") {
    total = total + count(seq[-1], code)
  } else if (seq[1] == "#") {
    total = total + 
      if (any(seq[seq_len(code[1])] == ".")) {
        return(0)
      } else {
        count(seq[-seq_len(code[1])], code[-1])
      }
    
  } else { # seq[1] == "?"
    total = total + count(seq[-1], code)
    total = total + 
      if (any(seq[seq_len(code[1])] == ".")) {
        return(0)
      } else {
        count(seq[-seq_len(code[1])], code[-1])
      }
  }
  
  total
}

count2 = memoise::memoise(count)

count2(z$seq[[1]], z$codes[[1]])




#cli::cli_progress_bar(total = nrow(z))

(z2 = z |>
    mutate(
      map_dbl(
        row_number(),
        function(i) {
          cat(i, "\n")
          code = z$codes[[i]]
          seq = z$seq[[i]]
          z = count2(seq, code)
          #cli::cli_progress_update(.envir = .GlobalEnv)
          z
        },
        .progress = TRUE
      )
    )
)




count = function(seq, code) {
  cat(paste(seq,collapse=""), code, "\n")
  if (length(code) == 0 & sum(seq == "#") == 0) {
    return(1)
  }
  
  i = min(
    which(seq == ".")[1],
    length(seq),
    na.rm=TRUE
  )
  
  sub = seq[seq_len(i)]
  if (sum(sub == "#") > code[1]) return(0)
  if (sum(sub == "#") + sum(sub == "?") < code[1]) return(0)
  
  if (sum(sub == "#") == code[1] & sum(sub == "?") == 0) {
    return(
      count(seq[-seq_len(i)], code[-1])
    )
  }
  
  j = which(seq == "?")[1]
  
  return( 
    count(replace(seq, j, "#"), code) +
      count(replace(seq, j, "."), code)
  )
}

count2 = memoise::memoise(count)


code = z$codes[[1]]
seq = z$seq[[1]]
count2(seq,code)


(z2 = z |>
    slice(-6) |>
    mutate(
      map2_dbl(
        seq, codes,
        function(x, y) {
          cat(unlist(codes),"\n")
          count2(x,y)
        }
      )
    )
)