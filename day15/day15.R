library(tidyverse)

## Part 1

hash = function(str, cur_val = 0) {
  str = str_split(str, "")[[1]]
  cur_val = 0
  for (c in str) {
    #cat(cur_val, utf8ToInt(c), (utf8ToInt(c) + cur_val), (utf8ToInt(c) + cur_val)*17,"\n")
    cur_val = ((utf8ToInt(c) + cur_val) * 17) %% 256
  }
  
  cur_val
}

hash("HASH")

d = read_lines(here::here("day15/test.txt"))
d = read_lines(here::here("day15/input.txt"))

str_split(d, ",")[[1]] |>
  map_dbl(hash) |>
  sum()

## Part 2

d = read_lines(here::here("day15/test.txt"))
d = read_lines(here::here("day15/input.txt"))

df = str_split(d, ",")[[1]] |>
  str_match("([a-z]+)([=-])(\\d+)?") |>
  as.data.frame() |>
  set_names(c("cmd","label","op","val")) |>
  mutate(
    hash = map_dbl(label, hash),
    val = as.numeric(val)
  )

boxes = list()
pwalk(
  df, 
  function(cmd, label, op, val, hash) {
    if (is.null(boxes[hash+1][[1]]))
      boxes[[hash+1]] <<- list()
    
    if (op == "=") {
      boxes[[hash+1]][label] <<- val
    } else {
      boxes[[hash+1]][label] <<- NULL
    }
  }
)

boxes |>
  map(unlist) |>
  map_dbl( ~ sum(.x * seq_along(.x))) |>
  (\(x) sum(x * seq_along(x)))()




