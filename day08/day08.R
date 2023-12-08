library(tidyverse)

## Part 1

d = read_lines(here::here("day08/test.txt"))
d = read_lines(here::here("day08/test2.txt")) 
d = read_lines(here::here("day08/input.txt"))

steps = d[1] |> str_split("") |> unlist()
z = d[-(1:2)] |>
  str_match("([A-Z]{3}) = \\(([A-Z]{3}), ([A-Z]{3})\\)") |>
  as.data.frame() |>
  setNames(c("full", "x","y","z")) |>
  select(-full) |>
  mutate(
    vals = map2(y,z, ~ list(L=.x, R=.y))
  ) |>
  (\(x) {
    x$vals |> set_names(x$x)
  })()

i = 0
node = "AAA"
repeat {
  cur_step = steps[ (i %% length(steps)) + 1]
  cat(node, " ", cur_step, "\n")
  
  node = z[[node]][[cur_step]]
  
  i = i+1
  if (node == "ZZZ") {
    break
  }

}
i




## Part 2

d = read_lines(here::here("day08/test3.txt"))
d = read_lines(here::here("day08/input.txt"))

steps = d[1] |> str_split("") |> unlist()
z = d[-(1:2)] |>
  str_match("([A-Z0-9]{3}) = \\(([A-Z0-9]{3}), ([A-Z0-9]{3})\\)") |>
  as.data.frame() |>
  setNames(c("full", "x","y","z")) |>
  select(-full) |>
  mutate(
    vals = map2(y,z, ~ list(L=.x, R=.y))
  ) |>
  (\(x) {
    x$vals |> set_names(x$x)
  })()

res = c()
for (node in str_subset(names(z), "..A")) {
  i = 0
  repeat {
    cur_step = steps[ (i %% length(steps)) + 1]
    
    node = z[[node]][[cur_step]]
    
    i = i+1
    if (str_detect(node, "..Z")) {
      break
    }
    
  }
  res[node] = i
}
res

options(digits = 22)
reduce(res, pracma::Lcm) 

