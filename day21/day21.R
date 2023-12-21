library(tidyverse)

## Part 1

d = read_lines(here::here("day21/test.txt"))
d = read_lines(here::here("day21/input.txt"))

m = d |>
  str_split("") |>
  do.call(rbind, args = _)

plots = which(m == "." | m == "S", arr.ind = TRUE)
plot_names = apply(plots, 1, paste, collapse = ",")


dist = plots |>
  dist() |>
  as.matrix()

adj = dist
adj[adj != 1] = 0
rownames(adj) = plot_names
colnames(adj) = plot_names

g = igraph::graph_from_adjacency_matrix(adj, mode = "undirected", add.rownames = "row") 

start = which(m == "S", arr.ind = TRUE) |>
  apply(1, paste, collapse=",")

gd = igraph::distances(g, v = start)

n_steps = 64

sum(gd <= n_steps & gd %% 2 == 0)





## Part 2

d = read_lines(here::here("day21/test.txt"))
d = read_lines(here::here("day21/input.txt"))

m = d |>
  str_split("") |>
  do.call(rbind, args = _)

nr = nrow(m)
nc = ncol(m)

moves = c(1+0i, 0+1i, -1+0i, 0-1i)

s = which(m == "S", arr.ind = TRUE)
d = complex(real=s[1], imaginary=s[2])

total_steps = 26501365

n = 3*nr + total_steps %% nr + 1

res = integer(n)
cli::cli_progress_bar(total=n)
for (i in seq_len(n)) {
  d = rep(d, rep(4, length(d))) + rep(moves, length(d)) |>
    unique()
  d = d[map2_lgl((Re(d)-1) %% nr + 1,(Im(d)-1) %% nc + 1, ~m[.x,.y] != "#")] |>
    unique()
  
  res[i] = length(d)
  cli::cli_progress_update()
}

options(digits = 22)

tibble(
  s = (0:3)*nr + total_steps %% nr  
) |>
  mutate(
    n = res[s]
  ) |>
  lm(n ~ 1+s+I(s^2), data=_) |>
  predict(newdata = tibble(s = total_steps))

