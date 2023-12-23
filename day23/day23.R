library(tidyverse)

## Part 1

d = read_file(here::here("day23/test.txt"))
d = read_file(here::here("day23/input.txt"))


slide = list("^" = c(-1,0), ">" = c(0,1), "v"=c(1,0), "<"=c(0,-1))

m = str_split(d, "\n")[[1]] |>
  str_split("") |>
  do.call(rbind, args=_)

edges = list()

for (r in 1:nrow(m)) {
  for (c in 1:ncol(m)) {
    edges[[length(edges)+1]] = if (m[r,c] == ".") {
      tibble(
        to_r = r+c(-1,0,1,0),
        to_c = c+c(0,-1,0,1)
      ) |>
        filter(
          to_r >= 1 & to_r <= nrow(m) & to_c >= 1 & to_c <= ncol(m)
        ) |>
        filter(
          map2_lgl(to_r, to_c, ~m[.x,.y] != "#")
        ) |>
        transmute(
          from = glue::glue("{r},{c}"),
          to = glue::glue("{to_r},{to_c}")
        )
    } else if (m[r,c] %in% c("^", ">", "v", "<")) {
      to = c(r,c) + slide[[m[r,c]]]
      tibble(
        from = glue::glue("{r},{c}"),
        to = glue::glue("{to[1]},{to[2]}")
      )
    }
  }
}

edges = bind_rows(edges)

start = glue::glue('1,{which(m[1,] == ".")}')
end = glue::glue(('{nrow(m)},{which(m[nrow(m),] == ".")}'))

g = igraph::graph_from_data_frame(edges)

igraph::all_simple_paths(g, start, end) |>
  map_int(length) |>
  max() |> (\(x) x-1)()

## Part 2

d = read_file(here::here("day23/test.txt"))
d = read_file(here::here("day23/input.txt"))

m = str_split(d, "\n")[[1]] |>
  str_split("") |>
  do.call(rbind, args=_)

edge_list = list()

for (r in 1:nrow(m)) {
  for (c in 1:ncol(m)) {
    edge_list[[length(edge_list)+1]] = if (m[r,c] %in% c(".", "^", ">", "v", "<")) {
      tibble(
        to_r = r+c(-1,0,1,0),
        to_c = c+c(0,-1,0,1)
      ) |>
        filter(
          to_r >= 1 & to_r <= nrow(m) & to_c >= 1 & to_c <= ncol(m)
        ) |>
        filter(
          map2_lgl(to_r, to_c, ~m[.x,.y] != "#")
        ) |>
        transmute(
          from = glue::glue("{r},{c}"),
          to = glue::glue("{to_r},{to_c}")
        )
    }
  }
}

edges = bind_rows(edge_list) |>
  mutate(weight = 1) |>
  igraph::graph_from_data_frame(directed = FALSE) |>
  igraph::as_data_frame() |>
  distinct() 


walk(
  c(edges$from, edges$to) |> unique(),
  function(v) {
    prev = edges |> slice(0)
    repeat {
      cur = edges |> filter(from == v | to == v)
      
      if (nrow(cur) != 2)
        break
      
      nodes = setdiff(c(cur$from, cur$to), v)
      
      edges <<- bind_rows(
        tibble(
          from = nodes[1],
          to = nodes[2],
          weight = sum(cur$weight)
        ),
        edges |> filter(from != v & to != v)
      )
      
      if (nrow(anti_join(prev, edges, by = join_by(from, to, weight))) == 0)
        break
      
      prev = edges
    }
  },
  .progress = TRUE
)

start = glue::glue('1,{which(m[1,] == ".")}')
end = glue::glue(('{nrow(m)},{which(m[nrow(m),] == ".")}'))


g = igraph::graph_from_data_frame(edges, directed = FALSE)
p = igraph::all_simple_paths(g, start, end) 

future::plan(future::multisession(workers = 8))

res = furrr::future_map_dbl(
  p,
  function(x) {
    ep = rep(names(x), each=2)[c(-1, -length(x)*2)]
    sum(igraph::E(g)$weight[igraph::get.edge.ids(g, ep)])
  },
  .progress = TRUE
)

max(res)

