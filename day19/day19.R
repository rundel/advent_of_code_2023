library(tidyverse)

## Part 1

d = read_file(here::here("day19/test.txt"))
d = read_file(here::here("day19/input.txt"))

z = str_split(d, "\n\n")[[1]]

wf = 
  str_split(z[1], "\n")[[1]] |>
    tibble(wf = _) |>
    separate_wider_delim(wf, delim = "{", names = c("label", "rules")) |>
    mutate(
      rules = rules |>
        str_remove("\\}") |>
        str_split(",") |>
        map_chr(
          ~ str_replace(.x, "([a-z]+[<>]\\d+):([ARa-z]+)", "if (\\1) {\"\\2\"}") |> 
            str_replace("^([ARa-z]+)$", "\"\\1\"") |>
            paste(collapse = " else ")
        ) |>
        map(~ parse(text = .x))
    )

wf =   wf$rules |> set_names(wf$label)
    

  

vals = str_split(str_trim(z[2]), "\n")[[1]] |>
  str_remove_all("[{}]") |>
  str_replace_all(",", ";") |>
  map(~ parse(text = .x))

map_dbl(
  vals, function(v) {
    e = rlang::env()
    eval(v, envir = e)
    
    state = "in"
    
    repeat {
      #cat(state,"\n")
      state = eval(wf[[state]], envir = e)
      
      if (state == "A") {
        return( eval(parse(text="x+m+a+s"), envir = e) )
      } else if (state == "R") {
        return( 0 )
      }
    }
  }
) |>
  sum()



## Part 2

d = read_file(here::here("day19/test.txt"))
d = read_file(here::here("day19/input.txt"))

z = str_split(d, "\n\n")[[1]]

wf = 
  str_split(z[1], "\n")[[1]] |>
  tibble(wf = _) |>
  separate_wider_delim(wf, delim = "{", names = c("label", "rules")) |>
  mutate(
    rules = rules |>
      str_remove("\\}") |>
      str_split(",") 
  ) |>
    unnest_longer(rules) |>
    separate_wider_delim(rules, delim = ":", names = c("filter", "target"), too_few = "align_end") |>
    mutate(
      filter = filter |>
        str_replace_all("([xmas])([<>]\\d+)", 'v[["\\1"]] = v[["\\1"]][v[["\\1"]]\\2]')
    ) 
    

count = function(posb = list(x=1:4000,m=1:4000,a=1:4000,s=1:4000), state = "in") {
  df = wf |>
    filter(label == state)
  
  n = 0
  for (i in seq_len(nrow(df))) {
    v = posb
    f = df$filter[i]
    tar = df$target[i]
    
    if (!is.na(f)) {
      eval(parse(text = f))
    }
    
    if (tar == "A") {
      n = n + map_dbl(v, length) |> prod()
    } else {
      n = n + count(v, tar)
    }
    
    if (!is.na(f)) {
      if (str_detect(f, "<")) {
        f = str_replace(f, "<", ">=")
      } else if (str_detect(f, ">")) {
        f = str_replace(f, ">", "<=")
      }
      
      v = posb
      eval(parse(text = f))
      posb = v
    }
  }
  
  n
}

count()
