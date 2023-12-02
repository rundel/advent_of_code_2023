library(tidyverse)

## Part 1

d = read_lines(here::here("day02/test.txt"))
d = read_lines(here::here("day02/input.txt"))

d = "Game 100: 2 red, 13 blue, 1 green; 1 green, 12 blue; 1 red, 5 blue, 1 green; 3 blue, 3 red"

z = d |>
  str_remove("Game \\d+: ") |>
  str_split(";") |>
  map(
    function(x) {
      str_trim(x) |> 
        str_split(", ") |>
        map(
          function(y) {
            z = str_split(y, " ")
            
            print(map_chr(z, 1) |> as.numeric())
            print(map_chr(z, 2))
            
            z = setNames(map_chr(z, 1) |> as.numeric(), map_chr(z, 2) )
            if (is.na(z["blue"]))  z["blue"]  = 0
            if (is.na(z["green"])) z["green"] = 0
            if (is.na(z["red"]))   z["red"]   = 0
            z[c("red", "green", "blue")]
          } 
        )
    }
  )

map_lgl(
  z,
  function(x) {
    any(map_lgl(
      x,
      function(y) {
        any(y > c(12,13,14))
      }
    ))
  }
) %>%
  {which(!.)} %>%
  sum()
  


str_match_all(d, "(?: (\\d) blue,?)?(?: (\\d) green,?)?(?: (\\d) red,?)?;")

  ## Part 2

d = read_lines(here::here("day02/test2.txt"))
d = read_lines(here::here("day02/input.txt"))

