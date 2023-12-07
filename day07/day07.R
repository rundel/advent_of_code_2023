library(tidyverse)

## Part 1

d = read_lines(here::here("day07/test.txt")) 
d = read_lines(here::here("day07/input.txt"))

lvl = c("A", "K", "Q", "J", "T", "9", "8", "7", "6", "5", "4", "3", "2")

d |>
  str_split(" ") |>
  map(~ set_names(.x, c("hand", "bid"))) |>
  bind_rows() |>
  mutate(
    hand = str_split(hand, "") |> map( ~ set_names(.x, paste0("C",1:5))),
    hand_tbl = map_chr(hand, ~table(.x) |> sort(decreasing = TRUE) |> paste(collapse="")),
    hand_score = case_when(
        hand_tbl == "5" ~ 7,
        hand_tbl == "41" ~ 6,
        hand_tbl == "32" ~ 5,
        hand_tbl == "311" ~ 4,
        hand_tbl == "221" ~ 3,
        hand_tbl == "2111" ~ 2,
        hand_tbl == "11111"~ 1
      )
  ) |>
  unnest_wider(hand) |>
  mutate(
    C1 = factor(C1, levels = lvl),
    C2 = factor(C2, levels = lvl),
    C3 = factor(C3, levels = lvl),
    C4 = factor(C4, levels = lvl),
    C5 = factor(C5, levels = lvl)
  ) |>
  arrange(desc(hand_score), C1,C2,C3,C4,C5) |>
  pull(bid) |>
  as.numeric() |>
  (\(x) {
    sum(x * rev(seq_along(x)))
  })()

## Part 2

d = read_lines(here::here("day07/test.txt"))
d = read_lines(here::here("day07/input.txt"))

lvl = c("A", "K", "Q", "T", "9", "8", "7", "6", "5", "4", "3", "2", "J")

use_joker = function(hand) {
  tbl = table(hand) |> sort(decreasing = TRUE)
  if (length(tbl) == 1) {
    tbl |> paste(collapse="")
  } else {
    hand[hand == "J"] = names(tbl[names(tbl) != "J"])[1]
    table(hand) |> sort(decreasing = TRUE) |> paste(collapse="")
  }
}

d |>
  str_split(" ") |>
  map(~ set_names(.x, c("hand", "bid"))) |>
  bind_rows() |>
  mutate(
    hand = str_split(hand, "") |> map( ~ set_names(.x, paste0("C",1:5))),
    hand_tbl = map_chr(hand, ~use_joker(.x)),
    hand_score = case_when(
      hand_tbl == "5" ~ 7,
      hand_tbl == "41" ~ 6,
      hand_tbl == "32" ~ 5,
      hand_tbl == "311" ~ 4,
      hand_tbl == "221" ~ 3,
      hand_tbl == "2111" ~ 2,
      hand_tbl == "11111"~ 1
    )
  ) |> 
  unnest_wider(hand) |>
  mutate(
    C1 = factor(C1, levels = lvl),
    C2 = factor(C2, levels = lvl),
    C3 = factor(C3, levels = lvl),
    C4 = factor(C4, levels = lvl),
    C5 = factor(C5, levels = lvl)
  ) |>
  arrange(desc(hand_score), C1,C2,C3,C4,C5) |>
  pull(bid) |>
  as.numeric() |>
  (\(x) {
    sum(x * rev(seq_along(x)))
  })()

