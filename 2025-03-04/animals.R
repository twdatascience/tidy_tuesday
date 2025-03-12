library(tidyverse)

tuesdata <- tidytuesdayR::tt_load(2025, week = 9)

longbeach <- tuesdata$longbeach

longbeach |>
  count(animal_type, outcome_type, outcome_subtype, sort = TRUE)

longbeach |>
  count(outcome_type, primary_color, sort = TRUE)
