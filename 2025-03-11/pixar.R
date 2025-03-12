library(tidyverse)

tuesdata <- tidytuesdayR::tt_load(2025, week = 10)

pixar_films <- tuesdata$pixar_films
public_response <- tuesdata$public_response

public_response |>
  ggplot(aes(rotten_tomatoes, metacritic)) +
  geom_point(aes(color = cinema_score)) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE)

public_response |>
  ggplot() +
  geom_point(aes(rotten_tomatoes, critics_choice, color = cinema_score))

public_agg <- public_response |>
  filter(!is.na(rotten_tomatoes)) |>
  mutate(
    agg = case_when(
      is.na(critics_choice) ~ (rotten_tomatoes + metacritic) / 2,
      !is.na(critics_choice) ~
        (rotten_tomatoes + metacritic + critics_choice) / 3
    )
  )

combined <- pixar_films |>
  select(!number) |>
  inner_join(public_agg, by = join_by("film"))

combined |>
  ggplot(aes(release_date, agg, color = cinema_score)) +
  geom_point() +
  facet_wrap(~film_rating)

combined |>
  ggplot(aes(run_time, agg, color = cinema_score)) +
  geom_point() +
  facet_wrap(~film_rating)
