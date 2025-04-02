library(dplyr)
library(ggplot2)
library(tidyr)
library(patchwork)
library(purrr)

tuesdata <- tidytuesdayR::tt_load(2025, week = 13)

pokemon <- tuesdata$pokemon_df

dplyr::glimpse(pokemon)

poke_avg <- pokemon |> 
  select(-c(id, species_id)) |> 
  group_by(generation_id) |> 
  summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE))) |> 
  mutate(generation_id = replace_na(generation_id, 10))

poke_avg |> 
  ggplot(aes(generation_id, height)) +
  geom_line()

plot_line <- function(x, y, ...) {
  ggplot(poke_avg, aes(x = !!sym(x), y = !!sym(y))) +  
    geom_line() +
    theme_minimal()
}

names_df <- data.frame(x = rep("generation_id", length(names(poke_avg)) - 1),
                   y = names(poke_avg)[-1])

plots <- pmap(names_df, plot_line)

wrap_plots(plots, ncol = 3)

poke_avg_type <- pokemon |> 
  select(-c(id, species_id)) |> 
  group_by(generation_id, type_1) |> 
  summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE))) |> 
  mutate(generation_id = replace_na(generation_id, 10))

plot_line_type <- function(x, y, ...) {
  ggplot(poke_avg_type, aes(x = !!sym(x), y = !!sym(y), color = type_1, group = type_1)) +  
    geom_line() +
    theme_minimal()
}

plots_type <- pmap(names_df, plot_line_type)

wrap_plots(plots_type, ncol = 3)

