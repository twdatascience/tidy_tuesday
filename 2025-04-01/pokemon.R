library(dplyr)
library(ggplot2)
library(tidyr)
library(patchwork)
library(purrr)
library(forcats)
library(shadowtext)

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


pokemon |> 
  group_by(generation_id, type_1, color_1) |> 
  summarise(n = n()) |> 
  arrange(desc(n))

pokemon |> 
  filter(generation_id == 1) |> 
  count(type_1, color_1)

pokemon |> 
  count(type_1, color_1) |> 
  arrange(desc(n))

pokemon |> 
  filter(type_1 == "flying")

pokemon |> 
  count(type_1)

pokemon |> 
  count(type_2) |> 
  arrange(desc(n))

pokemon |> 
  filter(!is.na(type_2)) |> 
  count(type_1, type_2) |> 
  arrange(desc(n))

pokemon_gens <- pokemon |> 
  filter(!is.na(generation_id))

pokemon_gens |> 
  count(type_1) |> 
  arrange(desc(n))

pokemon_gens |> 
  count(type_2) |> 
  arrange(desc(n))

pokemon_gens |> 
  mutate(avg_hhds = (hp + attack + defense + speed)/4,
         avg_sp = (special_attack + special_defense)/2,
         avg_stats = (hp + attack + defense + speed + special_attack + special_defense)/6) |> 
  select(pokemon, starts_with("avg"), color_1, type_1) |> 
  # pivot_longer(cols = -pokemon)
  ggplot(aes(avg_stats, avg_hhds, color = color_1)) +
  geom_point() +
  facet_wrap(~ type_1) +
  scale_color_identity() +
  theme_minimal()

pokemon_gens |> 
  select(pokemon, hp, attack, defense, speed, special_attack, 
         special_defense, base_experience, color_1, type_1, type_2) |> 
  ggplot(aes(base_experience, hp, color = color_1)) +
  geom_point() +
  facet_wrap(~ type_1) +
  scale_color_identity() +
  theme_minimal()

pokemon_gens |> 
  select(pokemon, hp, attack, defense, speed, special_attack, 
         special_defense, base_experience, color_2, type_1, type_2) |> 
  ggplot(aes(base_experience, hp, color = color_2)) +
  geom_point() +
  facet_wrap(~ type_2) +
  scale_color_identity() +
  theme_minimal()

pokemon_gens |> 
  select(pokemon, hp, attack, defense, speed, special_attack, 
         special_defense, base_experience, color_1, type_1, type_2) |> 
  ggplot(aes(hp, defense, color = color_1)) +
  geom_point() +
  facet_wrap(~ type_1) +
  scale_color_identity() +
  theme_minimal()

pokemon_gens |> 
  filter(type_1 == "bug") |> 
  arrange(desc(defense))


top_exp <- pokemon_gens |> 
  slice_max(base_experience, n = 10)

top_hp <- pokemon_gens |> 
  slice_max(hp, n = 10)

top_attack <- pokemon_gens |> 
  slice_max(attack, n = 10)

top_defense <- pokemon_gens |> 
  slice_max(defense, n = 10)

top_speed <- pokemon_gens |> 
  slice_max(speed, n = 10)

top_sp_attack <- pokemon_gens |> 
  slice_max(special_attack, n = 10)

top_sp_defense <- pokemon_gens |> 
  slice_max(special_defense, n = 10)

top_all <- top_exp |> 
  full_join(top_hp) |> 
  full_join(top_attack) |> 
  full_join(top_defense) |> 
  full_join(top_speed) |> 
  full_join(top_sp_attack) |> 
  full_join(top_sp_defense)

plot_scatter <- function(x, y, ...) {
  ggplot(top_all, aes(x = !!sym(x), y = !!sym(y), color = color_1)) +  
    geom_point() +
    # facet_wrap(~ type_1) +
    scale_color_identity() +
    theme_minimal()
}

combinations <- combn(c("base_experience", "hp", "attack", "defense", 
                        "speed", "special_attack", "special_defense"), 2)

combinations_df <- as.data.frame(t(combinations)) |> 
  rename(x = V1, y = V2)

plots <- pmap(combinations_df, plot_scatter)

wrap_plots(plots, ncol = 3)


pokemon_gens |> 
  mutate(effective_def = (hp * ((defense + special_defense)/2))**0.5) |> 
  select(pokemon, effective_def, color_1, type_1) |> 
  ggplot(aes(pokemon, effective_def, fill = color_1)) +
  geom_col() +
  facet_wrap(~ type_1) +
  scale_fill_identity()


# Compute effective defense
pokemon_gens_eff <- pokemon_gens |> 
  mutate(effective_def = hp * ((defense + special_defense) / 2)) |> 
  mutate(effective_def = (effective_def - min(effective_def)) / 
           (max(effective_def) - min(effective_def)) * 100) |> 
  select(pokemon, effective_def, color_1, type_1)

# Create a histogram for each type
plots <- pokemon_gens_eff |> 
  mutate(pokemon = stringr::str_to_title(pokemon)) |> 
  group_by(type_1) |> 
  slice_max(effective_def, n = 10) |> 
  arrange(desc(effective_def)) |> 
  group_split() |> 
  lapply(function(df) {
    df |> 
      mutate(pokemon = fct_reorder(pokemon, effective_def, .desc = TRUE)) |> 
      ggplot(aes(pokemon, effective_def, fill = color_1)) +
      geom_col() +
      scale_fill_identity() +
      ggtitle(stringr::str_to_title(unique(df$type_1))) +
      theme_minimal() +
      labs(x = NULL,
           y = NULL) +
      theme(axis.text.x = element_text(size = 4.5, angle = 33, hjust = 1),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "#E0F2B6", color = NA),
            plot.title = element_text(family = "Nimbus Sans", color = "#3E7614"),
            axis.text = element_text(family = "Nimbus Sans", color = "#3E7614")
      )
  })

# Combine histograms using patchwork
combined_plot <- wrap_plots(plots, ncol = 6) + 
  plot_annotation(title = "Top Relative Effective Defense by Pokémon Type",
                  caption = "Releative Effective Defense calculated by min-max normalizing product of mean defense stats and hp",
                  theme = theme(plot.background = element_rect(fill = "#E0F2B6", color = NA),
                                plot.title = element_text(family = "Nimbus Sans", face = "bold", color = "#3E7614"),
                                plot.caption = element_text(color = "#3E7614")))

# Display the combined plot
combined_plot

ggsave("effective_defense.png", combined_plot, device = "png", path = "./2025-04-01/", dpi = 800, height = 4500, width = 7000, units = "px")
