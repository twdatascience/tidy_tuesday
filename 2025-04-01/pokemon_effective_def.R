library(dplyr)
library(ggplot2)
library(tidyr)
library(patchwork)
library(purrr)
library(forcats)
library(shadowtext)

tuesdata <- tidytuesdayR::tt_load(2025, week = 13)

pokemon <- tuesdata$pokemon_df

pokemon_gens <- pokemon |> 
  filter(!is.na(generation_id))


# Compute effective defense
# Rationale: In the game to defeat a pokemon you must lower the hp to zero.
# The hit to hp depends on the attacking pokemon's stats and sometimes 
# environmental stats. To represent just the defensive stats I averaged defense
# and special defense then multiplied by hp to get an effective defense. Then I
# normalized to 0-100 to fit a cognitive equivalent of other stats.
pokemon_gens_eff <- pokemon_gens |> 
  mutate(effective_def = hp * ((defense + special_defense) / 2)) |> 
  mutate(effective_def = (effective_def - min(effective_def)) / 
           (max(effective_def) - min(effective_def)) * 100) |> 
  select(pokemon, effective_def, color_1, type_1, generation_id)

# Create a ordered bar chart for each type with descending effective defense
# I used the color scheme from bulbapedia for background and text colors
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
      geom_text(aes(label = generation_id), 
                vjust = -0.5, 
                color = "#3E7614", 
                family = "Nato Sans Display", 
                fontface = "bold",
                size = 2) +
      scale_fill_identity() +
      ggtitle(stringr::str_to_title(unique(df$type_1))) +
      theme_minimal() +
      labs(x = NULL,
           y = NULL) +
      scale_y_continuous(breaks = seq(0, 100, 25), limits = c(0, 100), expand = expansion(add = c(0, 20))) +
      theme(axis.text.x = element_text(size = 4.5, angle = 33, hjust = 1),
            panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_line(color = "#3E7614", linewidth = 0.05),
            # panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "#E0F2B6", color = NA),
            plot.title = element_text(family = "Nato Sans Display", color = "#3E7614"),
            axis.text = element_text(family = "Nato Sans Display", color = "#3E7614")
      )
  })

# Combine histograms using patchwork
combined_plot <- wrap_plots(plots, ncol = 6) + 
  plot_annotation(
    title = "Top Relative Effective Defense by Pokémon Type",
    subtitle = "Generation number above bar",
    caption = paste0("Releative Effective Defense calculated by min-max ",
    "normalizing product of mean defense stats and hp", 
    "\nGeneratons 1-7"),
    theme = theme(
      plot.background = element_rect(fill = "#E0F2B6", color = NA),
      plot.title = element_text(family = "Nato Sans Display", 
                                face = "bold", 
                                color = "#3E7614"),
      plot.subtitle = element_text(family = "Nato Sans Display", 
                                   color = "#3E7614",
                                   size = 7),
      plot.caption = element_text(family = "Nato Sans Display",
                                  color = "#3E7614",
                                  size = 5)))

# Display the combined plot
combined_plot

# Save final graphic
ggsave("effective_defense.png", combined_plot, device = "png", path = "./2025-04-01/", dpi = 800, height = 4500, width = 7000, units = "px")
