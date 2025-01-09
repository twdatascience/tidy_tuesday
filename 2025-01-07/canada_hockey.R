# 2024-01-09 substituted for first 2025
library(tidytuesdayR)
library(dplyr)
library(ggplot2)
library(gganimate)

tuesdata <- tidytuesdayR::tt_load('2024-01-09')

canada_births_1991_2022 <- tuesdata$canada_births_1991_2022
nhl_player_births <- tuesdata$nhl_player_births
nhl_rosters <- tuesdata$nhl_rosters
nhl_teams <- tuesdata$nhl_teams


# look at data - commentated out after
# View(canada_births_1991_2022)
# View(nhl_player_births)
# View(nhl_rosters)
# View(nhl_teams)

# combine nhl_rosters and nhl_teams

nhl <- nhl_rosters |>
  left_join(nhl_teams, by = "team_code") |> 
  relocate(full_name)

theme_set(theme_minimal())
# look at all canada births
canada_births_1991_2022 |> 
  mutate(month_name = lubridate::month(month, label = TRUE, abbr = FALSE)) |> 
  mutate(year = as.factor(year)) |>
  ggplot(aes(x = month_name, y = births, fill = year)) +
  geom_bar(position = "dodge", stat = "identity") +
  coord_cartesian(ylim = c(24500, 37000)) +
  theme(legend.position = "none")


hold <- canada_births_1991_2022 |> 
  mutate(year_an = as.integer(year),
         year = as.factor(year)) |>
  mutate(month_name = lubridate::month(month, label = TRUE, abbr = FALSE)) |>
  ggplot(aes(x = month_name, y = births, group = year, color = year)) +
  geom_line() +
  scale_color_manual(values = colorRampPalette(c("blue", "red"))(32)) +
  theme(legend.position = "none",
        axis.title = element_blank(),
        axis.text.x = element_text(size = 6),
        panel.grid.minor = element_blank()) +
  labs(title = "Year: {closest_state}") +
  transition_states(year)

animate(hold, res = 150, fps = 20, width = 10, height = 8, units = "in")



canada_births_1991_2022 |> 
  group_by(year) |>
  summarise(total_births = sum(births)) |> 
  ggplot(aes(x = year, y = total_births)) +
  geom_line(group = 1)
