# 2024-01-09 substituted for first 2025
library(tidytuesdayR)
library(dplyr)
library(ggplot2)
library(gganimate)
library(maps)
library(sf)
library(stringr)

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

animate(hold, res = 150)



canada_births_1991_2022 |> 
  group_by(year) |>
  summarise(total_births = sum(births)) |> 
  ggplot(aes(x = year, y = total_births)) +
  geom_line(group = 1)

# look at nhl_player_births
nhl_player_births |> 
  filter(birth_year > 1990) |>
  filter(birth_country %in% c("CAN", "USA")) |> 
  ggplot(aes(x = birth_year, fill = birth_country)) +
  geom_histogram(binwidth = 1, position = "dodge")

nhl_player_births |> 
  filter(birth_year > 1990) |>
  group_by(birth_country, birth_state_province) |>
  summarise(total_players = n()) |> 
  arrange(desc(total_players)) |> 
  ungroup() |> 
  filter(total_players > 10)

nhl_player_births |> 
  group_by(birth_city) |> 
  summarise(total_players = n()) |> 
  arrange(desc(total_players))



# just canada
nhl_can_cities <- nhl_player_births |> 
  filter(birth_country == "CAN") |>
  pull(birth_city) |>
  unique()
  
  
canada_map <- map_data("world", region = "Canada")

canada_map |> 
  ggplot(aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "lightblue", color = "white") +
  coord_fixed(1.3)

canada_provinces <- st_read("https://raw.githubusercontent.com/nvkelso/natural-earth-vector/master/geojson/ne_10m_admin_1_states_provinces.geojson") |> 
  filter(iso_a2 == "CA")

canada_provinces |>
  ggplot() +
  geom_sf() +
  coord_sf()

# only has cities in canada.cities
city_locs <- canada.cities |> 
    mutate(city = substr(name, 1, nchar(name) - 3),
           province = str_extract(name, "[^ ]+$")) |>
  filter(city %in% nhl_can_cities)

can_city_count <- nhl_player_births |> 
  filter(birth_city %in% city_locs$city) |>
  group_by(birth_city) |>
  summarise(total_players = n())

city_locs <- city_locs |>
  left_join(can_city_count, by = c("city" = "birth_city"))

canada_provinces |> 
  ggplot() +
  geom_sf() +
  geom_point(data = city_locs, 
             aes(x = long, y = lat, size = total_players), 
             shape = 21,
             fill = "red",
             stroke = 0.5,
             alpha = 0.3) +
  coord_sf() +
  labs(title = "NHL Player Birth Cities in Canada",
       size = "Total Players Born in City") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom")
