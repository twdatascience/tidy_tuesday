library(tidyverse)
library(patchwork)
library(sf)
library(tigris)

# functions
county_agg <- function(df_big, df_filter) {
  out1 <- df_big |>
    filter(
      state == df_filter$state & county == df_filter$county,
      is_nibrs == TRUE
    )
  out2 <- df_big |>
    filter(
      state == df_filter$state & county == df_filter$county,
      is_nibrs == FALSE
    )
  return(c(nrow(out1), nrow(out2)))
}

state_agg <- function(df_big, df_filter) {
  out1 <- df_big |>
    filter(
      state == df_filter$state,
      is_nibrs == TRUE
    )
  out2 <- df_big |>
    filter(
      state == df_filter$state,
      is_nibrs == FALSE
    )
  return(c(nrow(out1), nrow(out2)))
}

# set up
tuesdata <- tidytuesdayR::tt_load('2025-02-18')

agencies <- tuesdata$agencies

# explore data
glimpse(agencies)

# which counties have the most agencies?
agencies |>
  group_by(state, county) |>
  summarise(n = n()) |>
  arrange(desc(n))

# split between reporting and non?
agencies |>
  group_by(state, county, is_nibrs) |>
  summarise(n = n())

agencies |>
  group_by(is_nibrs) |>
  summarise(n = n())

agencies |>
  select(state, county, is_nibrs) |>
  arrange(state, county)

# look at which counties have highest/lowest percentage of agencies reporting to NIBRS

# combine counties with commas to link with geo data later
state_counties <- agencies |>
  select(state, county, state_abbr) |>
  mutate(county = str_extract(county, "^[^,]*")) |>
  arrange(state, county) |>
  unique()

# create a new tibble for aggregated data
df <- tibble(
  state = character(),
  county = character(),
  in_nibrs = integer(),
  not_nibrs = integer()
)

for (i in 1:nrow(state_counties)) {
  count_new <- county_agg(agencies, state_counties[i, ])
  df_new <- tibble(
    state = state_counties[i, 1]$state,
    state_abbr = state_counties[i, 3]$state_abbr,
    county = state_counties[i, 2]$county,
    in_nibrs = count_new[1],
    not_nibrs = count_new[2]
  )
  df <- bind_rows(df, df_new)
}

# Get county boundaries
counties <- counties(cb = TRUE)

df <- df |>
  mutate(county = str_remove(county, "REGION"))

counties <- counties |>
  mutate(NAME = str_to_upper(str_remove(NAME, "\\.")))

counties_merged <- counties %>%
  full_join(
    df,
    by = join_by(STUSPS == state_abbr, NAME == county),
    relationship = "many-to-many"
  ) |>
  # remove "Not Specified" counties and anything that doesn't have a geometry
  filter(!is.na(state))


# Make Maps
# used this blog as reference for legends:
# https://www.andrewheiss.com/blog/2025/02/19/ggplot-histogram-legend/#bonus-2-use-a-diverging-color-scheme-nested-legend-circles

# legend plot
county_legend <- df |>
  mutate(percent = in_nibrs / (in_nibrs + not_nibrs)) |>
  ggplot() +
  geom_histogram(
    aes(percent, fill = after_stat(x)),
    binwidth = 0.05,
    boundary = 0,
    color = "black",
    linewidth = 0.1
  ) +
  scale_fill_stepsn(
    colors = scales::viridis_pal(option = "A")(9),
    breaks = 1:10 / 10,
    guide = "none"
  ) +
  scale_x_continuous(breaks = 0:5 / 5, labels = (0:5 / 5) * 100) +
  scale_y_log10() +
  labs(x = "NIBRS Percent") +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(size = rel(0.5)),
    axis.title.x = element_text(size = rel(0.68), face = "bold"),
    axis.text.y = element_text(size = rel(0.5)),
    axis.title.y = element_blank()
  )

# county map
county_map <- counties_merged |>
  mutate(percent = in_nibrs / (in_nibrs + not_nibrs)) |>
  ggplot() +
  geom_sf(aes(fill = percent)) +
  coord_sf(
    xlim = c(-125.0011, -66.9326),
    ylim = c(24.9493, 49.5904)
  ) +
  scale_fill_stepsn(
    colors = scales::viridis_pal(option = "A")(9),
    breaks = 1:10 / 10,
    guide = "none"
  ) +
  theme_void() +
  labs(title = "US Counties NIBRS Reporting Choropleth Map") +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", hjust = 0.13, size = rel(1.4))
  )

county_map +
  inset_element(
    county_legend,
    left = 0.75,
    bottom = 0,
    right = 0.98,
    top = 0.3
  )

# look at which states have highest/lowest percentage of agencies reporting to NIBRS

states_data <- agencies |>
  select(state, state_abbr) |>
  arrange(state) |>
  unique()

df2 <- tibble(
  state = character(),
  county = character(),
  in_nibrs = integer(),
  not_nibrs = integer()
)

for (i in 1:nrow(states_data)) {
  count_new <- state_agg(agencies, states_data[i, ])
  df_new <- tibble(
    state = states_data[i, 1]$state,
    state_abbr = states_data[i, 2]$state_abbr,
    in_nibrs = count_new[1],
    not_nibrs = count_new[2]
  )
  df2 <- bind_rows(df2, df_new)
}

states <- states()

states_merged <- states |>
  full_join(
    df2,
    by = join_by(STUSPS == state_abbr)
  ) |>
  # remove rows not in agency data
  filter(!is.na(state))

# Create map
# states legend
states_legend <- df2 |>
  mutate(percent = in_nibrs / (in_nibrs + not_nibrs)) |>
  ggplot() +
  geom_histogram(
    aes(percent, fill = after_stat(x)),
    binwidth = 0.05,
    boundary = 0,
    color = "black",
    linewidth = 0.1
  ) +
  scale_fill_stepsn(
    colors = scales::viridis_pal(option = "A")(9),
    breaks = 1:10 / 10,
    guide = "none"
  ) +
  scale_x_continuous(breaks = 0:5 / 5, labels = (0:5 / 5) * 100) +
  labs(x = "NIBRS Percent") +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(size = rel(0.5)),
    axis.title.x = element_text(size = rel(0.68), face = "bold"),
    axis.text.y = element_blank(),
    axis.title.y = element_blank()
  )


states_map <- states_merged |>
  mutate(percent = in_nibrs / (in_nibrs + not_nibrs)) |>
  ggplot() +
  geom_sf(aes(fill = percent)) +
  coord_sf(
    xlim = c(-125.0011, -66.9326),
    ylim = c(24.9493, 49.5904)
  ) +
  scale_fill_stepsn(
    colors = scales::viridis_pal(option = "A")(9),
    breaks = 1:10 / 10,
    guide = "none"
  ) +
  theme_void() +
  labs(title = "US States NIBRS Reporting Choropleth Map")

states_map +
  inset_element(
    states_legend,
    left = 0.75,
    bottom = 0,
    right = 0.98,
    top = 0.3
  )
