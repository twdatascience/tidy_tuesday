tuesdata <- tidytuesdayR::tt_load(2025, week = 13)

pokemon <- tuesdata$pokemon_df

dplyr::glimpse(pokemon)
