library(shiny)
library(leaflet)
library(tidyverse)
library(leaflet.extras)
library(here)

# import data
actions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-18/actions.csv')
threats <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-18/threats.csv')

# data from https://developers.google.com/public-data/docs/canonical/countries_csv
# copied and pasted into text file then saved
countries <- read_delim(here('plant_map/data/countries.csv'), delim = '\t')

# combine data
actions %>% 
    left_join(threats, by = c("binomial_name", "country", "continent", "group", "year_last_seen", "red_list_category")) -> plants

plants %>% 
    mutate(country = case_when(
        country == 'Saint Helena, Ascension and Tristan da Cunha' ~ 'Saint Helena',
        country == 'Viet Nam' ~ 'Vietnam',
        TRUE ~ as.character(country)
    )) -> plants

countries %>% 
    select(-country) %>% 
    select(country = name, latitude, longitude) %>% 
    add_row(country = 'Sao Tome and Principe',
            latitude = 0.1864,
            longitude = 6.66131) %>% 
    mutate(country = case_when(
        country == 'Pitcairn Islands' ~ 'Pitcairn',
        country == 'Congo [DRC]' ~ 'Congo',
        country == 'Myanmar [Burma]' ~ 'Myanmar',
        country == 'Cape Verde' ~ 'Cabo Verde',
        TRUE ~ as.character(country)
    )) -> countries

plants %>% 
    full_join(countries, by = 'country') %>% 
    mutate(in_data = if_else(is.na(binomial_name), 'no', 'yes'))  -> plant_map

# Define UI for application that draws a histogram
ui <- fluidPage(
        mainPanel(
            # create space to display map
            leafletOutput(outputId = 'plants_map')
        )
    )

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    # create map
    output$plants_map <- renderLeaflet({
        leaflet(plant_map) %>% 
            addProviderTiles(providers$Stamen.TonerLite,
                             options = providerTileOptions(noWrap = TRUE)) %>% 
            addMarkers(~longitude, ~latitude, popup = ~as.character(in_data), label = as.character(~country))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
