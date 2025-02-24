library(tidyverse)

tuesdata <- tidytuesdayR::tt_load('2025-01-21')

exped_tidy <- tuesdata$exped_tidy
peaks_tidy <- tuesdata$peaks_tidy

# explore
glimpse(exped_tidy)
glimpse(peaks_tidy)

# quick graph of peaks by year/season

exped_tidy %>%
    group_by(PEAKID, YEAR, SEASON_FACTOR) %>%
    summarise(COUNT = n()) %>%
    ungroup() %>%
    ggplot(aes(x = YEAR, y = COUNT, fill = SEASON_FACTOR)) +
    geom_col() +
    facet_wrap(~PEAKID)


popular <- exped_tidy %>%
    group_by(PEAKID) %>%
    summarise(COUNT = n()) %>%
    filter(COUNT > 10)

unpopular <- exped_tidy %>%
    group_by(PEAKID) %>%
    summarise(COUNT = n()) %>%
    filter(COUNT <= 10)

exped_tidy %>%
    filter(PEAKID %in% popular$PEAKID) %>%
    group_by(PEAKID, YEAR, SEASON_FACTOR) %>%
    summarise(COUNT = n()) %>%
    ungroup() %>%
    ggplot(aes(x = YEAR, y = COUNT, fill = SEASON_FACTOR)) +
    geom_col() +
    facet_wrap(~PEAKID)

exped_tidy %>%
    filter(PEAKID %in% unpopular$PEAKID) %>%
    group_by(PEAKID, YEAR, SEASON_FACTOR) %>%
    summarise(COUNT = n()) %>%
    ungroup() %>%
    ggplot(aes(x = YEAR, y = COUNT, fill = SEASON_FACTOR)) +
    geom_col() +
    facet_wrap(~PEAKID)

exped_tidy %>%
    filter(PEAKID %in% popular$PEAKID) %>%
    left_join(
        peaks_tidy %>%
            select(PEAKID, PKNAME),
        by = "PEAKID"
    ) %>%
    group_by(PKNAME, YEAR, SEASON_FACTOR) %>%
    summarise(COUNT = n()) %>%
    ungroup() %>%
    ggplot(aes(x = YEAR, y = COUNT, fill = SEASON_FACTOR)) +
    geom_col(position = "dodge") +
    facet_wrap(~PKNAME)

exped_tidy %>%
    mutate(
        SUCCESS = case_when(
            SUCCESS1 == TRUE |
                SUCCESS2 == TRUE |
                SUCCESS3 == TRUE |
                SUCCESS4 == TRUE ~
                TRUE,
            .default = FALSE
        )
    ) %>%
    mutate(
        SEASON_SUCCESS = case_when(
            SUCCESS == TRUE ~ paste0(SEASON_FACTOR, "_1"),
            SUCCESS == FALSE ~ paste0(SEASON_FACTOR, "_0")
        )
    ) %>%
    select(PEAKID, YEAR, SEASON_SUCCESS) %>%
    filter(PEAKID %in% popular$PEAKID) %>%
    left_join(
        peaks_tidy %>%
            select(PEAKID, PKNAME),
        by = "PEAKID"
    ) %>%
    group_by(PKNAME, YEAR, SEASON_SUCCESS) %>%
    summarise(COUNT = n()) %>%
    ungroup() %>%
    ggplot(aes(x = YEAR, y = COUNT, fill = SEASON_SUCCESS)) +
    geom_col() +
    facet_wrap(~PKNAME) +
    scale_fill_brewer(type = "qual", palette = "Dark2") +
    theme_minimal()

# just look at 2022
exped_tidy %>%
    filter(YEAR == 2022) %>%
    mutate(
        SUCCESS = case_when(
            SUCCESS1 == TRUE |
                SUCCESS2 == TRUE |
                SUCCESS3 == TRUE |
                SUCCESS4 == TRUE ~
                TRUE,
            .default = FALSE
        )
    ) %>%
    select(PEAKID, SEASON_FACTOR, SUCCESS) %>%
    filter(PEAKID %in% popular$PEAKID) %>%
    left_join(
        peaks_tidy %>%
            select(PEAKID, PKNAME),
        by = "PEAKID"
    ) %>%
    group_by(PKNAME, SEASON_FACTOR, SUCCESS) %>%
    summarise(COUNT = n()) %>%
    ungroup() %>%
    ggplot(aes(x = SEASON_FACTOR, y = COUNT, fill = SUCCESS)) +
    geom_col() +
    facet_wrap(~PKNAME) +
    scale_fill_brewer(type = "qual", palette = "Dark2") +
    theme_minimal() +
    theme(
        panel.grid.major = element_blank(),
        panel.border = element_rect(fill = NA, color = "black", size = 1)
    )

# look at unpopular
exped_tidy %>%
    mutate(
        SUCCESS = case_when(
            SUCCESS1 == TRUE |
                SUCCESS2 == TRUE |
                SUCCESS3 == TRUE |
                SUCCESS4 == TRUE ~
                TRUE,
            .default = FALSE
        )
    ) %>%
    mutate(
        SEASON_SUCCESS = case_when(
            SUCCESS == TRUE ~ paste0(SEASON_FACTOR, "_1"),
            SUCCESS == FALSE ~ paste0(SEASON_FACTOR, "_0")
        )
    ) %>%
    select(PEAKID, YEAR, SEASON_SUCCESS) %>%
    filter(PEAKID %in% unpopular$PEAKID) %>%
    left_join(
        peaks_tidy %>%
            select(PEAKID, PKNAME),
        by = "PEAKID"
    ) %>%
    group_by(PKNAME, YEAR, SEASON_SUCCESS) %>%
    summarise(COUNT = n()) %>%
    ungroup() %>%
    ggplot(aes(x = YEAR, y = COUNT, fill = SEASON_SUCCESS)) +
    geom_col() +
    facet_wrap(~PKNAME) +
    scale_fill_brewer(type = "qual", palette = "Dark2") +
    theme_minimal()

# just look at 2022
exped_tidy %>%
    filter(YEAR == 2022) %>%
    mutate(
        SUCCESS = case_when(
            SUCCESS1 == TRUE |
                SUCCESS2 == TRUE |
                SUCCESS3 == TRUE |
                SUCCESS4 == TRUE ~
                TRUE,
            .default = FALSE
        )
    ) %>%
    select(PEAKID, SEASON_FACTOR, SUCCESS) %>%
    filter(PEAKID %in% unpopular$PEAKID) %>%
    left_join(
        peaks_tidy %>%
            select(PEAKID, PKNAME),
        by = "PEAKID"
    ) %>%
    group_by(PKNAME, SEASON_FACTOR, SUCCESS) %>%
    summarise(COUNT = n()) %>%
    ungroup() %>%
    ggplot(aes(x = SEASON_FACTOR, y = COUNT, fill = SUCCESS)) +
    geom_col() +
    facet_wrap(~PKNAME) +
    scale_fill_brewer(type = "qual", palette = "Dark2") +
    theme_minimal() +
    theme(
        panel.grid.major = element_blank(),
        panel.border = element_rect(fill = NA, color = "black", size = 1)
    )

# just group by peak and season combine years for unpopular
exped_tidy %>%
    mutate(
        SUCCESS = case_when(
            SUCCESS1 == TRUE |
                SUCCESS2 == TRUE |
                SUCCESS3 == TRUE |
                SUCCESS4 == TRUE ~
                TRUE,
            .default = FALSE
        )
    ) %>%
    select(PEAKID, SEASON_FACTOR, SUCCESS) %>%
    filter(PEAKID %in% unpopular$PEAKID) %>%
    left_join(
        peaks_tidy %>%
            select(PEAKID, PKNAME),
        by = "PEAKID"
    ) %>%
    group_by(PKNAME, SEASON_FACTOR, SUCCESS) %>%
    summarise(COUNT = n()) %>%
    ungroup() %>%
    ggplot(aes(x = SEASON_FACTOR, y = COUNT, fill = SUCCESS)) +
    geom_col() +
    facet_wrap(~PKNAME) +
    scale_fill_brewer(type = "qual", palette = "Dark2") +
    theme_minimal() +
    theme(
        panel.grid.major = element_blank(),
        panel.border = element_rect(fill = NA, color = "black", size = 1)
    )
