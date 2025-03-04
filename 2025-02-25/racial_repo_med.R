library(tidyverse)
library(tidytext)

tuesdata <- tidytuesdayR::tt_load('2025-02-25')

article_dat <- tuesdata$article_dat
model_dat <- tuesdata$model_dat

# Explore how race and ethnicity are categorized in these articles. Which categories are most prominent? Which are missing? How do sample sizes vary across groups?
# Has the sentiment of article titles, abstracts, keywords, and/or aims statements changed over time?
# What type of health outcomes have been studied? What disparities have been identified?

# eda
article_dat |>
  group_by(journal) |>
  summarise(n = n()) |>
  arrange(desc(n))

article_dat |>
  group_by(year) |>
  summarise(n = n()) |>
  arrange(desc(n))

article_dat |>
  group_by(month) |>
  summarise(n = n()) |>
  arrange(desc(n))

unique(article_dat$race1)

combined <- model_dat |>
  left_join(article_dat, by = join_by("doi"))

combined |>
  group_by(journal) |>
  summarise(n = n()) |>
  arrange(desc(n))

get_sentiments("nrc")

abstracts_tokens <- article_dat |>
  group_by(journal, title) |>
  unnest_tokens(word, abstract)

abstracts_tokens |>
  inner_join(get_sentiments("nrc")) |>
  count(sentiment, sort = T)

abstracts_tokens |>
  inner_join(get_sentiments("nrc")) |>
  ungroup() |>
  group_by(journal) |>
  count(sentiment, sort = T)

abstracts_tokens <- article_dat |>
  group_by(journal, year) |>
  unnest_tokens(word, abstract)

abstracts_tokens |>
  inner_join(get_sentiments("nrc"), relationship = "many-to-many") |>
  count(sentiment, sort = T) |>
  filter(n > 100) |>
  ggplot() +
  geom_col(aes(year, n, fill = sentiment), position = "dodge", width = 0.7) +
  facet_wrap(~journal)

article_dat |>
  group_by(journal, year) |>
  unnest_tokens(word, abstract) |>
  ungroup() |>
  inner_join(
    get_sentiments("nrc"),
    relationship = "many-to-many",
    by = join_by(word)
  ) |>
  count(year, journal, sentiment) |>
  ggplot(aes(x = as.factor(year), y = n, fill = as.factor(sentiment))) +
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.7,
    linewidth = 0.1
  ) +
  facet_wrap(~journal, scales = "free_y") +
  theme_minimal() +
  labs(x = "Year", y = "Count", fill = "Sentiment")

norm_mult <- article_dat |>
  filter(journal == "American journal of obstetrics and gynecology") |>
  group_by(year) |>
  summarise(n_year = n())


weighted_american_journal <- article_dat |>
  filter(journal == "American journal of obstetrics and gynecology") |>
  group_by(journal, year) |>
  unnest_tokens(word, abstract) |>
  ungroup() |>
  inner_join(
    get_sentiments("nrc"),
    relationship = "many-to-many",
    by = join_by(word)
  ) |>
  count(year, journal, sentiment) |>
  ungroup() |>
  left_join(norm_mult, by = "year") |>
  group_by(year) |>
  mutate(
    weighted_mean = sum(n * n_year) / sum(n_year),
    weighted_sd = sqrt(sum(n_year * (n - weighted_mean)^2) / sum(n_year)),
    normalized_value = (n - weighted_mean) / weighted_sd
  ) %>%
  ungroup()

weighted_american_journal |>
  ggplot(aes(
    x = as.factor(year),
    y = normalized_value,
    fill = as.factor(sentiment)
  )) +
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.7,
    color = "black",
    linewidth = 0.1
  ) +
  facet_wrap(~sentiment) +
  scale_fill_viridis_d(option = "B") +
  theme_minimal() +
  labs(x = "Year", y = "Relative use", fill = "Sentiment")
