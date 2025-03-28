library(dplyr)
library(ggplot2)
library(tidytext)
library(tidyr)

tuesdata <- tidytuesdayR::tt_load(2025, week = 12)

report_words <- tuesdata$report_words_clean

report_words |> 
  group_by(year, word) |>
  summarise(n = n()) |> 
  arrange(desc(n))

top_words <- report_words |> 
  count(word) |> 
  arrange(desc(n)) |> 
  top_n(9) |> 
  pull(word)

report_words |> 
  filter(word %in% top_words) |> 
  group_by(year, word) |> 
  summarise(n = n()) |> 
  ungroup() |> 
  mutate(word = as.factor(word)) |> 
  ggplot(aes(year, n, color = word)) +
  geom_line() +
  scale_color_brewer(type = "qual", palette = "Set1") +
  theme_minimal()

report_words |>
  inner_join(get_sentiments("nrc"), relationship = "many-to-many") |> 
  count(year, sentiment) |> 
  arrange(desc(n))

top_sent <- report_words |>
  inner_join(get_sentiments("nrc"), relationship = "many-to-many") |> 
  count(sentiment) |> 
  arrange(desc(n)) |> 
  top_n(9) |> 
  pull(sentiment)

report_words|>
  inner_join(get_sentiments("nrc"), relationship = "many-to-many") |> 
  filter(sentiment %in% top_sent) |> 
  count(year, sentiment) |> 
  ggplot(aes(year, n, color = as.factor(sentiment))) +
  geom_line() +
  scale_color_brewer(type = "qual", palette = "Paired") +
  theme_minimal()

report_words |> 
  inner_join(get_sentiments("bing")) |> 
  count(year, sentiment) |> 
  pivot_wider(id_cols = year, names_from = sentiment, values_from = n) |> 
  mutate(net = positive - negative,
         negative = -negative) |> 
  pivot_longer(cols = -year, names_to = "sentiment", values_to = "count") |> 
  mutate(sentiment = factor(sentiment, levels = c("positive", "negative", "net"))) |> 
  ggplot(aes(year, count, fill = sentiment)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("darkgreen", "darkred", "darkblue")) +
  theme_minimal()

report_words |> 
  inner_join(get_sentiments("bing")) |> 
  count(year, sentiment) |> 
  pivot_wider(id_cols = year, names_from = sentiment, values_from = n) |> 
  mutate(net = positive - negative) |> 
  pivot_longer(cols = -year, names_to = "sentiment", values_to = "count") |> 
  mutate(sentiment = factor(sentiment, levels = c("positive", "negative", "net"))) |> 
  ggplot(aes(year, count, fill = sentiment)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("darkgreen", "darkred", "darkblue")) +
  theme_minimal() +
  facet_wrap(~ sentiment, scales = "free_y")
