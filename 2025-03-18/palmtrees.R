library(tidyverse)
library(ggfortify)
library(patchwork)

tuesdata <- tidytuesdayR::tt_load(2025, week = 11)

palmtrees <- tuesdata$palmtrees

theme_set(theme_minimal())

glimpse(palmtrees)

palmtrees |> 
  ggplot(aes(max_leaf_number, max__blade__length_m, color = spec_name)) +
  geom_point() +
  guides(color = "none")

palmtrees |> 
  ggplot(aes(max_stem_dia_cm, max_stem_height_m, color = spec_name)) +
  geom_point() +
  guides(color = "none") +
  scale_x_log10() +
  scale_y_log10()

hold <- palmtrees |> drop_na()

pca_result <- hold %>%
  select(where(is.numeric)) %>%  
  scale() %>%                    
  prcomp(center = TRUE, scale. = TRUE)

summary(pca_result)

autoplot(pca_result, data = hold, colour = 'spec_name', size = 3) +
  guides(color = "none")

autoplot(pca_result, data = hold, colour = 'palm_tribe', size = 3) +
  guides(color = "none")

num_vals <- palmtrees |> 
  select(where(is.numeric)) |> 
  names()

combinations <- combn(num_vals, 2)

combinations_df <- as.data.frame(t(combinations)) |> 
  rename(x = V1, y = V2)

plot_scatter <- function(x, y, ...) {
  data_df <- palmtrees |> 
    filter(!is.na(!!sym(x)), !is.na(!!sym(y)))  
  
  ggplot(data_df, aes(x = !!sym(x), y = !!sym(y), color = !!sym("spec_name"))) +  
    geom_point(alpha = 0.2) +
    # ggtitle(paste("Scatter Plot of", x, "vs", y)) +
    guides(color = "none") +
    theme_void()
}


# plot_scatter(palmtrees, combinations_df[1, 1], combinations_df[1, 2], "spec_name")
# Generate and print plots
plots <- pmap(combinations_df, plot_scatter)

wrap_plots(plots, ncol = 6, nrow = 11)

plot_scatter <- function(x, y, ...) {
  data_df <- palmtrees |> 
    filter(!is.na(!!sym(x)), !is.na(!!sym(y)))  
  
  ggplot(data_df, aes(x = !!sym(x), y = !!sym(y), color = !!sym("palm_tribe"))) +  
    geom_point(alpha = 0.2) +
    # ggtitle(paste("Scatter Plot of", x, "vs", y)) +
    guides(color = "none") +
    theme_void()
}


# plot_scatter(palmtrees, combinations_df[1, 1], combinations_df[1, 2], "spec_name")
# Generate and print plots
plots <- pmap(combinations_df, plot_scatter)

wrap_plots(plots, ncol = 6, nrow = 11)
