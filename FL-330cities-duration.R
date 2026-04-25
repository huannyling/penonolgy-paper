library(tidyverse)

df <- read_csv("ohter_cities_combined_location_year_summary.csv")

glimpse(df)


df <- df %>%
  arrange(start_year) %>%
  mutate(location = factor(location, levels = unique(location)))


ggplot(df) +
  geom_segment(
    aes(
      x = start_year,
      xend = end_year,
      y = location,
      yend = location,
      color = location
    ),
    linewidth = 0.6,
    alpha = 0.7
  ) +
  scale_color_viridis_d(option = "turbo", guide = "none") +
  labs(
    x = "Year",
    y = "Location",
    title = "Temporal coverage of phenological records across locations"
  ) +
  theme_classic() +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    text = element_text(size = 14)
  )


geom_segment(
  aes(
    x = start_year,
    xend = end_year,
    y = location,
    yend = location,
    color = location
  ),
  linewidth = 0.8,
  alpha = 0.5
)
