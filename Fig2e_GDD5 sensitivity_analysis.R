# Load required libraries
library(ggplot2)
library(dplyr)
library(broom)  # For tidying regression output
library(grid)

# Read input data
df <- read.csv("data/3cities_doy_weather_1940_2024.csv")

# Check for required column
if (!"GDD5_cum" %in% colnames(df)) {
  stop("Column 'GDD5_cum' not found in the dataset. Please check the input file.")
}

# Define new periods: 1942–1971 and 2004–2024
df <- df %>%
  filter(Year >= 1942) %>%
  mutate(Period = case_when(
    Year >= 1942 & Year <= 1971 ~ "1942–1971",
    Year >= 1995 & Year <= 2024 ~ "1995–2024",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(Period))  # Remove rows not in either period

# Run linear regression bloom_doy ~ GDD5_cum for each city and period
results_list <- list()


# Load necessary libraries
library(ggpmisc)

# Ensure 'Period' is treated as factor
df$Period <- as.factor(df$Period)

# Create scatter plot with regression fits and equation labels per Period
p <- ggplot(df, aes(x = GDD5_cum, y = bloom_doy, color = Period)) +
  geom_point(alpha = 0.9) +
  geom_smooth(method = "lm", se = FALSE, size = 1.2, aes(group = Period)) +
  stat_poly_eq(
    aes(label = paste(..eq.label..,  ..p.value.label.., sep = "~~~"),
        group = Period, color = Period),
    formula = y ~ x,
    parse = TRUE,
    size = 5.5,
    show.legend = FALSE
  ) +
  facet_wrap(~ City) +
  scale_y_continuous(limits=c(70,130))+
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.1)))+
  labs(title = "Sensitivity of Bloom DOY to GDD5_cum",
       x = "Accumulated GDD5 (°C)",
       y = "Bloom DOY") +
  theme_minimal() +
  theme(
    legend.position = "top",
    legend.title = element_text(size=18),
    legend.text = element_text(size=18),
    plot.title = element_text(size = 22, face = "bold"),
    axis.title = element_text(size = 20, face = "bold"),
    axis.text = element_text(size=16),
    strip.text = element_text(size = 16),
    panel.grid = element_blank(),
    panel.spacing = unit(1.5, "lines")
    
  )

# Print the plot
print(p)
# Save high-quality TIFF image of the plot
ggsave("Fig2e-GDD5_slope.tiff", plot = p, dpi = 300, width = 12, height = 6, units = "in", compression = "lzw")
