# Load required libraries
library(ggplot2)
library(dplyr)
library(broom)  # For tidying regression output

# Read input data
df <- read.csv("data/3cities_doy_weather_1940_2024.csv")

# Check for required column
if (!"GDD5_cum" %in% colnames(df)) {
  stop("Column 'GDD5_cum' not found in the dataset. Please check the input file.")
}

# Define new periods: 1942–1962 and 2004–2024
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

for (city in unique(df$City)) {
  for (period in unique(df$Period)) {
    subset_data <- df %>% filter(City == city, Period == period)
    
    if (nrow(subset_data) > 5) {
      model <- lm(bloom_doy ~ GDD5_cum, data = subset_data)
      model_summary <- summary(model)
      
      results_list[[paste(city, period, sep = "_")]] <- data.frame(
        City = city,
        Period = period,
        Slope = round(coef(model)[2], 3),
        Std.Error = round(coef(summary(model))[2, 2], 3),
        R_squared = round(model_summary$r.squared, 3),
        P_value = round(coef(summary(model))[2, 4], 3),
        Num_Samples = nrow(subset_data)
      )
    }
  }
}

# Combine results into a table
results_df <- do.call(rbind, results_list)
print(results_df)
write.csv(results_df, "Table_S3-Linear_Regression_Results_2Periods.csv", row.names = FALSE)


# Add t-test to compare slopes between two periods within the same city

# Store slope comparison results
slope_comparison_list <- list()

# Loop through each city
for (city in unique(df$City)) {
  # Extract results for both periods
  result_early <- results_df %>% filter(City == city, Period == "1942–1971")
  result_late  <- results_df %>% filter(City == city, Period == "1995–2024")
  
  if (nrow(result_early) == 1 && nrow(result_late) == 1) {
    # Extract slope and SE
    b1 <- result_early$Slope
    b2 <- result_late$Slope
    se1 <- result_early$Std.Error
    se2 <- result_late$Std.Error
    
    # Compute t-statistic for difference in slopes
    t_value <- (b1 - b2) / sqrt(se1^2 + se2^2)
    
    # Degrees of freedom (approximate as smaller of n1-2 and n2-2)
    df <- min(result_early$Num_Samples - 2, result_late$Num_Samples - 2)
    
    # Two-sided p-value
    p_value <- 2 * pt(-abs(t_value), df)
    
    # Save results
    slope_comparison_list[[city]] <- data.frame(
      City = city,
      Slope_1942_1971 = round(b1,2),
      SE_1942_1975 = round(se1,2),
      Slope_1995_2024 = round(b2,2),
      SE_1976_2024 = round(se2,2),
      t_value = round(t_value, 2),
      df = df,
      P_value = round(p_value, 2)
    )
  }
}

# Combine and print all comparison results
slope_comparison_df <- do.call(rbind, slope_comparison_list)
print(slope_comparison_df)

# Optional: save to CSV
write.csv(slope_comparison_df, "Table_S3-Slope_Comparison_t_test.csv", row.names = FALSE)







# Load necessary libraries
library(ggpmisc)

# Ensure 'Period' is treated as factor
df$Period <- as.factor(df$Period)

# Create scatter plot with regression fits and equation labels per Period
p <- ggplot(df, aes(x = GDD5_cum, y = bloom_doy, color = Period)) +
  geom_point(alpha = 0.9) +
  geom_smooth(method = "lm", se = FALSE, size = 1.2, aes(group = Period)) +
  stat_poly_eq(
    aes(label = paste(..eq.label.., ..rr.label.., ..p.value.label.., sep = "~~~"),
        group = Period, color = Period),
    formula = y ~ x,
    parse = TRUE,
    size = 3.5,
    show.legend = FALSE
  ) +
  facet_wrap(~ City) +
  scale_y_continuous(limits=c(70,130))+
  labs(title = "Sensitivity of Bloom DOY to GDD5_cum",
       x = "Accumulated GDD5 (°C)",
       y = "Bloom DOY") +
  theme_minimal() +
  theme(
    legend.position = "top",
    legend.title = element_text(size=12),
    legend.text = element_text(size=12),
    plot.title = element_text(size = 18, face = "bold"),
    axis.title = element_text(size = 16, face = "bold"),
    axis.text = element_text(size=10),
    panel.grid = element_blank()
  )

# Print the plot
print(p)
# Save high-quality TIFF image of the plot
ggsave("FigS9-GDD5_slope.tiff", plot = p, dpi = 300, width = 12, height = 6, units = "in", compression = "lzw")
