# -------------------- Required Libraries -------------------- #
library(ggplot2)
library(dplyr)
library(gridExtra)

# -------------------- Load and Prepare Data -------------------- #
file_path <- "data/other_cities_weather_data_330city_average_with_bloom.csv"
data <- read.csv(file_path)

# Select relevant variables
selected_vars <- c("TMAX", "WIND", "EVPR", "GDD5_cum", "CODE0", "CODE2", 
                   "CODE51", "CODE63", "lat", "alt", "bloom_doy")
data <- data[, selected_vars]
data <- na.omit(data)

# -------------------- GDD5_cum Grouped Analysis by Threshold -------------------- #

# Create GDD group (≤160 vs >160)
data <- data %>% mutate(GDD_group = ifelse(GDD5_cum <= 160, "Low GDD", "High GDD"))

# Fit linear models by GDD group
model_low_gdd <- lm(bloom_doy ~ TMAX, data = filter(data, GDD_group == "Low GDD"))
model_high_gdd <- lm(bloom_doy ~ TMAX, data = filter(data, GDD_group == "High GDD"))

# Create equation labels
eq_low <- paste0("Low GDD5 < 160: y = ", round(coef(model_low_gdd)[2], 2), 
                 "x + ", round(coef(model_low_gdd)[1], 1))
eq_high <- paste0("High GDD5 > 160: y = ", round(coef(model_high_gdd)[2], 2), 
                  "x + ", round(coef(model_high_gdd)[1], 1))

# -------------------- Plot 1: TMAX vs DOY by GDD Group -------------------- #

# Create scatter plot with regression lines and equations
p1 <- ggplot(data, aes(x = TMAX, y = bloom_doy, color = GDD_group)) +
  geom_point(alpha = 0.4, size = 2) +
  geom_smooth(method = "lm", se = TRUE, size = 1.2) +
  annotate("text", x = Inf, y = Inf, label = eq_high, hjust = 1.1, vjust = 2,
           color = "#F8766D", size = 5, fontface = "bold") +
  annotate("text", x = Inf, y = Inf, label = eq_low, hjust = 1.1, vjust = 4,
           color = "#00BFC4", size = 5, fontface = "bold") +
  labs(
    title = "",
    x = "Maximum Temperature (°C)",
    y = "Day of Year (DOY)",
    color = "GDD Group"
  ) +
  theme_classic(base_size = 16) +
  theme(
    axis.title = element_text(size = 18, face = "bold"),
    axis.text = element_text(size = 16, color = "black"),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 16),
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5)
  )
p1
# Save plot 1 as FigS13
ggsave("FigS13_Effect_of_TMAX_on_DOY_by_GDD.tiff", plot = p1,
       width = 10, height = 7, dpi = 300, units = "in", compression = "lzw")

# -------------------- Plot 2: GDD5_cum vs DOY by Elevation Group -------------------- #

# Create elevation group if not already done
data <- data %>% mutate(ALT_group = ifelse(alt <= 500, "Low Elevation", "High Elevation"))

# Fit linear models
model_low_alt <- lm(bloom_doy ~ GDD5_cum, data = filter(data, ALT_group == "Low Elevation"))
model_high_alt <- lm(bloom_doy ~ GDD5_cum, data = filter(data, ALT_group == "High Elevation"))

# Generate equation labels
eq_low_alt <- paste0("Low Elevation ≤ 500m: y = ", round(coef(model_low_alt)[2], 2),
                     "x + ", round(coef(model_low_alt)[1], 1))
eq_high_alt <- paste0("High Elevation > 500m: y = ", round(coef(model_high_alt)[2], 2),
                      "x + ", round(coef(model_high_alt)[1], 1))

# Create the second plot
p2 <- ggplot(data, aes(x = GDD5_cum, y = bloom_doy, color = ALT_group)) +
  geom_point(alpha = 0.4, size = 2) +
  geom_smooth(method = "lm", se = TRUE, size = 1.2) +
  annotate("text", x = Inf, y = Inf, label = eq_low_alt, hjust = 1.1, vjust = 4,
           color = "#00BFC4", size = 5, fontface = "bold") +
  annotate("text", x = Inf, y = Inf, label = eq_high_alt, hjust = 1.1, vjust = 2,
           color = "#F8766D", size = 5, fontface = "bold") +
  labs(
    title = " ",
    x = "Accumulated GDD5 (°C·d)",
    y = "Day of Year (DOY)",
    color = "Elevation Group"
  ) +
  theme_classic(base_size = 16) +
  theme(
    axis.title = element_text(size = 18, face = "bold"),
    axis.text = element_text(size = 16, color = "black"),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 16),
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5)
  )

# Save plot 2 as FigS14
ggsave("FigS14_Effect_of_GDD5_on_DOY_by_Elevation.tiff", plot = p2,
       width = 10, height = 7, dpi = 300, units = "in", compression = "lzw")
