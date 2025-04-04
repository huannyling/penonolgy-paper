library(ggplot2)
library(ggpubr)
library(readr)

# Load the dataset
data <- read_csv("data/co2_doy_kyoto_correlation.csv")

# Fit a linear regression model
model <- lm(DOY ~ CO2, data = data)
summary_model <- summary(model)

# Extract model coefficients and statistics
slope <- round(coef(model)[2], 3)
intercept <- round(coef(model)[1], 1)
r_squared <- round(summary_model$r.squared, 2)
p_value <- signif(summary_model$coefficients[2, 4], 3)

# Create annotation text with regression equation and R²
eqn_text <- paste0(
  "DOY = ", intercept, " + ", slope, " × CO2\n",
  "R² = ", r_squared, ", p < 0.01 "
)

# Create the scatter plot with regression line and annotation
p <- ggplot(data, aes(x = CO2, y = DOY)) +
  geom_point(color = "#2C3E50", size = 2, alpha = 0.7) +  # Scatter points
  geom_smooth(method = "lm", se = TRUE, color = "#E74C3C", size = 1) +  # Linear regression line with CI
  annotate("text", 
           x = min(data$CO2, na.rm = TRUE) + 1, 
           y = 128, 
           label = eqn_text, 
           hjust = 0, size = 5, family = "mono") +  # Display model stats
  # Optional: show Pearson correlation as well
  # stat_cor(method = "pearson", label.x = min(data$CO2), label.y = 125, size = 5) +
  theme_minimal(base_size = 14) +  # Clean background
  scale_y_continuous(limits = c(70, 130)) +  # Y-axis limits
  labs(
    x = expression("Atmospheric CO"[2]*" Concentration (ppm)"),
    y = "Flowering Date (DOY)"
  ) +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold", size = 18),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14)
  )

# Display the plot
print(p)

# Optional: Save to file
 ggsave("FigS8_CO2_DOY_Kyoto_plot.tiff", p, width = 6, height = 5, dpi = 300)
