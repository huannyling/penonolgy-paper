# -------------------- Load Required Libraries -------------------- #
library(ggplot2)
library(randomForest)
library(iml)         # For model interpretation (PDP, interactions)
library(ggpubr)      # For publication-quality plots
library(gridExtra)   # For combining plots

# -------------------- Load and Prepare Data -------------------- #
file_path <- "data/other_cities_weather_data_330city_average_with_bloom.csv"  # Replace with your actual path
data <- read.csv(file_path)

# Select only relevant predictors and the target variable
selected_vars <- c("TMAX", "WIND", "EVPR", "GDD5_cum", "CODE0", "CODE2", 
                   "CODE51", "CODE63", "lat", "alt", "bloom_doy")

data <- data[, selected_vars]
data <- na.omit(data)  # Remove rows with missing values

# -------------------- Train Random Forest Model -------------------- #
rf_model <- randomForest(bloom_doy ~ ., data = data, ntree = 500)

# Create Predictor object from the trained model
predictor <- Predictor$new(rf_model, data = data, y = data$bloom_doy)

# -------------------- Partial Dependence Plot (PDP) -------------------- #
# Compute partial dependence for GDD5_cum
pdp_gdd <- FeatureEffect$new(predictor, feature = "GDD5_cum", method = "pdp")

# Plot PDP with rug and LOESS smoothing
Fig3d <- ggplot(pdp_gdd$results, aes(x = GDD5_cum, y = .value)) +
  geom_line(color = "black", size = 1.2) +
  geom_rug(sides = "b", alpha = 0.3) +
  geom_smooth(method = "loess", color = "blue", se = TRUE, fill = "lightblue") +
  labs(
    title = "Partial Dependence Analysis",
    x = "Accumulated Growing Degree Days (GDD5_cum)",
    y = "Predicted Blooming Day (DOY)"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(hjust = 0, size = 18, face = "bold"),
    axis.title.x = element_text(size = 16, face = "bold"),
    axis.title.y = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

# Save the PDP figure
ggsave("FigS11_PDP_Spatial_Analysis.tiff", plot = Fig3d, dpi = 300, width = 7, height = 6, units = "in")

# -------------------- Interaction Strength Plot -------------------- #
# Compute feature interaction strengths
interaction_effect <- Interaction$new(predictor)

# Plot interaction strength
interaction_plot <- ggplot(interaction_effect$results, aes(x = reorder(.feature, .interaction), y = .interaction)) +
  geom_segment(aes(xend = reorder(.feature, .interaction), y = 0, yend = .interaction),
               color = "black", linewidth = 1) +
  geom_point(size = 3, color = "black") +
  coord_flip() +
  labs(
    title = "Interaction Strength of Predictors",
    x = "Features",
    y = "Overall Interaction Strength"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(hjust = 0, size = 18, face = "bold"),
    axis.title.x = element_text(size = 16, face = "bold"),
    axis.title.y = element_text(size = 16, face = "bold"),
    axis.text = element_text(color = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

# Save the interaction strength figure
ggsave("FigS12_Interaction_Strength_Spatial_Analysis.tiff", 
       plot = interaction_plot, dpi = 300, width = 6, height = 6, units = "in")
