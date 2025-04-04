# -------------------- Random Forest Model & SHAP Interpretation --------------------

# Load necessary libraries
library(ggplot2)
library(randomForest)
library(iml)         # For model interpretation and SHAP values
library(lavaan)
library(lavaanPlot)  # For Structural Equation Modeling (SEM) visualization
library(ggpubr)      # For enhanced ggplot aesthetics
library(gridExtra)

# -------------------- Load and Preprocess Data --------------------

# Load dataset
data <- read.csv("data/3cities_doy_weather_1940_2024.csv")

# Select relevant variables for the model
selected_vars <- c(
  "bloom_doy", "GDD5_cum", "SUND", "PRCP", "CDD7_cum", "WIND", "EVPR", "TMAX",
  "CODE0", "CODE2", "CODE53", "CODE61", "CODE63", "CODE65"
)

# Subset and clean the data
data <- data[, selected_vars]
data <- na.omit(data)  # Remove rows with missing values

# -------------------- Train Random Forest Model --------------------

# Train a random forest regression model to predict bloom_doy
rf_model <- randomForest(bloom_doy ~ ., data = data, ntree = 500)

# Create a model predictor object for SHAP interpretation
predictor <- Predictor$new(rf_model, data = data, y = data$bloom_doy)

# -------------------- SHAP Feature Importance Plot --------------------

# Compute SHAP values for a representative observation (first row)
shap_values <- Shapley$new(predictor, x.interest = data[1, ])

# Extract SHAP results and round values for readability
shap_df <- shap_values$results
shap_df$phi <- round(shap_df$phi, 1)

# Plot SHAP values (feature contributions)
shap_plot <- ggplot(shap_df, aes(x = reorder(feature, phi), y = phi)) +
  geom_bar(stat = "identity", fill = "black") +
  coord_flip() +
  theme_minimal(base_size = 16) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(color = "black"),
    axis.title = element_text(face = "bold")
  ) +
  labs(
    x = "Features",
    y = "SHAP Value (phi)",
    title = "Feature Importance Based on SHAP Values"
  )

# Display plot
print(shap_plot)

# Save high-resolution TIFF image
ggsave("FigS3_shap_importance.tiff", plot = shap_plot, dpi = 300, width = 6, height = 8, units = "in")

# -------------------- Calculate Marginal Contribution Rate --------------------

# Compute absolute SHAP contributions
shap_df$abs_phi <- abs(shap_df$phi)

# Compute total absolute SHAP contribution
total_contribution <- sum(shap_df$abs_phi)

# Normalize to obtain marginal contribution rates (%)
shap_df$marginal_contribution_rate <- (shap_df$abs_phi / total_contribution) * 100

# Order features by contribution
shap_df <- shap_df[order(-shap_df$marginal_contribution_rate), ]

# Select and format output table
shap_table <- shap_df[, c("feature", "phi", "marginal_contribution_rate")]
shap_table$marginal_contribution_rate <- round(shap_table$marginal_contribution_rate, 2)

# Print and export table
print(shap_table)
write.csv(shap_table, "Table_S2_Marginal_Contribution_Rate.csv", row.names = FALSE)
