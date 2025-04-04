# -------------------- Load Required Packages -------------------- #
library(ggplot2)
library(randomForest)
library(iml)         # For model interpretation (PDP, interaction)
library(lavaan)      # For structural equation modeling (SEM)
library(lavaanPlot)  # For visualizing SEM
library(ggpubr)      # For combining plots
library(gridExtra)   # For arranging plots

# -------------------- Load and Prepare Data -------------------- #
data <- read.csv("data/3cities_doy_weather_1940_2024.csv")

# Select relevant variables
selected_vars <- c("bloom_doy", "GDD5_cum", "SUND", "PRCP", "CDD7_cum", "WIND", "EVPR", "TMAX",
                   "CODE0", "CODE2", "CODE53", "CODE61", "CODE63", "CODE65")

# Subset and remove missing values
data <- data[, selected_vars]
data <- na.omit(data)

# -------------------- Train Random Forest & Explain Interactions -------------------- #

# Train Random Forest model
rf_model <- randomForest(bloom_doy ~ ., data = data, ntree = 500)

# Create iml predictor
predictor <- Predictor$new(rf_model, data = data, y = data$bloom_doy)

# Compute feature interaction strength
interaction_effect <- Interaction$new(predictor)

# Plot interaction strength
interaction_plot <- ggplot(interaction_effect$results, aes(x = reorder(.feature, .interaction), y = .interaction)) +
  geom_segment(aes(xend = .feature, y = 0, yend = .interaction), color = "black", linewidth = 1) +  
  geom_point(size = 3, color = "black") +
  coord_flip() +
  theme_minimal(base_size = 16) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(color = "black"),
    plot.title = element_text(hjust = 0, size = 18, face = "bold"),
    axis.title = element_text(size = 16, face = "bold")
  ) +
  labs(
    x = "Predictor Variables", 
    y = "Overall Interaction Strength", 
    title = "(e) Interaction Strength of Predictors"
  )
interaction_plot 
# Save figure
ggsave("Fig3e_interaction_strength.tiff", plot = interaction_plot, dpi = 300, width = 6, height = 6, units = "in")

# -------------------- Partial Dependence Plot (PDP) -------------------- #

# Calculate PDP for GDD5_cum
pdp_gdd <- FeatureEffect$new(predictor, feature = "GDD5_cum", method = "pdp")

# Plot PDP
Fig3d <- ggplot(pdp_gdd$results, aes(x = GDD5_cum, y = .value)) +
  geom_line(color = "black", size = 1.2) +
  geom_rug(sides = "b", alpha = 0.3) +
  geom_smooth(method = "loess", color = "blue", fill = "lightblue", se = TRUE) +
  theme_minimal(base_size = 16) +
  labs(
    title = "(d) Partial Dependence Analysis",
    x = "Accumulated Growing Degree Days (GDD5_cum)",
    y = "Predicted Blooming Day (DOY)"
  ) +
  theme(
    plot.title = element_text(hjust = 0, size = 18, face = "bold"),
    axis.title = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 14),
    panel.grid = element_blank()
  )
Fig3d 
# Save PDP figure
ggsave("Fig3d_PDP.tiff", plot = Fig3d, dpi = 300, width = 7, height = 6, units = "in")

# Optional: PDP for other features
pdp_sund <- FeatureEffect$new(predictor, feature = "SUND", method = "pdp")
pdp_prcp <- FeatureEffect$new(predictor, feature = "PRCP", method = "pdp")

plot(pdp_gdd) + ggtitle("Partial Dependence of GDD5_cum on DOY")
plot(pdp_sund) + ggtitle("Partial Dependence of SUND on DOY")
plot(pdp_prcp) + ggtitle("Partial Dependence of PRCP on DOY")

# -------------------- Structural Equation Modeling (SEM) -------------------- #

# Define simplified SEM model
model_minimal <- '
  # Direct effects on bloom_doy
  bloom_doy ~ GDD5_cum + WIND + EVPR + TMAX + 
              CODE0 + CODE2 + CODE63

  # Indirect effects on GDD5_cum
  GDD5_cum ~ CODE63 + PRCP + CDD7_cum + WIND
'

# Fit SEM model
sem_fit_minimal <- sem(model_minimal, data = data)

# Summary of fit and standardized path coefficients
summary(sem_fit_minimal, standardized = TRUE, fit.measures = TRUE)

# Visualize SEM structure
lavaanPlot(
  model = sem_fit_minimal,
  node_options = list(shape = "box", fillcolor = "lightblue"),
  edge_options = list(color = "black", fontsize = 12),
  coefs = TRUE, stand = TRUE
)

# Save SEM diagram
ggsave("Fig3f_SEM_minimal.tiff", dpi = 300, width = 10, height = 8, units = "in")

# Export standardized path coefficients
path_coeffs <- parameterEstimates(sem_fit_minimal, standardized = TRUE)
print(path_coeffs)
write.csv(path_coeffs, "Table_S4_SEM_path_coeffs.csv", row.names = FALSE)

# Optional: save a second SEM plot
ggsave("SEM_final.tiff", dpi = 300, width = 10, height = 8, units = "in")
