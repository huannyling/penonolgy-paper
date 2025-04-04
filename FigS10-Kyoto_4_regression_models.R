# Load necessary libraries
library(ggplot2)
library(car)   
library(MASS)  
library(dplyr)
library(readr)

# Load dataset
file_path <- "data/3cities_doy_weather_1940_2024.csv"
df0 <- read_csv(file_path)

# Filter for Kyoto data
df <- df0 %>% filter(city == "Kyoto")

# Ensure bloom_doy is numeric
df$bloom_doy <- as.numeric(df$bloom_doy)

# Define variables for different models
main_vars <- c("TMAX", "TMIN", "TAVG", "TDIFF", "PRCP", "WIND", "EVPR", "SUND", 
               "WATR", "SUND5_cum", "GDD5_cum", "CDD7_cum", "CODE3", "CODE51", 
               "CODE71", "CODE73", "CODE61", "CODE53", "CODE63", "CODE55", "CODE75", 
               "CODE2", "CODE0", "CODE1", "CODE65")

quad_inter_vars <- c("TMAX", "TMIN", "TAVG", "TDIFF", "EVPR", "SUND", "SUND5_cum", 
                     "GDD5_cum", "CDD7_cum")

# Create formulas
quad_terms <- paste0("I(", quad_inter_vars, "^2)")
interaction_terms <- combn(quad_inter_vars, 2, FUN = function(x) paste(x, collapse = "*"))
quad_interaction_terms <- combn(quad_terms, 2, FUN = function(x) paste(x, collapse = "*"))

# Step 1: Linear Model (Main Effects Only)
formula1 <- as.formula(paste("bloom_doy ~", paste(main_vars, collapse = " + ")))
model1 <- lm(formula1, data = df)
step_model1 <- stepAIC(model1, direction = "both", trace = FALSE)

# Step 2: Quadratic Model (Adds Quadratic Terms)
formula2 <- as.formula(paste("bloom_doy ~", paste(main_vars, collapse = " + "), " + ", paste(quad_terms, collapse = " + ")))
model2 <- lm(formula2, data = df)
step_model2 <- stepAIC(model2, direction = "both", trace = FALSE)
summary(step_model2)
# Step 3: Quadratic + Pairwise Interaction Model
formula3 <- as.formula(paste("bloom_doy ~", paste(main_vars, collapse = " + "), " + ", 
                             paste(quad_terms, collapse = " + "), " + ", paste(interaction_terms, collapse = " + ")))
model3 <- lm(formula3, data = df)
step_model3 <- stepAIC(model3, direction = "both", trace = FALSE)

# Step 4: Quadratic + Pairwise + Quadratic Interaction Model
formula4 <- as.formula(paste("bloom_doy ~", paste(main_vars, collapse = " + "), " + ", 
                             paste(quad_terms, collapse = " + "), " + ", 
                             paste(interaction_terms, collapse = " + "), " + ", 
                             paste(quad_interaction_terms, collapse = " + ")))
model4 <- lm(formula4, data = df)
step_model4 <- stepAIC(model4, direction = "both", trace = FALSE)

# Extract Model Metrics
models <- list(step_model1, step_model2, step_model3, step_model4)
model_names <- c("Linear", "Quadratic", "Quadratic+Interaction", "Quadratic+QuadInteraction")

metrics <- data.frame(
  Model = model_names,
  R2 = sapply(models, function(m) summary(m)$r.squared),
  MSE = sapply(models, function(m) mean(m$residuals^2)),
  RMSE = sapply(models, function(m) sqrt(mean(m$residuals^2))),
  AIC = sapply(models, function(m) AIC(m))
)
metrics 
# Save Metrics
write.csv(metrics, "Kyoto_model_comparison_metrics.csv", row.names = FALSE)

# Save Model Coefficients
coefficients_list <- lapply(models, function(m) coef(m))
coefficients_df <- do.call(rbind, lapply(1:length(models), function(i) {
  data.frame(Model = model_names[i], Variable = names(coefficients_list[[i]]), Coefficient = coefficients_list[[i]])
}))
coefficients_df
write.csv(coefficients_df, "Kyoto_model_coefficients.csv", row.names = FALSE)

# Generate Predicted vs Actual Plots
plots <- list()
for (i in 1:length(models)) {
  df$predicted <- predict(models[[i]], newdata = df)
  r2_value <- round(summary(models[[i]])$r.squared, 3)
  mse_value <- round(mean(models[[i]]$residuals^2), 3)
  
  plot <- ggplot(df, aes(x = bloom_doy, y = predicted)) +
    geom_point(size = 3, alpha = 0.8, color = "#2C3E50") +
    geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed", size = 1) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold"),
      axis.title = element_text(size = 14, face = "bold"),
      axis.text = element_text(size = 12),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    ) +
    labs(title = paste("Predicted vs Actual DOY -", model_names[i]), x = "Actual DOY", y = "Predicted DOY") +
    annotate("text", x = max(df$bloom_doy) - 5, y = min(df$predicted) + 5,
             label = paste("R² =", r2_value, "\nMSE =", mse_value),
             size = 6, fontface = "bold", color = "black")
  
  plots[[i]] <- plot
}

# Save individual plots
for (i in 1:length(plots)) {
  ggsave(paste0("predicted_vs_actual_", model_names[i], ".png"), plots[[i]], width = 8, height = 6, dpi = 300)
}

# Arrange all plots into a 2x2 grid
library(gridExtra)
grid_plot <- marrangeGrob(plots, nrow = 2, ncol = 2)
grid_plot
ggsave("FigS10_Kyoto_model_comparison_2x2.tiff", grid_plot, width = 16, height = 12, dpi = 300)



