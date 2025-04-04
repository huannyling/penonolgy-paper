# Load necessary libraries
library(dplyr)
library(readr)
library(MASS)

# Load dataset
file_path <- "data/3cities_doy_weather_1940_2024.csv"
df0 <- read_csv(file_path)

# Filter for Kyoto data
df <- df0 %>% filter(city == "Kyoto")

# Ensure bloom_doy is numeric
df$bloom_doy <- as.numeric(df$bloom_doy)

# Define predictor sets
main_vars <- c("TMAX", "TMIN", "TAVG", "TDIFF", "PRCP", "WIND", "EVPR", "SUND", 
               "WATR", "SUND5_cum", "GDD5_cum", "CDD7_cum")

quad_inter_vars <- c("TMAX", "TMIN", "TAVG", "TDIFF", "EVPR", "SUND", "SUND5_cum", "GDD5_cum", "CDD7_cum")

# Generate quadratic terms
quad_terms <- paste0("I(", quad_inter_vars, "^2)")
interaction_terms <- combn(quad_inter_vars, 2, FUN = function(x) paste(x, collapse = "*"))
quad_interaction_terms <- combn(quad_terms, 2, FUN = function(x) paste(x, collapse = "*"))

# Initialize an empty data frame to store results
mse_results <- data.frame(Year = integer(), 
                          Linear_MSE = numeric(), Quadratic_MSE = numeric(), 
                          Quadratic_Interaction_MSE = numeric(), Quadratic_QuadInteraction_MSE = numeric())

# Loop through years to perform leave-one-year-out validation
for (test_year in 1940:2024) {
  # Split data into training and test sets
  train_data <- df %>% filter(year != test_year)
  test_data <- df %>% filter(year == test_year)
  
  # Skip if there is no test data for that year
  if (nrow(test_data) == 0) next
  
  # Define full model formulas
  formula_linear <- as.formula(paste("bloom_doy ~", paste(main_vars, collapse = " + ")))
  formula_quad <- as.formula(paste("bloom_doy ~", paste(main_vars, collapse = " + "), "+", paste(quad_terms, collapse = " + ")))
  formula_quad_interaction <- as.formula(paste("bloom_doy ~", paste(main_vars, collapse = " + "), "+", paste(quad_terms, collapse = " + "), "+", paste(interaction_terms, collapse = " + ")))
  formula_quad_quadinteraction <- as.formula(paste("bloom_doy ~", paste(main_vars, collapse = " + "), "+", paste(quad_terms, collapse = " + "), "+", paste(interaction_terms, collapse = " + "), "+", paste(quad_interaction_terms, collapse = " + ")))
  
  # Train models using stepwise selection
  step_model_linear <- stepAIC(lm(formula_linear, data = train_data), direction = "both", trace = FALSE)
  step_model_quad <- stepAIC(lm(formula_quad, data = train_data), direction = "both", trace = FALSE)
  step_model_quad_interaction <- stepAIC(lm(formula_quad_interaction, data = train_data), direction = "both", trace = FALSE)
  step_model_quad_quadinteraction <- stepAIC(lm(formula_quad_quadinteraction, data = train_data), direction = "both", trace = FALSE)
  
  # Predict DOY for test year
  pred_linear <- predict(step_model_linear, newdata = test_data)
  pred_quad <- predict(step_model_quad, newdata = test_data)
  pred_quad_interaction <- predict(step_model_quad_interaction, newdata = test_data)
  pred_quad_quadinteraction <- predict(step_model_quad_quadinteraction, newdata = test_data)
  
  # Calculate MSE
  mse_linear <- mean((test_data$bloom_doy - pred_linear)^2, na.rm = TRUE)
  mse_quad <- mean((test_data$bloom_doy - pred_quad)^2, na.rm = TRUE)
  mse_quad_interaction <- mean((test_data$bloom_doy - pred_quad_interaction)^2, na.rm = TRUE)
  mse_quad_quadinteraction <- mean((test_data$bloom_doy - pred_quad_quadinteraction)^2, na.rm = TRUE)
  
  # Store results
  mse_results <- rbind(mse_results, data.frame(Year = test_year, 
                                               Linear_MSE = mse_linear,
                                               Quadratic_MSE = mse_quad, 
                                               Quadratic_Interaction_MSE = mse_quad_interaction, 
                                               Quadratic_QuadInteraction_MSE = mse_quad_quadinteraction))
}

# Compute mean and standard deviation of MSE for each model
mse_summary <- mse_results %>%
  summarise(
    Mean_Linear_MSE = mean(Linear_MSE, na.rm = TRUE),
    Mean_Quadratic_MSE = mean(Quadratic_MSE, na.rm = TRUE),
    Mean_Quadratic_Interaction_MSE = mean(Quadratic_Interaction_MSE, na.rm = TRUE),
    Mean_Quadratic_QuadInteraction_MSE = mean(Quadratic_QuadInteraction_MSE, na.rm = TRUE),
    SD_Linear_MSE = sd(Linear_MSE, na.rm = TRUE),
    SD_Quadratic_MSE = sd(Quadratic_MSE, na.rm = TRUE),
    SD_Quadratic_Interaction_MSE = sd(Quadratic_Interaction_MSE, na.rm = TRUE),
    SD_Quadratic_QuadInteraction_MSE = sd(Quadratic_QuadInteraction_MSE, na.rm = TRUE)
  )
mse_summary
# Save results
write.csv(mse_results, "kyoto_mse_results_yearly.csv", row.names = FALSE)
write.csv(mse_summary, "kyoto_mse_summary.csv", row.names = FALSE)

print("LOYO-CV completed. Results saved:")
print("- Yearly MSE results: mse_results_yearly.csv")
print("- Mean & SD MSE summary: mse_summary.csv")



# Load necessary libraries
library(dplyr)
library(readr)

# Load the dataset
file_path <- "data/kyoto_mse_results_yearly.csv"
df_mse <- read_csv(file_path)

# Function to compute mean and standard deviation
compute_stats <- function(data, col_name) {
  mse_values <- na.omit(data[[col_name]])  # Remove NA values
  mean_val <- mean(mse_values)
  sd_val <- sd(mse_values)
  return(c(mean_val, sd_val))
}

# Function to compute trimmed mean and standard deviation (removing best & worst N years)
trimmed_stats <- function(data, col_name, trim_n) {
  mse_values <- na.omit(data[[col_name]])  # Remove NA values
  if (length(mse_values) > 2 * trim_n) {  # Ensure enough values remain
    trimmed_values <- sort(mse_values)[(trim_n + 1):(length(mse_values) - trim_n)]
    mean_trim <- mean(trimmed_values)
    sd_trim <- sd(trimmed_values)
  } else {
    mean_trim <- NA
    sd_trim <- NA
  }
  return(c(mean_trim, sd_trim))
}

# Define model names (excluding "Year")
models <- names(df_mse)[-1]  

# Compute statistics for each model
summary_table <- as.data.frame(t(sapply(models, function(col) compute_stats(df_mse, col))))
colnames(summary_table) <- c("mean_mse", "sd_mse")
rownames(summary_table) <- models

# Compute trimmed statistics (removing best & worst 3 and 5 years)
trim_3_results <- as.data.frame(t(sapply(models, function(col) trimmed_stats(df_mse, col, 3))))
trim_5_results <- as.data.frame(t(sapply(models, function(col) trimmed_stats(df_mse, col, 5))))

# Rename columns properly
colnames(trim_3_results) <- c("mean_mse_3", "sd_mse_3")
colnames(trim_5_results) <- c("mean_mse_5", "sd_mse_5")

# Ensure correct model names
rownames(trim_3_results) <- models
rownames(trim_5_results) <- models

# Merge all tables into a final summary
summary_table <- cbind(summary_table, trim_3_results, trim_5_results)

# Convert rownames to column
summary_table <- tibble(Model = rownames(summary_table), summary_table)

# Reshape summary table to 6 rows × 4 columns
summary_table_final <- summary_table %>%
  pivot_longer(cols = -Model, names_to = "Metric", values_to = "Value") %>%
  pivot_wider(names_from = Model, values_from = Value)

# Append summary table to yearly MSE results
df_mse_summary <- bind_rows(df_mse, summary_table_final)

# Save the updated results
write.csv(df_mse_summary, "kyoto_mse_results_with_summary.csv", row.names = FALSE)

print("Final MSE summary structured and saved as 'kyoto_mse_results_with_summary.csv'.")
