# Load required libraries
library(ggplot2)
library(mgcv)       # For GAM (Generalized Additive Models)
library(xgboost)    # For Gradient Boosted Trees
library(caret)      # For data partitioning
library(dplyr)
library(gridExtra)  # For arranging multiple plots

### ============================================================
### Part 1: Modeling for 3 Cities (1940–2024)
### ============================================================

# Load dataset for 3 cities
file_path2 <- "data/3cities_doy_weather_1940_2024.csv"
data <- read.csv(file_path2)

# Define predictors and target variable
features <- c("TMAX", "WIND", "EVPR", "GDD5_cum", 
              "CODE0", "CODE2", "CODE51", "CODE63", 
              "lat", "alt")
target <- "bloom_doy"

# Initialize list to store model results
results <- list(OLS = list(), GAM = list(), GBT = list())
num_iterations <- 10  # Repeated 10-fold modeling

# Run models
for (i in 1:num_iterations) {
  set.seed(i)
  trainIndex <- createDataPartition(data[[target]], p = 0.8, list = FALSE)
  trainData <- data[trainIndex, ]
  testData <- data[-trainIndex, ]
  
  X_train <- trainData[, features]
  y_train <- trainData[[target]]
  X_test <- testData[, features]
  y_test <- testData[[target]]
  
  # 1. Ordinary Least Squares (OLS)
  ols_model <- lm(bloom_doy ~ ., data = trainData[, c(features, target)])
  ols_pred <- predict(ols_model, testData)
  results$OLS[[i]] <- data.frame(Actual = y_test, Predicted = ols_pred)
  
  # 2. Generalized Additive Model (GAM)
  gam_model <- gam(bloom_doy ~ s(TMAX) + s(WIND) + s(EVPR) + s(GDD5_cum) + 
                     CODE0 + CODE2 + CODE51 + CODE63 + lat + alt, data = trainData)
  gam_pred <- predict(gam_model, testData)
  results$GAM[[i]] <- data.frame(Actual = y_test, Predicted = gam_pred)
  
  # 3. Gradient Boosted Trees (GBT)
  train_matrix <- xgb.DMatrix(data = as.matrix(X_train), label = y_train)
  test_matrix <- xgb.DMatrix(data = as.matrix(X_test))
  params <- list(
    objective = "reg:squarederror",
    eval_metric = "rmse",
    max_depth = 6,
    eta = 0.1,
    subsample = 0.8,
    colsample_bytree = 0.8
  )
  gbt_model <- xgb.train(params, train_matrix, nrounds = 500, verbose = 0)
  gbt_pred <- predict(gbt_model, test_matrix)
  results$GBT[[i]] <- data.frame(Actual = y_test, Predicted = gbt_pred)
}

# Calculate performance metrics
calculate_metrics <- function(result_list) {
  all_actuals <- unlist(lapply(result_list, function(df) df$Actual))
  all_preds <- unlist(lapply(result_list, function(df) df$Predicted))
  r2 <- round(cor(all_actuals, all_preds)^2, 2)
  mse <- round(mean((all_actuals - all_preds)^2), 2)
  return(list(R2 = r2, MSE = mse, Actual = all_actuals, Predicted = all_preds))
}

ols_metrics <- calculate_metrics(results$OLS)
gam_metrics <- calculate_metrics(results$GAM)
gbt_metrics <- calculate_metrics(results$GBT)

# Function to plot actual vs predicted with annotations
plot_pred_vs_actual <- function(actual, predicted, model_name, r2, mse, fig_label) {
  ggplot(data.frame(Actual = actual, Predicted = predicted), aes(x = Actual, y = Predicted)) +
    geom_point(alpha = 0.6, color = "red", size = 4) +
    geom_abline(slope = 1, intercept = 0, color = "blue", linetype = "solid", size = 2) +
    labs(title = paste(fig_label, model_name, "Predictions vs. Observations"),
         x = "Observed Bloom DOY", y = "Predicted Bloom DOY") +
    annotate("text", x = 86, y = 122, 
             label = paste("R² =", r2), size = 6, fontface = "bold") +
    annotate("text", x = 87, y = 118, 
             label = paste("MSE =", mse), size = 6, fontface = "bold") +
    theme_classic(base_size = 16) +
    theme(
      axis.title = element_text(size = 18, face = "bold"),
      axis.text = element_text(size = 14, face = "bold"),
      plot.title = element_text(size = 20, face = "bold"),
      panel.border = element_rect(color = "black", fill = NA, size = 1.2)
    )
}

# Create and export plots for 3-city models
plot1 <- plot_pred_vs_actual(ols_metrics$Actual, ols_metrics$Predicted, "OLS", ols_metrics$R2, ols_metrics$MSE, "(a)")
plot2 <- plot_pred_vs_actual(gam_metrics$Actual, gam_metrics$Predicted, "GAM", gam_metrics$R2, gam_metrics$MSE, "(b)")
plot3 <- plot_pred_vs_actual(gbt_metrics$Actual, gbt_metrics$Predicted, "GBT", gbt_metrics$R2, gbt_metrics$MSE, "(c)")

tiff("Fig3a_c_Three_cities_DOY_Model_Comparison.tiff", width = 16, height = 5, units = "in", res = 300)
grid.arrange(plot1, plot2, plot3, ncol = 3)
dev.off()

### ============================================================
### Part 2: Modeling for 330 Other Cities
### ============================================================

# Load extended dataset
file_path <- "data/other_cities_weather_data_330city_average_with_bloom.csv"
data <- read.csv(file_path)

# Use the same features and target
results <- list(OLS = list(), GAM = list(), GBT = list())

# Run modeling for 330-city dataset
for (i in 1:num_iterations) {
  set.seed(i)
  trainIndex <- createDataPartition(data[[target]], p = 0.8, list = FALSE)
  trainData <- data[trainIndex, ]
  testData <- data[-trainIndex, ]
  
  X_train <- trainData[, features]
  y_train <- trainData[[target]]
  X_test <- testData[, features]
  y_test <- testData[[target]]
  
  # OLS
  ols_model <- lm(bloom_doy ~ ., data = trainData[, c(features, target)])
  ols_pred <- predict(ols_model, testData)
  results$OLS[[i]] <- data.frame(Actual = y_test, Predicted = ols_pred)
  
  # GAM
  gam_model <- gam(bloom_doy ~ s(TMAX) + s(WIND) + s(EVPR) + s(GDD5_cum) + 
                     CODE0 + CODE2 + CODE51 + CODE63 + lat + alt, data = trainData)
  gam_pred <- predict(gam_model, testData)
  results$GAM[[i]] <- data.frame(Actual = y_test, Predicted = gam_pred)
  
  # GBT
  train_matrix <- xgb.DMatrix(data = as.matrix(X_train), label = y_train)
  test_matrix <- xgb.DMatrix(data = as.matrix(X_test))
  gbt_model <- xgb.train(params, train_matrix, nrounds = 500, verbose = 0)
  gbt_pred <- predict(gbt_model, test_matrix)
  results$GBT[[i]] <- data.frame(Actual = y_test, Predicted = gbt_pred)
}

# Compute metrics
ols_metrics <- calculate_metrics(results$OLS)
gam_metrics <- calculate_metrics(results$GAM)
gbt_metrics <- calculate_metrics(results$GBT)

# Plotting function reused, new style/positions
plot_pred_vs_actual <- function(actual, predicted, model_name, r2, mse, fig_label) {
  ggplot(data.frame(Actual = actual, Predicted = predicted), aes(x = Actual, y = Predicted)) +
    geom_point(alpha = 0.6, color = "blue", size = 4) +
    geom_abline(slope = 1, intercept = 0, color = "red", linetype = "solid", size = 2) +
    labs(title = paste(fig_label, model_name, "Predictions vs. Observations"),
         x = "Observed Bloom DOY", y = "Predicted Bloom DOY") +
    annotate("text", x = 90, y = 145, 
             label = paste("R² =", r2), size = 6, fontface = "bold") +
    annotate("text", x = 92, y = 140, 
             label = paste("MSE =", mse), size = 6, fontface = "bold") +
    theme_classic(base_size = 16) +
    theme(
      axis.title = element_text(size = 18, face = "bold"),
      axis.text = element_text(size = 14, face = "bold"),
      plot.title = element_text(size = 20, face = "bold"),
      panel.border = element_rect(color = "black", fill = NA, size = 1.2)
    )
}

# Create and export plots for 330-city models
plot1 <- plot_pred_vs_actual(ols_metrics$Actual, ols_metrics$Predicted, "OLS", ols_metrics$R2, ols_metrics$MSE, "(a)")
plot2 <- plot_pred_vs_actual(gam_metrics$Actual, gam_metrics$Predicted, "GAM", gam_metrics$R2, gam_metrics$MSE, "(b)")
plot3 <- plot_pred_vs_actual(gbt_metrics$Actual, gbt_metrics$Predicted, "GBT", gbt_metrics$R2, gbt_metrics$MSE, "(c)")

tiff("Fig4a_c_Other_cities_DOY_Model_Comparison.tiff", width = 16, height = 5, units = "in", res = 300)
grid.arrange(plot1, plot2, plot3, ncol = 3)
dev.off()
