library(tidyverse)
library(rpart)
library(patchwork)

# ---------------------------
# Load data
# ---------------------------
df <- read_csv("3cities_doy_weather_1940_2024.csv")

# ---------------------------
# Define predictors
# ---------------------------
clim_vars <- c(
  "TMAX","TMIN","TAVG","TDIFF","PRCP","WIND","EVPR","SUND","WATR",
  "SUND5_cum","GDD5_cum","CDD7_cum",
  "CODE3","CODE51","CODE71","CODE73","CODE61","CODE53","CODE63",
  "CODE55","CODE75","CODE2","CODE0","CODE1","CODE65"
)

# ---------------------------
# Prepare data
# ---------------------------
df <- df %>%
  mutate(City = factor(City, levels = c("Kyoto", "Liestal", "Washington"))) %>%
  select(City, Year, bloom_doy, all_of(clim_vars)) %>%
  drop_na(bloom_doy)

# ---------------------------
# Helper: fit regression tree
# ---------------------------
fit_tree <- function(train_data) {
  fit0 <- rpart(
    bloom_doy ~ .,
    data = train_data %>% select(bloom_doy, all_of(clim_vars)),
    method = "anova",
    control = rpart.control(cp = 0.0005, xval = 10, minsplit = 15)
  )
  
  best_cp <- fit0$cptable[which.min(fit0$cptable[, "xerror"]), "CP"]
  prune(fit0, cp = best_cp)
}

# ---------------------------
# Helper: calculate metrics
# ---------------------------
calc_metrics <- function(observed, predicted) {
  ss_res <- sum((observed - predicted)^2, na.rm = TRUE)
  ss_tot <- sum((observed - mean(observed, na.rm = TRUE))^2, na.rm = TRUE)
  r2 <- 1 - ss_res / ss_tot
  rmse <- sqrt(mean((observed - predicted)^2, na.rm = TRUE))
  list(R2 = r2, RMSE = rmse)
}

# ---------------------------
# Helper: make plot
# ---------------------------
make_pred_plot <- function(df_plot, title_text, panel_label = NULL) {
  met <- calc_metrics(df_plot$observed, df_plot$predicted)
  
  x_min <- min(df_plot$observed, na.rm = TRUE)
  x_max <- max(df_plot$observed, na.rm = TRUE)
  y_min <- min(df_plot$predicted, na.rm = TRUE)
  y_max <- max(df_plot$predicted, na.rm = TRUE)
  
  ggplot(df_plot, aes(x = observed, y = predicted, color = City)) +
    geom_point(size = 4, alpha = 0.75) +
    geom_abline(slope = 1, intercept = 0, color = "red", linewidth = 1.5) +
    scale_color_manual(values = c(
      "Kyoto" = "#F8766D",
      "Liestal" = "#00BA38",
      "Washington" = "#619CFF"
    )) +
    labs(
      title = if (!is.null(panel_label)) paste0(panel_label, " ", title_text) else title_text,
      x = "Observed Bloom DOY",
      y = "Predicted Bloom DOY"
    ) +
    annotate(
      "text",
      x = x_min + 0.02 * (x_max - x_min),
      y = y_max - 0.05 * (y_max - y_min),
      hjust = 0,
      vjust = 1,
      label = paste0(
        "R² = ", format(round(met$R2, 2), nsmall = 2),
        "\nRMSE = ", format(round(met$RMSE, 2), nsmall = 2)
      ),
      size = 6,
      fontface = "bold",
      color = "black"
    ) +
    theme_classic(base_size = 16) +
    theme(
      axis.title = element_text(size = 18, face = "bold"),
      axis.text = element_text(size = 14, face = "bold"),
      plot.title = element_text(size = 20, face = "bold"),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 1.2),
      legend.title = element_blank(),
      legend.text = element_text(size = 14)
    )
}

# ============================================================
# Panel A: LOYO cross-validation (within each city, pooled)
# ============================================================

loyo_list <- list()

for (city_name in levels(df$City)) {
  
  dat_city <- df %>% filter(City == city_name)
  years_city <- sort(unique(dat_city$Year))
  
  pred_city_list <- list()
  
  for (yy in years_city) {
    
    train_city <- dat_city %>% filter(Year != yy)
    test_city  <- dat_city %>% filter(Year == yy)
    
    # 跳过极端情况
    if (nrow(train_city) < 20 || nrow(test_city) == 0) next
    
    fit_city <- fit_tree(train_city)
    pred_city <- predict(fit_city, newdata = test_city)
    
    pred_city_list[[as.character(yy)]] <- tibble(
      City = city_name,
      Year = yy,
      observed = test_city$bloom_doy,
      predicted = pred_city,
      type = "LOYO"
    )
  }
  
  loyo_list[[city_name]] <- bind_rows(pred_city_list)
}

pred_loyo_all <- bind_rows(loyo_list)

# ============================================================
# Panel B: True forecast (train <= 2020, predict 2021–2024)
# ============================================================

forecast_list <- list()

for (city_name in levels(df$City)) {
  
  dat_city <- df %>% filter(City == city_name)
  
  train_city <- dat_city %>% filter(Year <= 2020)
  test_city  <- dat_city %>% filter(Year %in% 2021:2024)
  
  if (nrow(train_city) < 20 || nrow(test_city) == 0) next
  
  fit_city <- fit_tree(train_city)
  pred_city <- predict(fit_city, newdata = test_city)
  
  forecast_list[[city_name]] <- tibble(
    City = city_name,
    Year = test_city$Year,
    observed = test_city$bloom_doy,
    predicted = pred_city,
    type = "Forecast"
  )
}

pred_forecast_all <- bind_rows(forecast_list)

# ============================================================
# Make plots
# ============================================================

p_a <- make_pred_plot(
  pred_loyo_all,
  title_text = "LOYO cross-validation",
  panel_label = "(a)"
)

p_b <- make_pred_plot(
  pred_forecast_all,
  title_text = "Forecast for 2021–2024",
  panel_label = "(b)"
)

combined_plot <- p_a + p_b + plot_layout(ncol = 2)

combined_plot

# ---------------------------
# Save figure
# ---------------------------
ggsave(
  "LOYO_vs_true_forecast_regression_tree.png",
  combined_plot,
  width = 14,
  height = 6.5,
  dpi = 300
)

# ---------------------------
# Optional: save data tables
# ---------------------------
write_csv(pred_loyo_all, "pred_loyo_all_regression_tree.csv")
write_csv(pred_forecast_all, "pred_forecast_2021_2024_regression_tree.csv")

# ---------------------------
# Print metrics
# ---------------------------
cat("LOYO metrics:\n")
print(calc_metrics(pred_loyo_all$observed, pred_loyo_all$predicted))

cat("\nForecast (2021-2024) metrics:\n")
print(calc_metrics(pred_forecast_all$observed, pred_forecast_all$predicted))