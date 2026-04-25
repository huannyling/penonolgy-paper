# =============================================================================
# Cherry Blossom Bloom DOY Forecasting: Regression Tree & Random Forest Models
# Dataset: 3cities_doy_weather_1940_2024.csv (Kyoto, Liestal, Washington DC)
# Models: M1 = RT (clim), M2 = RF (clim), M3 = TS-RT (clim + lag), M4 = Hybrid RF+Lag
# Validation: (A) LOYO cross-validation; (B) True forecast 2021-2024 (train<=2020)
# Reference: Spiliotis (2022), Foresight Issue 64 — Decision Trees for Time-Series
# =============================================================================

library(tidyverse)
library(rpart)
library(randomForest)
library(patchwork)

set.seed(42)

# =============================================================================
# 1. LOAD DATA
# =============================================================================

df_raw <- read_csv("3cities_doy_weather_1940_2024.csv", show_col_types = FALSE)

df_raw <- df_raw %>%
  mutate(City = factor(City, levels = c("Kyoto", "Liestal", "Washington"))) %>%
  drop_na(bloom_doy)

# =============================================================================
# 2. PREDICTOR DEFINITIONS
# =============================================================================

clim_vars <- c(
  "TMAX", "TMIN", "TAVG", "TDIFF", "PRCP", "WIND", "EVPR", "SUND", "WATR",
  "SUND5_cum", "GDD5_cum", "CDD7_cum",
  "CODE3", "CODE51", "CODE71", "CODE73", "CODE61", "CODE53", "CODE63",
  "CODE55", "CODE75", "CODE2", "CODE0", "CODE1", "CODE65"
)

ts_vars  <- c("lag1_doy", "rolling3_doy", "Year_trend")
full_vars <- c(clim_vars, ts_vars)

# =============================================================================
# 3. FEATURE ENGINEERING  (lag1, rolling3, Year_trend — per PDF recommendations)
#    All lags reference only prior-year values: no data leakage in LOYO splits.
# =============================================================================

df <- df_raw %>%
  arrange(City, Year) %>%
  group_by(City) %>%
  mutate(
    lag1_doy     = lag(bloom_doy, 1),
    rolling3_doy = (lag(bloom_doy, 1) + lag(bloom_doy, 2) + lag(bloom_doy, 3)) / 3,
    Year_trend   = as.numeric(Year)
  ) %>%
  ungroup()

# =============================================================================
# 4. MODEL-FITTING HELPERS
# =============================================================================

# ---- M1: Regression Tree (climate vars only) --------------------------------
fit_rt <- function(train_data, predictors) {
  fml  <- as.formula(paste("bloom_doy ~", paste(predictors, collapse = " + ")))
  fit0 <- rpart(
    fml, data = train_data, method = "anova",
    control = rpart.control(cp = 0.0005, xval = 10, minsplit = 15)
  )
  best_cp <- fit0$cptable[which.min(fit0$cptable[, "xerror"]), "CP"]
  prune(fit0, cp = best_cp)
}

# ---- M2: Random Forest (climate vars only) ----------------------------------
fit_rf <- function(train_data, predictors) {
  fml <- as.formula(paste("bloom_doy ~", paste(predictors, collapse = " + ")))
  randomForest(fml, data = train_data, ntree = 300, importance = FALSE)
}

# ---- M3: PDF-inspired TS-RT (climate + lag1 + rolling3 + Year_trend) --------
# Identical tree machinery as M1 but with time-series features added
fit_ts_rt <- function(train_data, predictors) {
  fit_rt(train_data, predictors)   # same pruned rpart, different feature set
}

# ---- M4: Hybrid RF + Lag (climate + lag1 + rolling3 + Year_trend) -----------
fit_hybrid_rf <- function(train_data, predictors) {
  fit_rf(train_data, predictors)   # same RF machinery, enriched feature set
}

# =============================================================================
# 5. METRICS HELPER
# =============================================================================

calc_metrics <- function(observed, predicted) {
  n      <- sum(!is.na(observed) & !is.na(predicted))
  resid  <- observed - predicted
  rmse   <- sqrt(mean(resid^2, na.rm = TRUE))
  mae    <- mean(abs(resid),    na.rm = TRUE)
  ss_res <- sum(resid^2,          na.rm = TRUE)
  ss_tot <- sum((observed - mean(observed, na.rm = TRUE))^2, na.rm = TRUE)
  r2     <- 1 - ss_res / ss_tot
  list(N = n, R2 = round(r2, 4), RMSE = round(rmse, 4), MAE = round(mae, 4))
}

# =============================================================================
# 6. LOYO CROSS-VALIDATION  (per city, all 4 models)
# =============================================================================

run_loyo <- function(df, model_id, predictors, fit_fn) {

  results <- list()

  for (city_name in levels(df$City)) {

    dat_city   <- df %>% filter(City == city_name) %>% drop_na(all_of(predictors))
    years_city <- sort(unique(dat_city$Year))

    for (yy in years_city) {

      train <- dat_city %>% filter(Year != yy)
      test  <- dat_city %>% filter(Year == yy)

      if (nrow(train) < 20 || nrow(test) == 0) next

      # Impute any remaining NA in training lag columns with city training mean
      for (v in intersect(ts_vars, predictors)) {
        col_mean <- mean(train[[v]], na.rm = TRUE)
        train[[v]][is.na(train[[v]])] <- col_mean
        test[[v]][is.na(test[[v]])]   <- col_mean
      }

      fit  <- fit_fn(train, predictors)
      pred <- predict(fit, newdata = test)

      results[[length(results) + 1]] <- tibble(
        model    = model_id,
        City     = city_name,
        Year     = yy,
        observed = test$bloom_doy,
        predicted = pred
      )
    }
  }

  bind_rows(results)
}

cat("Running LOYO cross-validation...\n")

loyo_m1 <- run_loyo(df, "M1_RT",        clim_vars, fit_rt)
loyo_m2 <- run_loyo(df, "M2_RF",        clim_vars, fit_rf)
loyo_m3 <- run_loyo(df, "M3_TS_RT",     full_vars, fit_ts_rt)
loyo_m4 <- run_loyo(df, "M4_Hybrid_RF", full_vars, fit_hybrid_rf)

loyo_all <- bind_rows(loyo_m1, loyo_m2, loyo_m3, loyo_m4) %>%
  mutate(
    model = factor(model,
                   levels = c("M1_RT", "M2_RF", "M3_TS_RT", "M4_Hybrid_RF"),
                   labels = c("M1: RT (clim)", "M2: RF (clim)",
                              "M3: TS-RT (clim+lag)", "M4: Hybrid RF+Lag"))
  )

# =============================================================================
# 7. TRUE FORECAST  (train <= 2020, predict 2021-2024)
# =============================================================================

run_forecast <- function(df, model_id, predictors, fit_fn) {

  results <- list()

  for (city_name in levels(df$City)) {

    dat_city <- df %>% filter(City == city_name) %>% drop_na(all_of(predictors))
    train    <- dat_city %>% filter(Year <= 2020)
    test     <- dat_city %>% filter(Year %in% 2021:2024)

    if (nrow(train) < 20 || nrow(test) == 0) next

    for (v in intersect(ts_vars, predictors)) {
      col_mean <- mean(train[[v]], na.rm = TRUE)
      train[[v]][is.na(train[[v]])] <- col_mean
      test[[v]][is.na(test[[v]])]   <- col_mean
    }

    fit  <- fit_fn(train, predictors)
    pred <- predict(fit, newdata = test)

    results[[length(results) + 1]] <- tibble(
      model     = model_id,
      City      = city_name,
      Year      = test$Year,
      observed  = test$bloom_doy,
      predicted = pred
    )
  }

  bind_rows(results)
}

cat("Running true forecast 2021-2024...\n")

fc_m1 <- run_forecast(df, "M1_RT",        clim_vars, fit_rt)
fc_m2 <- run_forecast(df, "M2_RF",        clim_vars, fit_rf)
fc_m3 <- run_forecast(df, "M3_TS_RT",     full_vars, fit_ts_rt)
fc_m4 <- run_forecast(df, "M4_Hybrid_RF", full_vars, fit_hybrid_rf)

fc_all <- bind_rows(fc_m1, fc_m2, fc_m3, fc_m4) %>%
  mutate(
    model = factor(model,
                   levels = c("M1_RT", "M2_RF", "M3_TS_RT", "M4_Hybrid_RF"),
                   labels = c("M1: RT (clim)", "M2: RF (clim)",
                              "M3: TS-RT (clim+lag)", "M4: Hybrid RF+Lag"))
  )

# =============================================================================
# 8. METRICS TABLE  → model_comparison_metrics.csv
# =============================================================================

compute_metrics_table <- function(df_pred, eval_type) {
  models <- levels(df_pred$model)
  rows   <- list()

  for (mod in models) {
    sub <- df_pred %>% filter(model == mod)

    # Pooled
    m_all <- calc_metrics(sub$observed, sub$predicted)
    rows[[length(rows) + 1]] <- tibble(
      Evaluation = eval_type,
      Model = mod,
      City  = "All (pooled)",
      N     = m_all$N,
      R2    = m_all$R2,
      RMSE  = m_all$RMSE,
      MAE   = m_all$MAE
    )

    # Per city
    for (ct in levels(df$City)) {
      sub_ct <- sub %>% filter(City == ct)
      if (nrow(sub_ct) == 0) next
      m_ct <- calc_metrics(sub_ct$observed, sub_ct$predicted)
      rows[[length(rows) + 1]] <- tibble(
        Evaluation = eval_type,
        Model = mod,
        City  = ct,
        N     = m_ct$N,
        R2    = m_ct$R2,
        RMSE  = m_ct$RMSE,
        MAE   = m_ct$MAE
      )
    }
  }

  bind_rows(rows)
}

metrics_loyo <- compute_metrics_table(loyo_all, "LOYO CV")
metrics_fc   <- compute_metrics_table(fc_all,   "Forecast 2021-2024")

metrics_all <- bind_rows(metrics_loyo, metrics_fc)
write_csv(metrics_all, "model_comparison_metrics.csv")
cat("Saved: model_comparison_metrics.csv\n")

# Print summary
cat("\n========== LOYO pooled metrics ==========\n")
metrics_loyo %>% filter(City == "All (pooled)") %>%
  select(Model, R2, RMSE, MAE) %>% print(n = Inf)

cat("\n========== Forecast 2021-2024 pooled metrics ==========\n")
metrics_fc %>% filter(City == "All (pooled)") %>%
  select(Model, R2, RMSE, MAE) %>% print(n = Inf)

# =============================================================================
# 9. PLOTTING HELPERS
# =============================================================================

city_colors <- c(
  "Kyoto"      = "#E64B35",
  "Liestal"    = "#00A087",
  "Washington" = "#4DBBD5"
)

# Single-model predicted vs observed scatter panel
make_single_panel <- function(df_plot, model_label, panel_tag = NULL) {

  met <- calc_metrics(df_plot$observed, df_plot$predicted)

  axis_lo <- min(c(df_plot$observed, df_plot$predicted), na.rm = TRUE) - 2
  axis_hi <- max(c(df_plot$observed, df_plot$predicted), na.rm = TRUE) + 2

  annot_x <- axis_lo + 0.03 * (axis_hi - axis_lo)
  annot_y <- axis_hi - 0.03 * (axis_hi - axis_lo)

  title_str <- if (!is.null(panel_tag)) paste0(panel_tag, " ", model_label) else model_label

  ggplot(df_plot, aes(x = observed, y = predicted, color = City)) +
    geom_point(size = 3, alpha = 0.75) +
    geom_abline(slope = 1, intercept = 0, color = "firebrick", linewidth = 1.2, linetype = "dashed") +
    scale_color_manual(values = city_colors) +
    coord_fixed(xlim = c(axis_lo, axis_hi), ylim = c(axis_lo, axis_hi)) +
    labs(title = title_str, x = "Observed DOY", y = "Predicted DOY") +
    annotate("text",
             x = annot_x, y = annot_y, hjust = 0, vjust = 1,
             label = paste0("R² = ", sprintf("%.2f", met$R2),
                            "\nRMSE = ", sprintf("%.1f", met$RMSE),
                            "\nMAE = ",  sprintf("%.1f", met$MAE)),
             size = 4.5, fontface = "bold", color = "black") +
    theme_classic(base_size = 13) +
    theme(
      axis.title       = element_text(size = 13, face = "bold"),
      axis.text        = element_text(size = 11),
      plot.title       = element_text(size = 13, face = "bold"),
      panel.border     = element_rect(color = "black", fill = NA, linewidth = 0.9),
      legend.title     = element_blank(),
      legend.text      = element_text(size = 11),
      legend.position  = "bottom"
    )
}

# Build 4-panel figure for one evaluation type
make_4panel <- function(df_pred, panel_tags = c("(a)", "(b)", "(c)", "(d)")) {
  models <- levels(df_pred$model)
  panels <- Map(function(mod, tag) {
    sub <- df_pred %>% filter(model == mod)
    make_single_panel(sub, mod, tag)
  }, models, panel_tags)

  wrap_plots(panels, ncol = 2) +
    plot_layout(guides = "collect") &
    theme(legend.position = "bottom")
}

# =============================================================================
# 10. FIGURE A: LOYO cross-validation — 4 models
# =============================================================================

cat("Generating figures...\n")

fig_loyo <- make_4panel(loyo_all) +
  plot_annotation(
    title    = "LOYO Cross-Validation: Predicted vs Observed Bloom DOY",
    subtitle = "Leave-One-Year-Out per city | 1940-2024",
    theme    = theme(
      plot.title    = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5, color = "grey40")
    )
  )

ggsave("LOYO_model_comparison.png", fig_loyo,
       width = 13, height = 12, dpi = 300)
cat("Saved: LOYO_model_comparison.png\n")

# =============================================================================
# 11. FIGURE B: True forecast 2021-2024 — 4 models
# =============================================================================

fig_fc <- make_4panel(fc_all) +
  plot_annotation(
    title    = "True Forecast 2021-2024: Predicted vs Observed Bloom DOY",
    subtitle = "Training data ≤ 2020 | Test years: 2021-2024",
    theme    = theme(
      plot.title    = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5, color = "grey40")
    )
  )

ggsave("forecast_2021_2024_model_comparison.png", fig_fc,
       width = 13, height = 12, dpi = 300)
cat("Saved: forecast_2021_2024_model_comparison.png\n")

# =============================================================================
# 12. FIGURE C: Combined 8-panel (LOYO top, Forecast bottom)
# =============================================================================

loyo_panels <- Map(function(mod, tag) {
  sub <- loyo_all %>% filter(model == mod)
  make_single_panel(sub, mod, tag)
}, levels(loyo_all$model), c("(a)", "(b)", "(c)", "(d)"))

fc_panels <- Map(function(mod, tag) {
  sub <- fc_all %>% filter(model == mod)
  make_single_panel(sub, mod, tag)
}, levels(fc_all$model), c("(e)", "(f)", "(g)", "(h)"))

all_panels <- c(loyo_panels, fc_panels)

fig_combined <- wrap_plots(all_panels, ncol = 4) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

fig_combined <- fig_combined +
  plot_annotation(
    title    = "Cherry Blossom DOY Forecasting: Model Comparison",
    subtitle = "Top row: LOYO cross-validation (1940-2024) | Bottom row: True forecast 2021-2024 (train ≤ 2020)",
    theme    = theme(
      plot.title    = element_text(size = 17, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5, color = "grey40")
    )
  )

ggsave("combined_model_comparison.png", fig_combined,
       width = 22, height = 12, dpi = 300)
cat("Saved: combined_model_comparison.png\n")

# =============================================================================
# 13. SAVE PREDICTION DATA TABLES
# =============================================================================

write_csv(loyo_all, "loyo_predictions_all_models.csv")
write_csv(fc_all,   "forecast_predictions_2021_2024_all_models.csv")

cat("\nDone. Output files:\n")
cat("  model_comparison_metrics.csv\n")
cat("  loyo_predictions_all_models.csv\n")
cat("  forecast_predictions_2021_2024_all_models.csv\n")
cat("  LOYO_model_comparison.png\n")
cat("  forecast_2021_2024_model_comparison.png\n")
cat("  combined_model_comparison.png\n")
