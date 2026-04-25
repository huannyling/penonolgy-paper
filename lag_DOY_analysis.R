# =============================================================================
# lag_DOY_analysis.R
# Question: Does past multi-year mean flowering DOY improve bloom prediction?
# Models   : Regression Tree | Linear (VIF<10 + backward p<0.05) | Random Forest
# Lags     : DOY_mean_2yr … DOY_mean_10yr  (one at a time)
# Baseline : climate-only (no lag DOY)
# Validation: Leave-One-Year-Out (LOYO) per city, pooled across all three cities
# Output   : lag_DOY_model_comparison.csv  |  2 PNG figures  |  .md interpretation
# Runtime  : ~15-25 min (RF ntree=100 × 30 model-lag combos × ~240 LOYO folds)
# =============================================================================

library(tidyverse)
library(rpart)
library(randomForest)
library(car)        # vif()
library(patchwork)

set.seed(42)

cat("=== lag_DOY_analysis.R started ===\n")

# =============================================================================
# 1.  LOAD DATA
# =============================================================================

df_raw <- read_csv("3cities_doy_weather_1940_2024.csv", show_col_types = FALSE) %>%
  mutate(City = factor(City, levels = c("Kyoto", "Liestal", "Washington"))) %>%
  drop_na(bloom_doy)

clim_vars <- c(
  "TMAX","TMIN","TAVG","TDIFF","PRCP","WIND","EVPR","SUND","WATR",
  "SUND5_cum","GDD5_cum","CDD7_cum",
  "CODE3","CODE51","CODE71","CODE73","CODE61","CODE53","CODE63",
  "CODE55","CODE75","CODE2","CODE0","CODE1","CODE65"
)

# =============================================================================
# 2.  FEATURE ENGINEERING: DOY_mean_kyr  (k = 2 … 10)
#     For year t:  mean of bloom_doy[t-1], bloom_doy[t-2], …, bloom_doy[t-k]
#     Computed within each city; rows with insufficient history remain NA.
# =============================================================================

df <- df_raw %>%
  arrange(City, Year) %>%
  group_by(City) %>%
  mutate(
    DOY_mean_2yr  = (lag(bloom_doy,1) + lag(bloom_doy,2)) / 2,
    DOY_mean_3yr  = (lag(bloom_doy,1) + lag(bloom_doy,2) + lag(bloom_doy,3)) / 3,
    DOY_mean_4yr  = (lag(bloom_doy,1) + lag(bloom_doy,2) + lag(bloom_doy,3) +
                       lag(bloom_doy,4)) / 4,
    DOY_mean_5yr  = (lag(bloom_doy,1) + lag(bloom_doy,2) + lag(bloom_doy,3) +
                       lag(bloom_doy,4) + lag(bloom_doy,5)) / 5,
    DOY_mean_6yr  = (lag(bloom_doy,1) + lag(bloom_doy,2) + lag(bloom_doy,3) +
                       lag(bloom_doy,4) + lag(bloom_doy,5) + lag(bloom_doy,6)) / 6,
    DOY_mean_7yr  = (lag(bloom_doy,1) + lag(bloom_doy,2) + lag(bloom_doy,3) +
                       lag(bloom_doy,4) + lag(bloom_doy,5) + lag(bloom_doy,6) +
                       lag(bloom_doy,7)) / 7,
    DOY_mean_8yr  = (lag(bloom_doy,1) + lag(bloom_doy,2) + lag(bloom_doy,3) +
                       lag(bloom_doy,4) + lag(bloom_doy,5) + lag(bloom_doy,6) +
                       lag(bloom_doy,7) + lag(bloom_doy,8)) / 8,
    DOY_mean_9yr  = (lag(bloom_doy,1) + lag(bloom_doy,2) + lag(bloom_doy,3) +
                       lag(bloom_doy,4) + lag(bloom_doy,5) + lag(bloom_doy,6) +
                       lag(bloom_doy,7) + lag(bloom_doy,8) + lag(bloom_doy,9)) / 9,
    DOY_mean_10yr = (lag(bloom_doy,1) + lag(bloom_doy,2) + lag(bloom_doy,3) +
                       lag(bloom_doy,4) + lag(bloom_doy,5) + lag(bloom_doy,6) +
                       lag(bloom_doy,7) + lag(bloom_doy,8) + lag(bloom_doy,9) +
                       lag(bloom_doy,10)) / 10
  ) %>%
  ungroup()

lag_names <- paste0("DOY_mean_", 2:10, "yr")   # "DOY_mean_2yr" … "DOY_mean_10yr"

cat(sprintf("Data loaded: %d rows, %d cities\n", nrow(df), nlevels(df$City)))
cat("Lag columns created:", paste(lag_names, collapse=", "), "\n\n")

# =============================================================================
# 3.  METRIC HELPER
# =============================================================================

calc_metrics <- function(observed, predicted) {
  ok     <- !is.na(observed) & !is.na(predicted)
  obs    <- observed[ok];  prd <- predicted[ok]
  resid  <- obs - prd
  rmse   <- sqrt(mean(resid^2))
  ss_res <- sum(resid^2)
  ss_tot <- sum((obs - mean(obs))^2)
  r2     <- ifelse(ss_tot == 0, NA_real_, 1 - ss_res / ss_tot)
  list(N = sum(ok), R2 = round(r2, 4), RMSE = round(rmse, 4))
}

# =============================================================================
# 4.  MODEL-FITTING FUNCTIONS
# =============================================================================

# ---- 4a. Regression Tree (rpart, pruned to best cross-validated CP) ----------
fit_tree <- function(train_data, predictors) {
  d   <- train_data %>% select(bloom_doy, all_of(predictors)) %>% drop_na()
  fml <- as.formula(paste("bloom_doy ~", paste(predictors, collapse = "+")))
  fit0 <- rpart(fml, data = d, method = "anova",
                control = rpart.control(cp = 0.0005, xval = 10, minsplit = 15))
  best_cp <- fit0$cptable[which.min(fit0$cptable[,"xerror"]), "CP"]
  prune(fit0, cp = best_cp)
}

# ---- 4b. Linear model: VIF < 10 reduction → backward p < 0.05 selection -----
fit_lm <- function(train_data, predictors) {

  d    <- train_data %>% select(bloom_doy, all_of(predictors)) %>% drop_na()
  preds <- predictors   # working copy; will shrink

  # Guard: need at least predictors+5 observations
  if (nrow(d) < length(preds) + 5) return(lm(bloom_doy ~ 1, data = d))

  # Step 0: drop predictors whose lm coefficient is NA (perfect collinearity)
  repeat {
    if (length(preds) == 0) return(lm(bloom_doy ~ 1, data = d))
    fml  <- as.formula(paste("bloom_doy ~", paste(preds, collapse = "+")))
    fit  <- lm(fml, data = d)
    drop <- names(coef(fit))[is.na(coef(fit)) & names(coef(fit)) != "(Intercept)"]
    if (length(drop) == 0) break
    preds <- setdiff(preds, drop)
  }

  # Step 1: iteratively remove the predictor with the highest VIF (if VIF > 10)
  repeat {
    if (length(preds) < 2) break   # vif() needs >= 2 terms
    fml  <- as.formula(paste("bloom_doy ~", paste(preds, collapse = "+")))
    fit  <- lm(fml, data = d)
    vifs <- tryCatch(car::vif(fit), error = function(e) NULL)
    if (is.null(vifs) || max(vifs, na.rm = TRUE) <= 10) break
    worst <- names(which.max(vifs))
    preds <- setdiff(preds, worst)
    if (length(preds) == 0) return(lm(bloom_doy ~ 1, data = d))
  }

  # Step 2: backward elimination — remove highest p-value predictor (p > 0.05)
  repeat {
    if (length(preds) == 0) break
    fml  <- as.formula(paste("bloom_doy ~", paste(preds, collapse = "+")))
    fit  <- lm(fml, data = d)
    ct   <- summary(fit)$coefficients
    if (nrow(ct) <= 1) break  # intercept-only
    pv   <- ct[-1, "Pr(>|t|)", drop = TRUE]
    if (max(pv, na.rm = TRUE) <= 0.05) break
    worst <- names(which.max(pv))
    preds <- setdiff(preds, worst)
  }

  if (length(preds) == 0) return(lm(bloom_doy ~ 1, data = d))
  fml <- as.formula(paste("bloom_doy ~", paste(preds, collapse = "+")))
  lm(fml, data = d)
}

# ---- 4c. Random Forest (ntree = 100, mtry ~ p/3) ----------------------------
fit_rf <- function(train_data, predictors) {
  d   <- train_data %>% select(bloom_doy, all_of(predictors)) %>% drop_na()
  fml <- as.formula(paste("bloom_doy ~", paste(predictors, collapse = "+")))
  randomForest(fml, data = d, ntree = 100, importance = FALSE)
}

# =============================================================================
# 5.  LOYO ENGINE
#     Returns a data frame with columns: City, Year, observed, predicted
# =============================================================================

run_loyo <- function(df_full, predictors, fit_fn, min_train = 15) {

  # Keep only rows where ALL required predictors are non-NA
  needed_cols <- c("bloom_doy", predictors)
  df_use <- df_full %>%
    select(City, Year, all_of(needed_cols)) %>%
    drop_na()

  results <- list()

  for (city_name in levels(df_full$City)) {

    dat  <- df_use %>% filter(City == city_name)
    yrs  <- sort(unique(dat$Year))

    for (yy in yrs) {

      train <- dat %>% filter(Year != yy)
      test  <- dat %>% filter(Year == yy)

      if (nrow(train) < min_train || nrow(test) == 0) next

      pred_val <- tryCatch({
        fit <- fit_fn(train, predictors)
        as.numeric(predict(fit, newdata = test))
      }, error = function(e) NA_real_)

      results[[length(results) + 1]] <- tibble(
        City      = city_name,
        Year      = yy,
        observed  = test$bloom_doy,
        predicted = pred_val
      )
    }
  }

  bind_rows(results)
}

# =============================================================================
# 6.  MAIN ANALYSIS LOOP
#     For each model × lag-window combination, run LOYO and record metrics.
# =============================================================================

model_specs <- list(
  list(name = "Tree",   fn = fit_tree),
  list(name = "Linear", fn = fit_lm),
  list(name = "RF",     fn = fit_rf)
)

# lag window = "None" (baseline) or k in 2:10
lag_settings <- c("None", as.character(2:10))

all_results   <- list()   # detailed predictions
metrics_rows  <- list()   # one row per model × lag

total_combos  <- length(model_specs) * length(lag_settings)
combo_count   <- 0

for (ms in model_specs) {
  for (lag_w in lag_settings) {

    combo_count <- combo_count + 1
    cat(sprintf("[%d/%d] Model: %-6s | Lag: %s\n",
                combo_count, total_combos, ms$name, lag_w))

    # Build predictor set for this combination
    if (lag_w == "None") {
      preds    <- clim_vars
      lag_col  <- NA_character_
    } else {
      lag_col  <- paste0("DOY_mean_", lag_w, "yr")
      preds    <- c(clim_vars, lag_col)
    }

    # Run LOYO
    loyo_df <- run_loyo(df, preds, ms$fn)

    if (nrow(loyo_df) == 0) {
      cat("  WARNING: no predictions produced, skipping.\n")
      next
    }

    # Store predictions with labels
    loyo_df$Model      <- ms$name
    loyo_df$Lag_window <- lag_w
    all_results[[length(all_results) + 1]] <- loyo_df

    # Compute pooled metrics
    met <- calc_metrics(loyo_df$observed, loyo_df$predicted)

    metrics_rows[[length(metrics_rows) + 1]] <- tibble(
      Model      = ms$name,
      Lag_window = lag_w,
      N          = met$N,
      R2         = met$R2,
      RMSE       = met$RMSE
    )
  }
}

pred_all    <- bind_rows(all_results)
metrics_tbl <- bind_rows(metrics_rows)

# =============================================================================
# 7.  SAVE METRICS CSV
# =============================================================================

metrics_out <- metrics_tbl %>%
  mutate(
    Lag_window = factor(Lag_window, levels = c("None", as.character(2:10))),
    Model      = factor(Model, levels = c("Tree", "Linear", "RF"))
  ) %>%
  arrange(Model, Lag_window)

write_csv(metrics_out, "lag_DOY_model_comparison.csv")
cat("\nSaved: lag_DOY_model_comparison.csv\n")

cat("\n===== METRICS SUMMARY =====\n")
print(metrics_out %>% select(Model, Lag_window, R2, RMSE), n = Inf)

# =============================================================================
# 8.  FIGURE 1: Performance curve — RMSE and R² vs lag window (2–10)
#     Baseline "None" shown as a horizontal reference line per model.
# =============================================================================

model_colors <- c("Tree"   = "#E64B35",
                  "Linear" = "#4DBBD5",
                  "RF"     = "#00A087")
model_shapes <- c("Tree" = 16, "Linear" = 17, "RF" = 15)

# Separate baseline and lag-window rows
baseline <- metrics_out %>% filter(Lag_window == "None") %>%
  mutate(Lag_num = NA_real_)

lag_curve <- metrics_out %>%
  filter(Lag_window != "None") %>%
  mutate(Lag_num = as.numeric(as.character(Lag_window)))

# Best lag per model (lowest RMSE)
best_lag <- lag_curve %>%
  group_by(Model) %>%
  slice_min(RMSE, n = 1, with_ties = FALSE) %>%
  ungroup()

# ---- RMSE panel ----
p_rmse <- ggplot(lag_curve, aes(x = Lag_num, y = RMSE, color = Model, shape = Model)) +
  # Baseline reference lines
  geom_hline(data = baseline,
             aes(yintercept = RMSE, color = Model),
             linetype = "dashed", linewidth = 0.8, alpha = 0.7) +
  # Performance curves
  geom_line(linewidth = 1.1) +
  geom_point(size = 3.5) +
  # Star for best lag per model
  geom_point(data = best_lag,
             aes(x = Lag_num, y = RMSE),
             shape = 8, size = 5, stroke = 1.5, color = "black", show.legend = FALSE) +
  scale_color_manual(values = model_colors) +
  scale_shape_manual(values = model_shapes) +
  scale_x_continuous(breaks = 2:10) +
  labs(title = "(a) RMSE vs Lag Window",
       subtitle = "Dashed lines = climate-only baseline (Lag = None)\n★ = best lag per model",
       x = "Lag Window (years)", y = "RMSE (days)") +
  theme_classic(base_size = 13) +
  theme(
    plot.title    = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10, color = "grey40"),
    axis.title    = element_text(face = "bold"),
    panel.border  = element_rect(color = "black", fill = NA, linewidth = 0.9),
    legend.position = "bottom",
    legend.title  = element_blank()
  )

# ---- R² panel ----
p_r2 <- ggplot(lag_curve, aes(x = Lag_num, y = R2, color = Model, shape = Model)) +
  geom_hline(data = baseline,
             aes(yintercept = R2, color = Model),
             linetype = "dashed", linewidth = 0.8, alpha = 0.7) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 3.5) +
  geom_point(data = best_lag %>%
               group_by(Model) %>% slice_max(R2, n=1, with_ties=FALSE),
             aes(x = Lag_num, y = R2),
             shape = 8, size = 5, stroke = 1.5, color = "black", show.legend = FALSE) +
  scale_color_manual(values = model_colors) +
  scale_shape_manual(values = model_shapes) +
  scale_x_continuous(breaks = 2:10) +
  labs(title = "(b) R² vs Lag Window",
       subtitle = "Dashed lines = climate-only baseline (Lag = None)",
       x = "Lag Window (years)", y = "R²") +
  theme_classic(base_size = 13) +
  theme(
    plot.title    = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10, color = "grey40"),
    axis.title    = element_text(face = "bold"),
    panel.border  = element_rect(color = "black", fill = NA, linewidth = 0.9),
    legend.position = "bottom",
    legend.title  = element_blank()
  )

fig_curve <- (p_rmse | p_r2) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

fig_curve <- fig_curve +
  plot_annotation(
    title = "Impact of Lagged DOY on Model Performance (LOYO Cross-Validation)",
    theme = theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5))
  )

ggsave("lag_DOY_performance_curve.png", fig_curve,
       width = 13, height = 6, dpi = 300)
cat("Saved: lag_DOY_performance_curve.png\n")

# =============================================================================
# 9.  FIGURE 2: Best-performing lag model — Predicted vs Observed
#     "Best" = single lowest RMSE across all model × lag-window combinations
# =============================================================================

# Identify best combination (excluding baseline)
best_combo <- lag_curve %>%
  slice_min(RMSE, n = 1, with_ties = FALSE)

best_model  <- as.character(best_combo$Model)
best_lag_w  <- as.character(best_combo$Lag_window)
best_rmse   <- best_combo$RMSE
best_r2     <- best_combo$R2

cat(sprintf("\nBest combination: Model=%s, Lag=%s yr | R²=%.3f, RMSE=%.2f\n",
            best_model, best_lag_w, best_r2, best_rmse))

# Pull predictions for that combination
pred_best <- pred_all %>%
  filter(Model == best_model, Lag_window == best_lag_w) %>%
  mutate(City = factor(City, levels = c("Kyoto", "Liestal", "Washington")))

city_colors <- c("Kyoto" = "#E64B35", "Liestal" = "#00A087", "Washington" = "#4DBBD5")

axis_lo <- min(c(pred_best$observed, pred_best$predicted), na.rm = TRUE) - 3
axis_hi <- max(c(pred_best$observed, pred_best$predicted), na.rm = TRUE) + 3

fig_best <- ggplot(pred_best, aes(x = observed, y = predicted, color = City)) +
  geom_point(size = 3.5, alpha = 0.8) +
  geom_abline(slope = 1, intercept = 0, color = "firebrick",
              linewidth = 1.3, linetype = "dashed") +
  scale_color_manual(values = city_colors) +
  coord_fixed(xlim = c(axis_lo, axis_hi), ylim = c(axis_lo, axis_hi)) +
  annotate("text",
           x = axis_lo + 0.03*(axis_hi - axis_lo),
           y = axis_hi - 0.03*(axis_hi - axis_lo),
           hjust = 0, vjust = 1,
           label = paste0("Model: ", best_model,
                          "\nLag: DOY_mean_", best_lag_w, "yr",
                          "\nR² = ", sprintf("%.3f", best_r2),
                          "\nRMSE = ", sprintf("%.2f", best_rmse), " days"),
           size = 5, fontface = "bold", color = "black") +
  labs(
    title    = paste0("Best-Performing Lag Model: ", best_model,
                      " + DOY_mean_", best_lag_w, "yr"),
    subtitle = "LOYO cross-validation | All three cities pooled",
    x        = "Observed Bloom DOY",
    y        = "Predicted Bloom DOY"
  ) +
  theme_classic(base_size = 14) +
  theme(
    axis.title      = element_text(size = 14, face = "bold"),
    axis.text       = element_text(size = 12),
    plot.title      = element_text(size = 15, face = "bold"),
    plot.subtitle   = element_text(size = 11, color = "grey40"),
    panel.border    = element_rect(color = "black", fill = NA, linewidth = 1),
    legend.title    = element_blank(),
    legend.text     = element_text(size = 12),
    legend.position = "right"
  )

ggsave("best_lag_model_predicted_vs_observed.png", fig_best,
       width = 8, height = 7, dpi = 300)
cat("Saved: best_lag_model_predicted_vs_observed.png\n")

# =============================================================================
# 10.  PER-CITY METRICS for the best combination (supplementary)
# =============================================================================

cat("\n===== Per-city metrics for best combination =====\n")
pred_best %>%
  group_by(City) %>%
  summarise(
    N    = n(),
    R2   = round(1 - sum((observed-predicted)^2) / sum((observed-mean(observed))^2), 3),
    RMSE = round(sqrt(mean((observed-predicted)^2)), 2),
    .groups = "drop"
  ) %>%
  print()

cat("\n=== lag_DOY_analysis.R complete ===\n")
cat("Output files:\n")
cat("  lag_DOY_model_comparison.csv\n")
cat("  lag_DOY_performance_curve.png\n")
cat("  best_lag_model_predicted_vs_observed.png\n")
