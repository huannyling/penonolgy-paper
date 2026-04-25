library(tidyverse)
library(rpart)
library(randomForest)
library(car)
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

# ============================================================
# Helper functions
# ============================================================

# ---- Metrics ----
calc_metrics <- function(observed, predicted) {
  ss_res <- sum((observed - predicted)^2, na.rm = TRUE)
  ss_tot <- sum((observed - mean(observed, na.rm = TRUE))^2, na.rm = TRUE)
  r2 <- 1 - ss_res / ss_tot
  rmse <- sqrt(mean((observed - predicted)^2, na.rm = TRUE))
  list(R2 = r2, RMSE = rmse)
}

# ---- Plot ----
make_pred_plot <- function(df_plot, title_text, panel_label = NULL) {
  met <- calc_metrics(df_plot$observed, df_plot$predicted)
  
  x_min <- min(df_plot$observed, na.rm = TRUE)
  x_max <- max(df_plot$observed, na.rm = TRUE)
  y_min <- min(df_plot$predicted, na.rm = TRUE)
  y_max <- max(df_plot$predicted, na.rm = TRUE)
  
  ggplot(df_plot, aes(x = observed, y = predicted, color = City)) +
    geom_point(size = 4, alpha = 0.75) +
    geom_abline(slope = 1, intercept = 0, color = "red", linewidth = 1.4) +
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
      size = 5.5,
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
      legend.text = element_text(size = 13)
    )
}

# ---- Regression tree ----
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

# ---- VIF-based variable selection for linear model ----
library(tidyverse)
library(car)

# ---------------------------
# Remove problematic predictors before VIF
# ---------------------------
clean_predictors <- function(train_data, vars) {
  dat <- train_data %>% select(bloom_doy, all_of(vars))
  
  # 1. remove zero-variance / single-value columns
  keep_vars <- vars[sapply(dat %>% select(all_of(vars)), function(x) {
    x <- x[!is.na(x)]
    length(unique(x)) > 1
  })]
  
  # 2. remove columns with all NA after filtering
  keep_vars <- keep_vars[sapply(dat %>% select(all_of(keep_vars)), function(x) {
    !all(is.na(x))
  })]
  
  keep_vars
}

# ---------------------------
# Iterative VIF selection with alias handling
# ---------------------------
select_vif_vars <- function(train_data, vars, threshold = 10) {
  
  keep_vars <- clean_predictors(train_data, vars)
  
  repeat {
    if (length(keep_vars) <= 2) break
    
    form <- as.formula(paste("bloom_doy ~", paste(keep_vars, collapse = " + ")))
    dat_use <- train_data %>% select(bloom_doy, all_of(keep_vars))
    
    mod <- lm(form, data = dat_use)
    
    # 1) remove aliased coefficients first
    ali <- alias(mod)$Complete
    if (!is.null(ali)) {
      aliased_vars <- rownames(ali)
      keep_vars <- setdiff(keep_vars, aliased_vars)
      next
    }
    
    # 2) then calculate VIF
    vif_vals <- car::vif(mod)
    
    if (is.matrix(vif_vals)) {
      vif_vals <- vif_vals[, 1]
    }
    
    max_vif <- max(vif_vals, na.rm = TRUE)
    
    if (max_vif < threshold) break
    
    drop_var <- names(which.max(vif_vals))
    keep_vars <- setdiff(keep_vars, drop_var)
  }
  
  keep_vars
}

# ---------------------------
# Fit final linear model after VIF filtering
# ---------------------------
fit_lm_vif <- function(train_data, vars, threshold = 10) {
  final_vars <- select_vif_vars(train_data, vars, threshold)
  
  form <- as.formula(paste("bloom_doy ~", paste(final_vars, collapse = " + ")))
  dat_use <- train_data %>% select(bloom_doy, all_of(final_vars))
  
  mod <- lm(form, data = dat_use)
  
  list(model = mod, vars = final_vars)
}



fit_lm_vif_select <- function(train_data, vars, vif_threshold = 10, p_threshold = 0.05) {
  # Step 1: VIF screening
  final_vars <- select_vif_vars(train_data, vars, threshold = vif_threshold)
  
  repeat {
    # build full model with current variables
    form <- as.formula(paste("bloom_doy ~", paste(final_vars, collapse = " + ")))
    dat_use <- train_data %>% select(bloom_doy, all_of(final_vars))
    mod <- lm(form, data = dat_use)
    
    # get p-values for predictors (exclude intercept)
    pvals <- summary(mod)$coefficients[-1, 4]
    
    # if only one predictor remains, keep it
    if (length(pvals) == 1) break
    
    max_p <- max(pvals, na.rm = TRUE)
    
    # stop if all predictors significant
    if (max_p <= p_threshold) break
    
    # remove worst predictor
    drop_var <- names(which.max(pvals))
    final_vars <- setdiff(final_vars, drop_var)
    
    # safety stop
    if (length(final_vars) < 1) break
  }
  
  # refit final model
  form <- as.formula(paste("bloom_doy ~", paste(final_vars, collapse = " + ")))
  dat_use <- train_data %>% select(bloom_doy, all_of(final_vars))
  mod <- lm(form, data = dat_use)
  
  list(model = mod, vars = final_vars)
}

# ---- Random forest ----
fit_rf <- function(train_data, vars) {
  randomForest(
    x = train_data %>% select(all_of(vars)),
    y = train_data$bloom_doy,
    ntree = 500,
    mtry = floor(sqrt(length(vars))),
    importance = TRUE
  )
}

# ============================================================
# 1) REGRESSION TREE
# ============================================================

# ---- LOYO ----
loyo_tree_list <- list()

for (city_name in levels(df$City)) {
  dat_city <- df %>% filter(City == city_name)
  years_city <- sort(unique(dat_city$Year))
  pred_city_list <- list()
  
  for (yy in years_city) {
    train_city <- dat_city %>% filter(Year != yy)
    test_city  <- dat_city %>% filter(Year == yy)
    if (nrow(train_city) < 20 || nrow(test_city) == 0) next
    
    fit_city <- fit_tree(train_city)
    pred_city <- predict(fit_city, newdata = test_city)
    
    pred_city_list[[as.character(yy)]] <- tibble(
      City = city_name,
      Year = yy,
      observed = test_city$bloom_doy,
      predicted = pred_city
    )
  }
  
  loyo_tree_list[[city_name]] <- bind_rows(pred_city_list)
}

pred_loyo_tree <- bind_rows(loyo_tree_list)

# ---- Forecast 2021-2024 ----
forecast_tree_list <- list()

for (city_name in levels(df$City)) {
  dat_city <- df %>% filter(City == city_name)
  train_city <- dat_city %>% filter(Year <= 2020)
  test_city  <- dat_city %>% filter(Year %in% 2021:2024)
  if (nrow(train_city) < 20 || nrow(test_city) == 0) next
  
  fit_city <- fit_tree(train_city)
  pred_city <- predict(fit_city, newdata = test_city)
  
  forecast_tree_list[[city_name]] <- tibble(
    City = city_name,
    Year = test_city$Year,
    observed = test_city$bloom_doy,
    predicted = pred_city
  )
}

pred_forecast_tree <- bind_rows(forecast_tree_list)

# ============================================================
# 2) LINEAR MODEL (VIF < 10)
# ============================================================

# ---- LOYO ----
loyo_lm_list <- list()

for (city_name in levels(df$City)) {
  dat_city <- df %>% filter(City == city_name)
  years_city <- sort(unique(dat_city$Year))
  pred_city_list <- list()
  
  for (yy in years_city) {
    train_city <- dat_city %>% filter(Year != yy)
    test_city  <- dat_city %>% filter(Year == yy)
    if (nrow(train_city) < 20 || nrow(test_city) == 0) next
    
    lm_fit <- fit_lm_vif(train_city, clim_vars, threshold = 10)
    pred_city <- predict(lm_fit$model, newdata = test_city %>% select(all_of(lm_fit$vars)))
    
    pred_city_list[[as.character(yy)]] <- tibble(
      City = city_name,
      Year = yy,
      observed = test_city$bloom_doy,
      predicted = pred_city
    )
  }
  
  loyo_lm_list[[city_name]] <- bind_rows(pred_city_list)
}

pred_loyo_lm <- bind_rows(loyo_lm_list)

# ---- Forecast 2021-2024 ----
forecast_lm_list <- list()

for (city_name in levels(df$City)) {
  dat_city <- df %>% filter(City == city_name)
  train_city <- dat_city %>% filter(Year <= 2020)
  test_city  <- dat_city %>% filter(Year %in% 2021:2024)
  if (nrow(train_city) < 20 || nrow(test_city) == 0) next
  
  lm_fit <- fit_lm_vif(train_city, clim_vars, threshold = 10)
  pred_city <- predict(lm_fit$model, newdata = test_city %>% select(all_of(lm_fit$vars)))
  
  forecast_lm_list[[city_name]] <- tibble(
    City = city_name,
    Year = test_city$Year,
    observed = test_city$bloom_doy,
    predicted = pred_city
  )
}

pred_forecast_lm <- bind_rows(forecast_lm_list)

# ============================================================
# 3) RANDOM FOREST
# ============================================================

# ---- LOYO ----
loyo_rf_list <- list()

for (city_name in levels(df$City)) {
  dat_city <- df %>% filter(City == city_name)
  years_city <- sort(unique(dat_city$Year))
  pred_city_list <- list()
  
  for (yy in years_city) {
    train_city <- dat_city %>% filter(Year != yy)
    test_city  <- dat_city %>% filter(Year == yy)
    if (nrow(train_city) < 20 || nrow(test_city) == 0) next
    
    rf_fit <- fit_rf(train_city, clim_vars)
    pred_city <- predict(rf_fit, newdata = test_city %>% select(all_of(clim_vars)))
    
    pred_city_list[[as.character(yy)]] <- tibble(
      City = city_name,
      Year = yy,
      observed = test_city$bloom_doy,
      predicted = pred_city
    )
  }
  
  loyo_rf_list[[city_name]] <- bind_rows(pred_city_list)
}

pred_loyo_rf <- bind_rows(loyo_rf_list)

# ---- Forecast 2021-2024 ----
forecast_rf_list <- list()

for (city_name in levels(df$City)) {
  dat_city <- df %>% filter(City == city_name)
  train_city <- dat_city %>% filter(Year <= 2020)
  test_city  <- dat_city %>% filter(Year %in% 2021:2024)
  if (nrow(train_city) < 20 || nrow(test_city) == 0) next
  
  rf_fit <- fit_rf(train_city, clim_vars)
  pred_city <- predict(rf_fit, newdata = test_city %>% select(all_of(clim_vars)))
  
  forecast_rf_list[[city_name]] <- tibble(
    City = city_name,
    Year = test_city$Year,
    observed = test_city$bloom_doy,
    predicted = pred_city
  )
}

pred_forecast_rf <- bind_rows(forecast_rf_list)

# ============================================================
# 4) PLOTS
# ============================================================

p1 <- make_pred_plot(pred_loyo_tree,      "Regression tree: LOYO cross-validation", "(a)")
p2 <- make_pred_plot(pred_forecast_tree,  "Regression tree: forecast for 2021–2024", "(b)")
p3 <- make_pred_plot(pred_loyo_lm,        "Linear model (VIF < 10): LOYO cross-validation", "(c)")
p4 <- make_pred_plot(pred_forecast_lm,    "Linear model (VIF < 10): forecast for 2021–2024", "(d)")
p5 <- make_pred_plot(pred_loyo_rf,        "Random forest: LOYO cross-validation", "(e)")
p6 <- make_pred_plot(pred_forecast_rf,    "Random forest: forecast for 2021–2024", "(f)")

combined_plot <- (p1 + p2) / (p3 + p4) / (p5 + p6)

combined_plot

# ---------------------------
# Save figure
# ---------------------------
ggsave(
  "six_model_prediction_panels.png",
  combined_plot,
  width = 14,
  height = 18,
  dpi = 300
)

# ============================================================
# 5) METRICS TABLE
# ============================================================

metric_df <- tibble(
  Model = c("Regression tree", "Regression tree",
            "Linear (VIF < 10)", "Linear (VIF < 10)",
            "Random forest", "Random forest"),
  Scenario = c("LOYO", "Forecast 2021-2024",
               "LOYO", "Forecast 2021-2024",
               "LOYO", "Forecast 2021-2024"),
  R2 = c(
    calc_metrics(pred_loyo_tree$observed, pred_loyo_tree$predicted)$R2,
    calc_metrics(pred_forecast_tree$observed, pred_forecast_tree$predicted)$R2,
    calc_metrics(pred_loyo_lm$observed, pred_loyo_lm$predicted)$R2,
    calc_metrics(pred_forecast_lm$observed, pred_forecast_lm$predicted)$R2,
    calc_metrics(pred_loyo_rf$observed, pred_loyo_rf$predicted)$R2,
    calc_metrics(pred_forecast_rf$observed, pred_forecast_rf$predicted)$R2
  ),
  RMSE = c(
    calc_metrics(pred_loyo_tree$observed, pred_loyo_tree$predicted)$RMSE,
    calc_metrics(pred_forecast_tree$observed, pred_forecast_tree$predicted)$RMSE,
    calc_metrics(pred_loyo_lm$observed, pred_loyo_lm$predicted)$RMSE,
    calc_metrics(pred_forecast_lm$observed, pred_forecast_lm$predicted)$RMSE,
    calc_metrics(pred_loyo_rf$observed, pred_loyo_rf$predicted)$RMSE,
    calc_metrics(pred_forecast_rf$observed, pred_forecast_rf$predicted)$RMSE
  )
)

print(metric_df)
write_csv(metric_df, "six_model_prediction_metrics.csv")





# ============================================================
# 4) PLOTS (ONLY LOYO)
# ============================================================

p1 <- make_pred_plot(pred_loyo_tree, 
                     "Regression tree:\nLOYO cross-validation", "(a)")

p2 <- make_pred_plot(pred_loyo_lm, 
                     "Linear model (VIF < 10):\nLOYO cross-validation", "(b)")

p3 <- make_pred_plot(pred_loyo_rf, 
                     "Random forest:\nLOYO cross-validation", "(c)")

# 横排 3 张图（更适合 Science）
combined_plot <- p1 + p2 + p3

combined_plot

# ---------------------------
# Save figure
# ---------------------------
ggsave(
  "LOYO_model_comparison.png",
  combined_plot,
  width = 16,
  height = 5,
  dpi = 300
)



metric_df <- tibble(
  Model = c("Regression tree", "Linear (VIF < 10)", "Random forest"),
  R2 = c(
    calc_metrics(pred_loyo_tree$observed, pred_loyo_tree$predicted)$R2,
    calc_metrics(pred_loyo_lm$observed, pred_loyo_lm$predicted)$R2,
    calc_metrics(pred_loyo_rf$observed, pred_loyo_rf$predicted)$R2
  ),
  RMSE = c(
    calc_metrics(pred_loyo_tree$observed, pred_loyo_tree$predicted)$RMSE,
    calc_metrics(pred_loyo_lm$observed, pred_loyo_lm$predicted)$RMSE,
    calc_metrics(pred_loyo_rf$observed, pred_loyo_rf$predicted)$RMSE
  )
)

print(metric_df)

write_csv(metric_df, "LOYO_model_metrics.csv")





# ============================================================
# EXTRACTION 1) Regression tree predictors used in each LOYO fold
# ============================================================

tree_var_info_list <- list()

city_name <- 'Kyoto'

for (city_name in unique(df$City)) {
  dat_city <- df %>% filter(City == city_name)
  years_city <- sort(unique(dat_city$Year))
  
  fold_info <- list()
  
  for (yy in years_city) {
    train_city <- dat_city %>% filter(Year != yy)
    test_city  <- dat_city %>% filter(Year == yy)
    if (nrow(train_city) < 20 || nrow(test_city) == 0) next
    
    fit_city <- fit_tree(train_city)
    
    # 提取最终 tree 中真正用到的变量
    vars_used <- unique(fit_city$frame$var[fit_city$frame$var != "<leaf>"])
    
    fold_info[[as.character(yy)]] <- tibble(
      City = city_name,
      Year = yy,
      Model = "Regression tree",
      n_predictors = length(vars_used),
      predictors = paste(vars_used, collapse = ", ")
    )
  }
  
  tree_var_info_list[[city_name]] <- bind_rows(fold_info)
}

tree_var_info <- bind_rows(tree_var_info_list)


# ============================================================
# EXTRACTION 2) Linear model predictors used in each LOYO fold
# ============================================================

lm_var_info_list <- list()

for (city_name in unique(df$City)) {
  dat_city <- df %>% filter(City == city_name)
  years_city <- sort(unique(dat_city$Year))
  
  fold_info <- list()
  
  for (yy in years_city) {
    train_city <- dat_city %>% filter(Year != yy)
    test_city  <- dat_city %>% filter(Year == yy)
    if (nrow(train_city) < 20 || nrow(test_city) == 0) next
    
    lm_fit <- fit_lm_vif_select(train_city, clim_vars, vif_threshold = 10, p_threshold = 0.05)
    vars_used <- lm_fit$vars
    
    fold_info[[as.character(yy)]] <- tibble(
      City = city_name,
      Year = yy,
      Model = "Linear (VIF < 10)",
      n_predictors = length(vars_used),
      predictors = paste(vars_used, collapse = ", ")
    )
  }
  
  lm_var_info_list[[city_name]] <- bind_rows(fold_info)
}

lm_var_info <- bind_rows(lm_var_info_list)


# ============================================================
# SUMMARY 1) Number of predictors per fold
# ============================================================

bind_rows(tree_var_info, lm_var_info) %>%
  group_by(Model, City) %>%
  summarise(
    mean_n_predictors = mean(n_predictors),
    min_n_predictors = min(n_predictors),
    max_n_predictors = max(n_predictors),
    .groups = "drop"
  ) %>%
  arrange(Model, City)


bind_rows(tree_var_info, lm_var_info) %>%
  group_by(Model) %>%
  summarise(
    mean_n_predictors = mean(n_predictors),
    min_n_predictors = min(n_predictors),
    max_n_predictors = max(n_predictors),
    .groups = "drop"
  )


# ============================================================
# SUMMARY 2A) Most frequently used predictors in regression tree
# ============================================================

tree_var_freq <- tree_var_info %>%
  separate_rows(predictors, sep = ",\\s*") %>%
  group_by(City, predictors) %>%
  summarise(freq = n(), .groups = "drop") %>%
  arrange(City, desc(freq))

tree_var_freq


# ============================================================
# SUMMARY 2B) Most frequently used predictors in linear model
# ============================================================

lm_var_freq <- lm_var_info %>%
  separate_rows(predictors, sep = ",\\s*") %>%
  group_by(City, predictors) %>%
  summarise(freq = n(), .groups = "drop") %>%
  arrange(City, desc(freq))

lm_var_freq

# ============================================================
# SUMMARY 3) Compare predictors fold by fold
# ============================================================

compare_fold_vars <- tree_var_info %>%
  rename(tree_n = n_predictors, tree_predictors = predictors) %>%
  left_join(
    lm_var_info %>%
      rename(lm_n = n_predictors, lm_predictors = predictors),
    by = c("City", "Year")
  ) %>%
  rowwise() %>%
  mutate(
    tree_set = list(strsplit(tree_predictors, ",\\s*")[[1]]),
    lm_set   = list(strsplit(lm_predictors, ",\\s*")[[1]]),
    same_predictors = setequal(tree_set, lm_set),
    n_overlap = length(intersect(tree_set, lm_set)),
    overlap_predictors = paste(intersect(tree_set, lm_set), collapse = ", ")
  ) %>%
  ungroup() %>%
  select(City, Year, tree_n, lm_n, same_predictors, n_overlap, overlap_predictors,
         tree_predictors, lm_predictors)

compare_fold_vars



compare_fold_vars %>%
  group_by(City) %>%
  summarise(
    n_folds = n(),
    n_same = sum(same_predictors),
    prop_same = mean(same_predictors),
    mean_overlap = mean(n_overlap),
    .groups = "drop"
  )


write_csv(tree_var_info, "tree_predictors_per_fold.csv")
write_csv(lm_var_info, "lm_predictors_per_fold.csv")
write_csv(tree_var_freq, "tree_predictor_frequency.csv")
write_csv(lm_var_freq, "lm_predictor_frequency.csv")
write_csv(compare_fold_vars, "tree_vs_lm_predictor_comparison.csv")


# ============================================================
# EXTRACTION 3) Random forest variable importance
# ============================================================

rf_importance_list <- list()

for (city_name in unique(df$City)) {
  dat_city <- df %>% filter(City == city_name)
  years_city <- sort(unique(dat_city$Year))
  
  fold_info <- list()
  
  for (yy in years_city) {
    train_city <- dat_city %>% filter(Year != yy)
    test_city  <- dat_city %>% filter(Year == yy)
    if (nrow(train_city) < 20 || nrow(test_city) == 0) next
    
    rf_fit <- fit_rf(train_city, clim_vars)
    
    imp <- importance(rf_fit)
    imp_df <- tibble(
      City = city_name,
      Year = yy,
      predictor = rownames(imp),
      IncMSE = imp[, 1]
    )
    
    fold_info[[as.character(yy)]] <- imp_df
  }
  
  rf_importance_list[[city_name]] <- bind_rows(fold_info)
}

rf_importance_all <- bind_rows(rf_importance_list)

rf_importance_summary <- rf_importance_all %>%
  group_by(City, predictor) %>%
  summarise(mean_IncMSE = mean(IncMSE, na.rm = TRUE), .groups = "drop") %>%
  arrange(City, desc(mean_IncMSE))

rf_importance_summary

write_csv(rf_importance_summary, "rf_mean_importance_summary.csv")
