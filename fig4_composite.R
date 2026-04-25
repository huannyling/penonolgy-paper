# =============================================================================
# fig4_composite.R
# Generate Fig4_phenological_memory_prediction.png/.pdf
# 6-panel composite figure for the phenological memory / prediction section
# Panels:
#   (a) LOYO RMSE by model — grouped bar (pooled + city)
#   (b) Best climate-only model (RF) LOYO predicted vs observed
#   (c) Lag-window effect on RMSE — performance curves
#   (d) Lag-window effect on R²  — performance curves
#   (e) Best lag model (RF + DOY_mean_8yr) LOYO predicted vs observed
#   (f) True forecast 2021-2024 — RF (best) predicted vs observed
# =============================================================================

library(tidyverse)
library(randomForest)
library(patchwork)

set.seed(42)

# =============================================================================
# 0.  Shared aesthetics
# =============================================================================
city_colors  <- c("Kyoto" = "#E64B35", "Liestal" = "#00A087", "Washington" = "#4DBBD5")
model_colors <- c("Tree" = "#E64B35", "Linear" = "#4DBBD5", "RF" = "#00A087")
model_shapes <- c("Tree" = 16, "Linear" = 17, "RF" = 15)

base_theme <- theme_classic(base_size = 12) +
  theme(
    axis.title       = element_text(size = 12, face = "bold"),
    axis.text        = element_text(size = 10),
    plot.title       = element_text(size = 12, face = "bold"),
    panel.border     = element_rect(color = "black", fill = NA, linewidth = 0.85),
    legend.title     = element_blank(),
    legend.text      = element_text(size = 10),
    legend.position  = "bottom",
    legend.key.size  = unit(0.45, "cm")
  )

# =============================================================================
# 1.  Load pre-computed data
# =============================================================================
metrics_main <- read_csv("model_comparison_metrics.csv", show_col_types = FALSE)
loyo_preds   <- read_csv("loyo_predictions_all_models.csv",  show_col_types = FALSE)
fc_preds     <- read_csv("forecast_predictions_2021_2024_all_models.csv", show_col_types = FALSE)
lag_metrics  <- read_csv("lag_DOY_model_comparison.csv", show_col_types = FALSE)

# =============================================================================
# 2.  Generate RF + DOY_mean_8yr LOYO predictions (for Panel e)
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

df_lag <- df_raw %>%
  arrange(City, Year) %>%
  group_by(City) %>%
  mutate(DOY_mean_8yr = (lag(bloom_doy,1)+lag(bloom_doy,2)+lag(bloom_doy,3)+
                           lag(bloom_doy,4)+lag(bloom_doy,5)+lag(bloom_doy,6)+
                           lag(bloom_doy,7)+lag(bloom_doy,8)) / 8) %>%
  ungroup()

preds_rf8 <- list()
for (city_name in levels(df_lag$City)) {
  dat <- df_lag %>%
    filter(City == city_name) %>%
    select(City, Year, bloom_doy, all_of(clim_vars), DOY_mean_8yr) %>%
    drop_na()
  for (yy in sort(unique(dat$Year))) {
    train <- dat %>% filter(Year != yy)
    test  <- dat %>% filter(Year == yy)
    if (nrow(train) < 15) next
    fit  <- randomForest(bloom_doy ~ ., data = train %>% select(-City, -Year),
                         ntree = 100)
    pred <- predict(fit, newdata = test)
    preds_rf8[[length(preds_rf8)+1]] <- tibble(
      City = city_name, Year = yy,
      observed = test$bloom_doy, predicted = as.numeric(pred)
    )
  }
}
pred_rf8_df <- bind_rows(preds_rf8) %>%
  mutate(City = factor(City, levels = c("Kyoto","Liestal","Washington")))

cat("RF+8yr LOYO done:", nrow(pred_rf8_df), "predictions\n")

# =============================================================================
# 3.  PANEL (a): LOYO RMSE grouped bar chart — M1 RT, M2 RF, M4 Hybrid RF+Lag
# =============================================================================
# Use pooled metrics from model_comparison_metrics.csv
loyo_bar <- metrics_main %>%
  filter(Evaluation == "LOYO CV", City != "All (pooled)") %>%
  filter(Model %in% c("M1: RT (clim)", "M2: RF (clim)", "M4: Hybrid RF+Lag")) %>%
  mutate(
    Model_short = recode(Model,
      "M1: RT (clim)"    = "RT\n(climate)",
      "M2: RF (clim)"    = "RF\n(climate)",
      "M4: Hybrid RF+Lag"= "RF\n(+lag DOY)"),
    Model_short = factor(Model_short,
      levels = c("RT\n(climate)", "RF\n(climate)", "RF\n(+lag DOY)")),
    City = factor(City, levels = c("Kyoto", "Liestal", "Washington"))
  )

# Pooled for reference line
loyo_pooled <- metrics_main %>%
  filter(Evaluation == "LOYO CV", City == "All (pooled)") %>%
  filter(Model %in% c("M1: RT (clim)", "M2: RF (clim)", "M4: Hybrid RF+Lag")) %>%
  mutate(Model_short = recode(Model,
    "M1: RT (clim)"    = "RT\n(climate)",
    "M2: RF (clim)"    = "RF\n(climate)",
    "M4: Hybrid RF+Lag"= "RF\n(+lag DOY)"),
    Model_short = factor(Model_short,
      levels = c("RT\n(climate)", "RF\n(climate)", "RF\n(+lag DOY)")))

pa <- ggplot(loyo_bar, aes(x = Model_short, y = RMSE, fill = City)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.75),
           width = 0.65, color = "white", linewidth = 0.3) +
  geom_point(data = loyo_pooled,
             aes(x = Model_short, y = RMSE), inherit.aes = FALSE,
             shape = 18, size = 4, color = "black") +
  scale_fill_manual(values = city_colors) +
  labs(title = "(a) LOYO: RMSE by model",
       x = NULL, y = "RMSE (days)") +
  annotate("text", x = 3.3, y = max(loyo_bar$RMSE) * 0.97,
           label = "◆ = pooled", hjust = 1, size = 3, color = "black") +
  base_theme +
  theme(legend.position = "bottom")

# =============================================================================
# 4.  PANEL (b): Best climate-only model (M2: RF) LOYO predicted vs observed
# =============================================================================
pred_m2 <- loyo_preds %>%
  filter(model == "M2: RF (clim)") %>%
  mutate(City = factor(City, levels = c("Kyoto", "Liestal", "Washington")))

m2_r2   <- round(1 - sum((pred_m2$observed - pred_m2$predicted)^2) /
                   sum((pred_m2$observed - mean(pred_m2$observed))^2), 2)
m2_rmse <- round(sqrt(mean((pred_m2$observed - pred_m2$predicted)^2)), 1)

ax_lo <- min(c(pred_m2$observed, pred_m2$predicted)) - 2
ax_hi <- max(c(pred_m2$observed, pred_m2$predicted)) + 2

pb <- ggplot(pred_m2, aes(x = observed, y = predicted, color = City)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, color = "firebrick",
              linewidth = 1.1, linetype = "dashed") +
  scale_color_manual(values = city_colors) +
  coord_fixed(xlim = c(ax_lo, ax_hi), ylim = c(ax_lo, ax_hi)) +
  annotate("text", x = ax_lo + 1, y = ax_hi - 1, hjust = 0, vjust = 1,
           label = paste0("R² = ", m2_r2, "\nRMSE = ", m2_rmse, " d"),
           size = 3.8, fontface = "bold") +
  labs(title = "(b) LOYO: RF (climate only)",
       x = "Observed DOY", y = "Predicted DOY") +
  base_theme

# =============================================================================
# 5.  PANEL (c) & (d): Lag-window RMSE and R² curves
# =============================================================================
lag_curve <- lag_metrics %>%
  filter(Lag_window != "None") %>%
  mutate(Lag_num = as.numeric(as.character(Lag_window)),
         Model   = factor(Model, levels = c("Tree","Linear","RF")))

lag_base  <- lag_metrics %>%
  filter(Lag_window == "None") %>%
  mutate(Model = factor(Model, levels = c("Tree","Linear","RF")))

best_lag_pt <- lag_curve %>%
  group_by(Model) %>%
  slice_min(RMSE, n = 1, with_ties = FALSE) %>% ungroup()

pc <- ggplot(lag_curve, aes(x = Lag_num, y = RMSE, color = Model, shape = Model)) +
  geom_hline(data = lag_base,
             aes(yintercept = RMSE, color = Model),
             linetype = "dashed", linewidth = 0.7, alpha = 0.8) +
  geom_line(linewidth = 1.0) +
  geom_point(size = 2.8) +
  geom_point(data = best_lag_pt, aes(x = Lag_num, y = RMSE),
             shape = 8, size = 4, color = "black", show.legend = FALSE) +
  scale_color_manual(values = model_colors) +
  scale_shape_manual(values = model_shapes) +
  scale_x_continuous(breaks = 2:10) +
  labs(title = "(c) Lag window: RMSE",
       subtitle = "Dashed = climate-only baseline; ★ = best",
       x = "Lag window (yr)", y = "RMSE (days)") +
  base_theme +
  theme(plot.subtitle = element_text(size = 8.5, color = "grey40"))

best_r2_pt <- lag_curve %>%
  group_by(Model) %>% slice_max(R2, n = 1, with_ties = FALSE) %>% ungroup()

pd <- ggplot(lag_curve, aes(x = Lag_num, y = R2, color = Model, shape = Model)) +
  geom_hline(data = lag_base,
             aes(yintercept = R2, color = Model),
             linetype = "dashed", linewidth = 0.7, alpha = 0.8) +
  geom_line(linewidth = 1.0) +
  geom_point(size = 2.8) +
  geom_point(data = best_r2_pt, aes(x = Lag_num, y = R2),
             shape = 8, size = 4, color = "black", show.legend = FALSE) +
  scale_color_manual(values = model_colors) +
  scale_shape_manual(values = model_shapes) +
  scale_x_continuous(breaks = 2:10) +
  labs(title = "(d) Lag window: R²",
       subtitle = "Dashed = climate-only baseline; ★ = best",
       x = "Lag window (yr)", y = "R²") +
  base_theme +
  theme(plot.subtitle = element_text(size = 8.5, color = "grey40"))

# =============================================================================
# 6.  PANEL (e): Best lag model (RF + DOY_mean_8yr) LOYO predicted vs observed
# =============================================================================
r2_rf8   <- round(1 - sum((pred_rf8_df$observed - pred_rf8_df$predicted)^2) /
                    sum((pred_rf8_df$observed - mean(pred_rf8_df$observed))^2), 2)
rmse_rf8 <- round(sqrt(mean((pred_rf8_df$observed - pred_rf8_df$predicted)^2)), 1)

ax2_lo <- min(c(pred_rf8_df$observed, pred_rf8_df$predicted)) - 2
ax2_hi <- max(c(pred_rf8_df$observed, pred_rf8_df$predicted)) + 2

pe <- ggplot(pred_rf8_df, aes(x = observed, y = predicted, color = City)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, color = "firebrick",
              linewidth = 1.1, linetype = "dashed") +
  scale_color_manual(values = city_colors) +
  coord_fixed(xlim = c(ax2_lo, ax2_hi), ylim = c(ax2_lo, ax2_hi)) +
  annotate("text", x = ax2_lo + 1, y = ax2_hi - 1, hjust = 0, vjust = 1,
           label = paste0("R² = ", r2_rf8, "\nRMSE = ", rmse_rf8, " d"),
           size = 3.8, fontface = "bold") +
  labs(title = "(e) LOYO: RF + DOY_mean_8yr (best lag)",
       x = "Observed DOY", y = "Predicted DOY") +
  base_theme

# =============================================================================
# 7.  PANEL (f): True forecast 2021-2024 — RF (climate-only, best for forecast)
# =============================================================================
pred_fc_rf <- fc_preds %>%
  filter(model == "M2: RF (clim)") %>%
  mutate(City = factor(City, levels = c("Kyoto","Liestal","Washington")))

fc_r2   <- round(1 - sum((pred_fc_rf$observed - pred_fc_rf$predicted)^2) /
                   sum((pred_fc_rf$observed - mean(pred_fc_rf$observed))^2), 2)
fc_rmse <- round(sqrt(mean((pred_fc_rf$observed - pred_fc_rf$predicted)^2)), 1)

ax3_lo <- min(c(pred_fc_rf$observed, pred_fc_rf$predicted)) - 3
ax3_hi <- max(c(pred_fc_rf$observed, pred_fc_rf$predicted)) + 3

pf <- ggplot(pred_fc_rf, aes(x = observed, y = predicted, color = City,
                              shape = factor(Year))) +
  geom_point(size = 3.5, alpha = 0.9) +
  geom_abline(slope = 1, intercept = 0, color = "firebrick",
              linewidth = 1.1, linetype = "dashed") +
  scale_color_manual(values = city_colors) +
  scale_shape_manual(values = c("2021"=15,"2022"=16,"2023"=17,"2024"=18),
                     name = "Year") +
  coord_fixed(xlim = c(ax3_lo, ax3_hi), ylim = c(ax3_lo, ax3_hi)) +
  annotate("text", x = ax3_lo + 1, y = ax3_hi - 1, hjust = 0, vjust = 1,
           label = paste0("R² = ", fc_r2, "\nRMSE = ", fc_rmse, " d\ntrain ≤ 2020"),
           size = 3.8, fontface = "bold") +
  labs(title = "(f) True forecast 2021–2024 (RF, climate)",
       x = "Observed DOY", y = "Predicted DOY") +
  base_theme +
  guides(color = guide_legend(order = 1),
         shape = guide_legend(order = 2,
           override.aes = list(color = "grey30", size = 2.5)))

# =============================================================================
# 8.  ASSEMBLE COMPOSITE
# =============================================================================
fig4 <- (pa | pb | pc) / (pd | pe | pf) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom",
        legend.box = "horizontal")

fig4 <- fig4 +
  plot_annotation(
    title   = "Fig. 4  Phenological memory and model-based prediction of cherry blossom bloom DOY",
    caption = paste0(
      "Kyoto (red), Liestal (teal), Washington DC (blue). ",
      "Dashed red lines in scatter plots = 1:1. ",
      "Panels (a–b): climate-only models; (c–f): phenological-memory (lag DOY) models. ",
      "LOYO = leave-one-year-out cross-validation."
    ),
    theme = theme(
      plot.title   = element_text(size = 13, face = "bold", hjust = 0),
      plot.caption = element_text(size = 9, color = "grey40", hjust = 0)
    )
  )

# =============================================================================
# 9.  SAVE
# =============================================================================
ggsave("Fig4_phenological_memory_prediction.png", fig4,
       width = 16, height = 10, dpi = 300)
cat("Saved: Fig4_phenological_memory_prediction.png\n")

# PDF version
ggsave("Fig4_phenological_memory_prediction.pdf", fig4,
       width = 16, height = 10, device = cairo_pdf)
cat("Saved: Fig4_phenological_memory_prediction.pdf\n")

cat("Panel metrics summary:\n")
cat(sprintf("  (b) RF LOYO:    R²=%.2f, RMSE=%.1f d\n", m2_r2, m2_rmse))
cat(sprintf("  (e) RF+8yr LOYO:R²=%.2f, RMSE=%.1f d\n", r2_rf8, rmse_rf8))
cat(sprintf("  (f) Forecast:   R²=%.2f, RMSE=%.1f d\n", fc_r2, fc_rmse))
