# =============================================================================
# fig4_v2_composite.R  —  Revised Fig. 4 (6 panels)
#
# Scientific narrative:
#   (a) GDD5 sensitivity DECLINED significantly in Liestal but not Kyoto / WashDC
#       → thermal stationarity is breaking down in the cooler European site
#       → motivates testing whether PAST bloom timing (phenological memory) helps
#   (b) LOYO RMSE: RF > tree; Liestal hardest to predict in absolute RMSE
#   (c) Adding lag DOY window 2–10 yr consistently improves all models (RMSE)
#   (d) Same improvement shown via R²
#   (e) Best LOYO model (RF + 8-yr mean DOY): predicted vs observed
#   (f) True forecast 2021–2024: much harder — R² near zero, RMSE ≥ 4.6 d
#
# Layout: 2 rows × 3 columns (panel a spans top-left 2 columns via design matrix)
# =============================================================================

library(tidyverse)
library(ggpmisc)        # stat_poly_eq for regression equation annotations
library(broom)          # tidy() for regression results
library(randomForest)
library(patchwork)

set.seed(42)

# ─────────────────────────────────────────────────────────────────────────────
# Shared aesthetics
# ─────────────────────────────────────────────────────────────────────────────
CITY_COL  <- c("Kyoto" = "#E64B35", "Liestal" = "#00A087", "Washington" = "#4DBBD5")
MODEL_COL <- c("Tree" = "#E64B35",  "Linear"  = "#4DBBD5", "RF"         = "#00A087")
MODEL_SHP <- c("Tree" = 16, "Linear" = 17, "RF" = 15)
PER_COL   <- c("1942–1971" = "#D62728", "1995–2024" = "#1F77B4")

bthm <- theme_classic(base_size = 11) +
  theme(
    axis.title      = element_text(size = 11, face = "bold"),
    axis.text       = element_text(size = 9),
    plot.title      = element_text(size = 11, face = "bold"),
    panel.border    = element_rect(color = "black", fill = NA, linewidth = 0.8),
    legend.title    = element_blank(),
    legend.text     = element_text(size = 9),
    legend.position = "bottom",
    legend.key.size = unit(0.4, "cm")
  )

# ─────────────────────────────────────────────────────────────────────────────
# 1. Load data
# ─────────────────────────────────────────────────────────────────────────────
df_raw <- read_csv("3cities_doy_weather_1940_2024.csv", show_col_types = FALSE) %>%
  mutate(City = factor(City, levels = c("Kyoto", "Liestal", "Washington"))) %>%
  drop_na(bloom_doy)

metrics_main <- read_csv("model_comparison_metrics.csv", show_col_types = FALSE)
loyo_preds   <- read_csv("loyo_predictions_all_models.csv", show_col_types = FALSE)
fc_preds     <- read_csv("forecast_predictions_2021_2024_all_models.csv",
                          show_col_types = FALSE)
lag_metrics  <- read_csv("lag_DOY_model_comparison.csv", show_col_types = FALSE)

# ─────────────────────────────────────────────────────────────────────────────
# 2. Generate RF + DOY_mean_8yr LOYO predictions (for panel e)
# ─────────────────────────────────────────────────────────────────────────────
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
    fit  <- randomForest(bloom_doy ~ .,
                         data = train %>% select(-City, -Year), ntree = 100)
    preds_rf8[[length(preds_rf8)+1]] <- tibble(
      City = city_name, Year = yy,
      observed = test$bloom_doy,
      predicted = as.numeric(predict(fit, newdata = test))
    )
  }
}
pred_rf8_df <- bind_rows(preds_rf8) %>%
  mutate(City = factor(City, levels = c("Kyoto","Liestal","Washington")))
cat("RF+8yr LOYO predictions:", nrow(pred_rf8_df), "\n")

# ─────────────────────────────────────────────────────────────────────────────
# PANEL (a): GDD5 sensitivity — 3-city facets, 2 periods
# Motivation: Liestal slope declined; Kyoto/Washington stable
# ─────────────────────────────────────────────────────────────────────────────
df_sens <- df_raw %>%
  filter(Year >= 1942) %>%
  mutate(Period = case_when(
    Year >= 1942 & Year <= 1971 ~ "1942–1971",
    Year >= 1995 & Year <= 2024 ~ "1995–2024",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(Period)) %>%
  mutate(Period = factor(Period, levels = c("1942–1971", "1995–2024")),
         City   = factor(City,   levels = c("Kyoto", "Liestal", "Washington")))

# City-specific slope table for annotation
slope_tbl <- df_sens %>%
  group_by(City, Period) %>%
  do(tidy(lm(bloom_doy ~ GDD5_cum, data = .))) %>%
  filter(term == "GDD5_cum") %>%
  select(City, Period, estimate, p.value) %>%
  mutate(slope_label = sprintf("b=%.3f (p%s)",
                               estimate,
                               ifelse(p.value < 0.001, "<0.001",
                                      sprintf("=%.3f", p.value))))

pa <- ggplot(df_sens, aes(x = GDD5_cum, y = bloom_doy, color = Period)) +
  geom_point(alpha = 0.6, size = 1.5) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1.2,
              aes(group = Period, color = Period)) +
  stat_poly_eq(
    aes(label = paste(after_stat(eq.label), after_stat(p.value.label), sep = "~~~"),
        group = Period, color = Period),
    formula = y ~ x, parse = TRUE,
    size = 2.8, label.x.npc = "left", label.y.npc = c(0.97, 0.85),
    show.legend = FALSE
  ) +
  facet_wrap(~ City, scales = "free_x", nrow = 1) +
  scale_color_manual(values = PER_COL) +
  scale_y_continuous(limits = c(70, 128)) +
  labs(
    title = "(a) Thermal sensitivity of bloom DOY to accumulated heat (GDD₅_cum)",
    x     = "Accumulated GDD₅ (Feb 1–Mar 31, °C·d)",
    y     = "Bloom DOY"
  ) +
  bthm +
  theme(
    strip.text      = element_text(size = 10, face = "bold"),
    legend.position = "top",
    axis.text.x     = element_text(size = 8)
  )

# ─────────────────────────────────────────────────────────────────────────────
# PANEL (b): LOYO RMSE grouped bar — M1 RT, M2 RF, M4 Hybrid RF+Lag
# ─────────────────────────────────────────────────────────────────────────────
loyo_bar <- metrics_main %>%
  filter(Evaluation == "LOYO CV", City != "All (pooled)") %>%
  filter(Model %in% c("M1: RT (clim)", "M2: RF (clim)", "M4: Hybrid RF+Lag")) %>%
  mutate(
    Model_s = recode(Model,
      "M1: RT (clim)"    = "RT",
      "M2: RF (clim)"    = "RF",
      "M4: Hybrid RF+Lag"= "RF+lag"),
    Model_s = factor(Model_s, levels = c("RT", "RF", "RF+lag")),
    City    = factor(City, levels = c("Kyoto", "Liestal", "Washington"))
  )

loyo_pooled <- metrics_main %>%
  filter(Evaluation == "LOYO CV", City == "All (pooled)") %>%
  filter(Model %in% c("M1: RT (clim)", "M2: RF (clim)", "M4: Hybrid RF+Lag")) %>%
  mutate(
    Model_s = recode(Model,
      "M1: RT (clim)"    = "RT",
      "M2: RF (clim)"    = "RF",
      "M4: Hybrid RF+Lag"= "RF+lag"),
    Model_s = factor(Model_s, levels = c("RT", "RF", "RF+lag"))
  )

pb <- ggplot(loyo_bar, aes(x = Model_s, y = RMSE, fill = City)) +
  geom_bar(stat = "identity", position = position_dodge(0.72),
           width = 0.65, color = "white", linewidth = 0.25) +
  geom_point(data = loyo_pooled, aes(x = Model_s, y = RMSE),
             inherit.aes = FALSE,
             shape = 18, size = 4.5, color = "black") +
  scale_fill_manual(values = CITY_COL) +
  scale_y_continuous(limits = c(0, 8.5), expand = c(0, 0)) +
  labs(title = "(b) LOYO RMSE by model",
       x = NULL, y = "RMSE (days)") +
  annotate("text", x = 3.35, y = 8.2,
           label = "filled diamond = pooled", hjust = 1, size = 2.8,
           color = "grey40", fontface = "italic") +
  bthm + theme(legend.position = "none")  # legend shared at bottom

# ─────────────────────────────────────────────────────────────────────────────
# PANEL (c): Lag-window effect on RMSE
# ─────────────────────────────────────────────────────────────────────────────
lag_curve <- lag_metrics %>%
  filter(Lag_window != "None") %>%
  mutate(Lag_num = as.numeric(as.character(Lag_window)),
         Model   = factor(Model, levels = c("Tree","Linear","RF")))

lag_base  <- lag_metrics %>%
  filter(Lag_window == "None") %>%
  mutate(Model = factor(Model, levels = c("Tree","Linear","RF")))

best_rmse_pt <- lag_curve %>%
  group_by(Model) %>% slice_min(RMSE, n = 1, with_ties = FALSE) %>% ungroup()

pc <- ggplot(lag_curve, aes(x = Lag_num, y = RMSE, color = Model, shape = Model)) +
  geom_hline(data = lag_base, aes(yintercept = RMSE, color = Model),
             linetype = "dashed", linewidth = 0.65, alpha = 0.75) +
  geom_line(linewidth = 0.95) +
  geom_point(size = 2.5) +
  geom_point(data = best_rmse_pt, aes(x = Lag_num, y = RMSE),
             shape = 8, size = 4, color = "black", show.legend = FALSE) +
  scale_color_manual(values = MODEL_COL) +
  scale_shape_manual(values = MODEL_SHP) +
  scale_x_continuous(breaks = 2:10) +
  labs(title = "(c) Phenological memory: RMSE",
       subtitle = "Dashed = climate-only baseline | ★ = best window",
       x = "Prior mean DOY window (yr)", y = "RMSE (days)") +
  bthm + theme(plot.subtitle = element_text(size = 8, color = "grey40"))

# ─────────────────────────────────────────────────────────────────────────────
# PANEL (d): Lag-window effect on R²
# ─────────────────────────────────────────────────────────────────────────────
best_r2_pt <- lag_curve %>%
  group_by(Model) %>% slice_max(R2, n = 1, with_ties = FALSE) %>% ungroup()

pd <- ggplot(lag_curve, aes(x = Lag_num, y = R2, color = Model, shape = Model)) +
  geom_hline(data = lag_base, aes(yintercept = R2, color = Model),
             linetype = "dashed", linewidth = 0.65, alpha = 0.75) +
  geom_line(linewidth = 0.95) +
  geom_point(size = 2.5) +
  geom_point(data = best_r2_pt, aes(x = Lag_num, y = R2),
             shape = 8, size = 4, color = "black", show.legend = FALSE) +
  scale_color_manual(values = MODEL_COL) +
  scale_shape_manual(values = MODEL_SHP) +
  scale_x_continuous(breaks = 2:10) +
  labs(title = "(d) Phenological memory: R²",
       subtitle = "Dashed = climate-only baseline | ★ = best window",
       x = "Prior mean DOY window (yr)", y = "R²") +
  bthm + theme(plot.subtitle = element_text(size = 8, color = "grey40"))

# ─────────────────────────────────────────────────────────────────────────────
# PANEL (e): Best lag model LOYO — RF + DOY_mean_8yr
# ─────────────────────────────────────────────────────────────────────────────
r2_e   <- round(1 - sum((pred_rf8_df$observed - pred_rf8_df$predicted)^2) /
                  sum((pred_rf8_df$observed - mean(pred_rf8_df$observed))^2), 2)
rmse_e <- round(sqrt(mean((pred_rf8_df$observed - pred_rf8_df$predicted)^2)), 1)

ax_e <- range(c(pred_rf8_df$observed, pred_rf8_df$predicted)) + c(-2, 2)

pe <- ggplot(pred_rf8_df, aes(x = observed, y = predicted, color = City)) +
  geom_point(size = 1.8, alpha = 0.75) +
  geom_abline(slope = 1, intercept = 0, color = "firebrick",
              linewidth = 1.0, linetype = "dashed") +
  scale_color_manual(values = CITY_COL) +
  coord_fixed(xlim = ax_e, ylim = ax_e) +
  annotate("text", x = ax_e[1]+1, y = ax_e[2]-1, hjust = 0, vjust = 1,
           label = paste0("R²=", r2_e, "\nRMSE=", rmse_e, " d"),
           size = 3.5, fontface = "bold") +
  labs(title = "(e) LOYO: RF + 8-yr mean DOY (best model)",
       x = "Observed DOY", y = "Predicted DOY") +
  bthm + theme(legend.position = "none")

# ─────────────────────────────────────────────────────────────────────────────
# PANEL (f): True forecast 2021–2024 — RF (climate-only, best for forecast)
# ─────────────────────────────────────────────────────────────────────────────
pred_fc <- fc_preds %>%
  filter(model == "M2: RF (clim)") %>%
  mutate(City = factor(City, levels = c("Kyoto","Liestal","Washington")))

r2_f   <- round(1 - sum((pred_fc$observed - pred_fc$predicted)^2) /
                  sum((pred_fc$observed - mean(pred_fc$observed))^2), 2)
rmse_f <- round(sqrt(mean((pred_fc$observed - pred_fc$predicted)^2)), 1)

ax_f <- range(c(pred_fc$observed, pred_fc$predicted)) + c(-4, 4)

pf <- ggplot(pred_fc, aes(x = observed, y = predicted,
                           color = City, shape = factor(Year))) +
  geom_point(size = 3, alpha = 0.9) +
  geom_abline(slope = 1, intercept = 0, color = "firebrick",
              linewidth = 1.0, linetype = "dashed") +
  scale_color_manual(values = CITY_COL) +
  scale_shape_manual(values = c("2021"=15,"2022"=16,"2023"=17,"2024"=18),
                     name = "Year") +
  coord_fixed(xlim = ax_f, ylim = ax_f) +
  annotate("text", x = ax_f[1]+1, y = ax_f[2]-1, hjust = 0, vjust = 1,
           label = paste0("R²=", r2_f, "\nRMSE=", rmse_f, " d\ntrain≤2020"),
           size = 3.5, fontface = "bold") +
  labs(title = "(f) True forecast 2021–2024 (RF, climate)",
       x = "Observed DOY", y = "Predicted DOY") +
  bthm +
  guides(color = guide_legend(order = 1),
         shape = guide_legend(order = 2,
           override.aes = list(color = "grey30", size = 2)))

# ─────────────────────────────────────────────────────────────────────────────
# ASSEMBLE: panel (a) full top row; panels (b)-(f) in two rows of 3
# Patchwork design: a spans top full width; b,c in 2nd row left 2; d,e,f in 3rd
# Use a 6-cell layout: a across all 3 cols, then 2 rows of 3
# ─────────────────────────────────────────────────────────────────────────────

# Row layout: a full top; bottom 5 panels in two groups
fig4 <- (pa) /
  (pb | pc | pd) /
  (pe | pf | plot_spacer()) +
  plot_layout(heights = c(1.05, 1, 1), guides = "collect") &
  theme(legend.position = "bottom", legend.box = "horizontal")

fig4 <- fig4 +
  plot_annotation(
    title   = paste0("Fig. 4  Thermal sensitivity change and phenological memory in ",
                     "cherry blossom bloom timing"),
    caption = paste0(
      "City colours: Kyoto (red), Liestal (teal), Washington DC (blue). ",
      "(a) Period colours: early (dark red) and recent (blue). ",
      "Dashed red lines in scatter panels = 1:1 line. LOYO = leave-one-year-out."
    ),
    theme = theme(
      plot.title   = element_text(size = 13, face = "bold", hjust = 0),
      plot.caption = element_text(size = 9,  color = "grey40", hjust = 0)
    )
  )

# ─────────────────────────────────────────────────────────────────────────────
# SAVE
# ─────────────────────────────────────────────────────────────────────────────
ggsave("Fig4_phenological_memory_prediction.png", fig4,
       width = 16, height = 13, dpi = 300)
cat("Saved: Fig4_phenological_memory_prediction.png\n")

pdf("Fig4_phenological_memory_prediction.pdf", width = 16, height = 13)
print(fig4)
dev.off()
cat("Saved: Fig4_phenological_memory_prediction.pdf\n")

cat(sprintf("Panel metrics — (e) RF+8yr LOYO: R2=%.2f, RMSE=%.1f d\n", r2_e, rmse_e))
cat(sprintf("Panel metrics — (f) Forecast:    R2=%.2f, RMSE=%.1f d\n", r2_f, rmse_f))
