# ============================================================
# Observed vs Null Figure (a–c) — Standalone Script
# ============================================================

library(ggplot2)
library(dplyr)
library(readr)
library(lubridate)
library(cowplot)

set.seed(123)

# ---- Theme ----
theme_pub <- theme_minimal(base_size = 14) +
  theme(
    plot.title      = element_text(face = "bold", size = 16, hjust = 0),
    axis.title      = element_text(face = "bold"),
    axis.text       = element_text(face = "bold"),
    panel.grid.minor= element_blank(),
    legend.title    = element_blank(),
    legend.position = "top",
    plot.margin     = margin(5.5, 5.5, 5.5, 5.5)
  )

# ---- Helper: Observed vs Null overlay ----
plot_obs_vs_null <- function(obs, nul, title) {
  df_obs  <- data.frame(GDD5 = obs, type = "Observed")
  df_null <- data.frame(GDD5 = nul, type = "Null model")
  ggplot(bind_rows(df_obs, df_null), aes(GDD5, fill = type, colour = type)) +
    geom_histogram(data = df_obs, aes(y = ..density..), bins = 30, alpha = 0.45, position = "identity") +
    geom_density(data = df_null, size = 1.2, alpha = 0.5) +
    geom_density(data = df_obs,  size = 1.2, alpha = 0.8) +
    labs(title = title, x = "Accumulated GDD5", y = "Density") +
    theme_pub
}

# ---- Helper: Compute observed GDD5 from Feb 1 to bloom DOY ----
compute_gdd5_F1_to_bloom <- function(bloom_df, weather_df) {
  yrs <- intersect(bloom_df$year, unique(year(weather_df$date)))
  bdf <- bloom_df %>% filter(year %in% yrs) %>% select(year, bloom_doy)
  out <- numeric(nrow(bdf))
  w_by_yr <- split(weather_df, year(weather_df$date))
  for (i in seq_len(nrow(bdf))) {
    yr  <- bdf$year[i]
    doy <- bdf$bloom_doy[i]
    w   <- w_by_yr[[as.character(yr)]]
    start_date <- as.Date(paste0(yr, "-02-01"))
    bloom_date <- as.Date(paste0(yr, "-01-01")) + days(doy - 1)
    out[i] <- w %>%
      filter(date >= start_date, date <= bloom_date) %>%
      summarise(GDD5 = sum(GDD5, na.rm = TRUE)) %>% pull(GDD5)
  }
  out[is.na(out)] <- NA_real_
  out
}

# ---- Helper: Null-3 generator ----
get_bloom_window <- function(bloom_df) {
  q <- quantile(bloom_df$bloom_doy, c(0.05, 0.95), na.rm = TRUE, type = 7)
  c(floor(q[1]), ceiling(q[2]))
}

compute_null3_gdd <- function(bloom_df, weather_df, n_iter = 1000) {
  years_ok <- intersect(bloom_df$year, unique(year(weather_df$date)))
  bdf <- bloom_df %>% filter(year %in% years_ok)
  win <- get_bloom_window(bdf)
  weather_by_year <- split(weather_df, year(weather_df$date))
  
  out <- vector("list", n_iter)
  for (it in seq_len(n_iter)) {
    acc <- numeric(nrow(bdf))
    for (i in seq_len(nrow(bdf))) {
      yr <- bdf$year[i]
      w  <- weather_by_year[[as.character(yr)]]
      if (is.null(w)) { acc[i] <- NA_real_; next }
      rand_doy   <- sample(win[1]:win[2], 1)
      rand_date  <- as.Date(paste0(yr, "-01-01")) + days(rand_doy - 1)
      start_date <- as.Date(paste0(yr, "-02-01"))
      acc[i] <- w %>% filter(date >= start_date, date <= rand_date) %>%
        summarise(GDD5 = sum(GDD5, na.rm = TRUE)) %>% pull(GDD5)
    }
    out[[it]] <- acc
  }
  v <- unlist(out, use.names = FALSE)
  v[is.finite(v)]
}

# ---- Load data ----
kyoto_bloom      <- read_csv("data/kyoto.csv")
liestal_bloom    <- read_csv("data/liestal.csv")
washington_bloom <- read_csv("data/washingtondc.csv")

kyoto_weather      <- read_csv("data/kyoto_weather_final.csv")
liestal_weather    <- read_csv("data/liestal_weather_final.csv")
washington_weather <- read_csv("data/washington_weather_final.csv")

# ---- Observed GDD5 ----
kyoto_obs_gdd      <- compute_gdd5_F1_to_bloom(kyoto_bloom,      kyoto_weather)
liestal_obs_gdd    <- compute_gdd5_F1_to_bloom(liestal_bloom,    liestal_weather)
washington_obs_gdd <- compute_gdd5_F1_to_bloom(washington_bloom, washington_weather)

# ---- Null distributions ----
kyoto_null      <- compute_null3_gdd(kyoto_bloom,      kyoto_weather,      n_iter = 1000)
liestal_null    <- compute_null3_gdd(liestal_bloom,    liestal_weather,    n_iter = 1000)
washington_null <- compute_null3_gdd(washington_bloom, washington_weather, n_iter = 1000)

# ---- Build plots ----
p_a <- plot_obs_vs_null(kyoto_obs_gdd,      kyoto_null,      "(a) Kyoto")
p_b <- plot_obs_vs_null(liestal_obs_gdd,    liestal_null,    "(b) Liestal")
p_c <- plot_obs_vs_null(washington_obs_gdd, washington_null, "(c) Washington DC")

fig_obs_null <- plot_grid(p_a, p_b, p_c, ncol = 3, align = "hv")
fig_obs_null

# ---- Save ----
if (!dir.exists("fig")) dir.create("fig", recursive = TRUE)
ggsave("Figure_obs_vs_null_abc.jpg",  fig_obs_null, width = 10.5, height = 4.0, dpi = 600)
ggsave("Figure_obs_vs_null_abc.tiff", fig_obs_null, width = 10.5, height = 4.0, dpi = 600, compression = "lzw")
