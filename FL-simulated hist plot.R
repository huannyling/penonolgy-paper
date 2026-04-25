library(ggplot2)
library(dplyr)
library(cowplot)
library(lubridate)

# ============================================================
# 1) Theme used by plot_hist_obs (this will be the global style)
# ============================================================
theme_hist <- theme_bw(base_size = 16) +
  theme(
    plot.title       = element_text(face = "bold", hjust = 0.5),
    axis.title       = element_text(face = "bold"),
    axis.text        = element_text(face = "bold"),
    panel.grid.major = element_line(color = "grey80", linewidth = 0.8),
    panel.grid.minor = element_line(color = "grey90", linewidth = 0.6),
    plot.margin      = margin(8, 8, 8, 8),
    panel.border     = element_blank(),
    axis.line        = element_blank()
  )

# ============================================================
# 2) Panel A–C (top row)
# ============================================================
set.seed(1)
n <- 1000

plot_sim <- function(x, title, fill_col, xlab = "GDD", bins = 22, xlim = c(100, 300)) {
  df <- data.frame(GDD = x)
  ggplot(df, aes(GDD)) +
    geom_histogram(aes(y = after_stat(density)),
                   bins = bins, fill = fill_col, color = "white", alpha = 0.85) +
    geom_density(linewidth = 1.3, color = "black", trim = TRUE) +
    scale_x_continuous(limits = xlim, breaks = seq(100, 300, 80)) +
    labs(title = title, x = xlab, y = "Density") +
    theme_hist
}


kyoto_null      <- compute_null3_gdd(kyoto_bloom,      kyoto_weather,      n_iter = 1000)
liestal_null    <- compute_null3_gdd(liestal_bloom,    liestal_weather,    n_iter = 1000)
washington_null <- compute_null3_gdd(washington_bloom, washington_weather, n_iter = 1000)

# Panel A
sim_norm <- rnorm(n, mean = 200, sd = 30)
pa <- plot_sim(sim_norm, "(a) Normal (200 peak)", fill_col = "grey80")

# Panel B
sim_unif <- pmin(pmax(runif(n, 100, 300) + rnorm(n, 0, 8), 80), 320)
pb <- plot_sim(sim_unif, "(b) Uniform (100–300)", fill_col = "grey80")

# Panel C (separate)
center <- 220
sim_skew_left <- center - rgamma(n, shape = 3, scale = 18)
sim_skew_left <- pmin(pmax(sim_skew_left, 100), 300)

df_c <- data.frame(GDD = sim_skew_left)
pc <- ggplot(df_c, aes(GDD)) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 22, fill = "grey80", color = "white", alpha = 0.85) +
  geom_density(linewidth = 1.3, color = "black", trim = TRUE, adjust = 1.2) +
  scale_x_continuous(limits = c(100, 220), breaks = seq(100, 220, 60)) +
  labs(title = "(c) Skewed (left tail)", x = "GDD", y = "Density") +
  theme_hist

p_abc <- plot_grid(pa, pb, pc, ncol = 3, align = "hv")

# ============================================================
# 3) Observed GDD panels (bottom row)
# ============================================================
plot_hist_obs <- function(obs, title, fill_col) {
  df <- data.frame(GDD5 = obs)
  ggplot(df, aes(GDD5)) +
    geom_histogram(aes(y = after_stat(density)),
                   bins = 30, fill = fill_col, color = "white", alpha = 0.85) +
    geom_density(linewidth = 1.3, color = "black") +
    labs(title = title, x = "Accumulated GDD5", y = "Density") +
    theme_hist
}

p_d <- plot_hist_obs(kyoto_obs_gdd, "(d) Kyoto", "lightblue")
p_e <- plot_hist_obs(liestal_obs_gdd, "(e) Liestal", "lightgreen")
p_f <- plot_hist_obs(washington_obs_gdd, "(f) Washington DC", "gold")

p_def <- plot_grid(p_d, p_e, p_f, ncol = 3, align = "hv")

# ============================================================
# 4) Merge top + bottom rows and save
# ============================================================
p_all <- plot_grid(p_abc, p_def, ncol = 1, align = "v")
p_all

ggsave("Fig5_ABC_DEF_hist.png", p_all, width = 14, height = 9, dpi = 300)
