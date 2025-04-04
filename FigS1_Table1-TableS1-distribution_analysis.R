### ============================================================
### Load Required Libraries
### ============================================================
library(ggplot2)
library(dplyr)
library(boot)
library(fitdistrplus)
library(e1071)
library(tseries)
library(gridExtra)

### ============================================================
### Load and Prepare Data
### ============================================================
# Read GDD5 data for each city
kyoto_gdd5 <- read.csv("data/kyoto_gdd5_from_Feb.csv")
liestal_gdd5 <- read.csv("data/liestal_gdd5_from_Feb.csv")
washington_gdd5 <- read.csv("data/washington_gdd5_from_Feb.csv")

# Extract GDD5 values corresponding to blooming years (y == 1)
kyoto_bloom <- kyoto_gdd5 %>% filter(y == 1) %>% pull(GDD5)
liestal_bloom <- liestal_gdd5 %>% filter(y == 1) %>% pull(GDD5)
washington_bloom <- washington_gdd5 %>% filter(y == 1) %>% pull(GDD5)

# Combine all city data for ensemble analysis
ensemble_bloom <- c(kyoto_bloom, liestal_bloom, washington_bloom)

### ============================================================
### Helper Functions for Distribution Analysis
### ============================================================

# Remove extreme values (both ends) by a given percentage
remove_extreme_percent <- function(data, percent) {
  if (length(data) < 10) return(rep(NA, length(data)))
  n_remove <- round(length(data) * (percent / 100))
  sorted_data <- sort(data)
  if (length(sorted_data) <= 2 * n_remove) return(rep(NA, length(data)))
  return(sorted_data[(n_remove + 1):(length(sorted_data) - n_remove)])
}

# Compute 5% and 95% quantiles
compute_percentiles <- function(data) {
  if (all(is.na(data)) || length(data) < 10) return(c(NA, NA))
  return(quantile(data, probs = c(0.05, 0.95)))
}

# Shapiro–Wilk test for normality
compute_shapiro_p_value <- function(data) {
  if (all(is.na(data)) || length(data) < 10) return(NA)
  return(shapiro.test(data)$p.value)
}

# Kolmogorov–Smirnov test for uniform distribution
compute_ks_uniform_p_value <- function(data) {
  if (all(is.na(data)) || length(data) < 10) return(NA)
  return(ks.test(data, "punif", min = min(data), max = max(data))$p.value)
}

# AIC ratio between normal and uniform distributions
compute_aic_ratio <- function(data) {
  if (all(is.na(data)) || length(data) < 10) return(NA)
  fit_norm <- fitdist(data, "norm")
  fit_unif <- fitdist(data, "unif")
  return(round(fit_norm$aic / fit_unif$aic, 3))
}

# Compute skewness
compute_skewness <- function(data) {
  if (all(is.na(data)) || length(data) < 10) return(NA)
  return(skewness(data))
}

# Jarque–Bera test for normality
compute_jb_p_value <- function(data) {
  if (all(is.na(data)) || length(data) < 10) return(NA)
  return(jarque.bera.test(data)$p.value)
}

### ============================================================
### Statistical Summary for Table 1 & Table S1
### ============================================================

# Define trimming levels
percentages <- c(0, 5, 10, 20, 30, 40)
all_results <- data.frame()

# Iterate through each trimming level
for (percent in percentages) {
  # Remove extremes from each city's data
  kyoto_trimmed <- remove_extreme_percent(kyoto_bloom, percent)
  liestal_trimmed <- remove_extreme_percent(liestal_bloom, percent)
  washington_trimmed <- remove_extreme_percent(washington_bloom, percent)
  ensemble_trimmed <- remove_extreme_percent(ensemble_bloom, percent)
  
  # Compute all metrics
  results <- data.frame(
    Retained_Percentage = rep(100 - percent, 4),
    City = c("Kyoto", "Liestal", "Washington", "Ensemble"),
    `X5% GDD5` = round(c(compute_percentiles(kyoto_trimmed)[1],
                         compute_percentiles(liestal_trimmed)[1],
                         compute_percentiles(washington_trimmed)[1],
                         compute_percentiles(ensemble_trimmed)[1]), 3),
    `X95% GDD5` = round(c(compute_percentiles(kyoto_trimmed)[2],
                          compute_percentiles(liestal_trimmed)[2],
                          compute_percentiles(washington_trimmed)[2],
                          compute_percentiles(ensemble_trimmed)[2]), 3),
    `Shapiro p-value` = round(c(compute_shapiro_p_value(kyoto_trimmed),
                                compute_shapiro_p_value(liestal_trimmed),
                                compute_shapiro_p_value(washington_trimmed),
                                compute_shapiro_p_value(ensemble_trimmed)), 3),
    `KS Uniform p-value` = round(c(compute_ks_uniform_p_value(kyoto_trimmed),
                                   compute_ks_uniform_p_value(liestal_trimmed),
                                   compute_ks_uniform_p_value(washington_trimmed),
                                   compute_ks_uniform_p_value(ensemble_trimmed)), 3),
    `AIC Ratio` = c(compute_aic_ratio(kyoto_trimmed),
                    compute_aic_ratio(liestal_trimmed),
                    compute_aic_ratio(washington_trimmed),
                    compute_aic_ratio(ensemble_trimmed)),
    `Skewness` = round(c(compute_skewness(kyoto_trimmed),
                         compute_skewness(liestal_trimmed),
                         compute_skewness(washington_trimmed),
                         compute_skewness(ensemble_trimmed)), 3),
    `JB p-value` = round(c(compute_jb_p_value(kyoto_trimmed),
                           compute_jb_p_value(liestal_trimmed),
                           compute_jb_p_value(washington_trimmed),
                           compute_jb_p_value(ensemble_trimmed)), 3)
  )
  
  # Highlight significant values
  results$`Shapiro p-value` <- ifelse(as.numeric(suppressWarnings(results$`Shapiro p-value`)) < 0.05,
                                      paste0(results$`Shapiro p-value`, "*"),
                                      as.character(results$`Shapiro p-value`))
  
  results$`KS Uniform p-value` <- ifelse(as.numeric(suppressWarnings(results$`KS Uniform p-value`)) < 0.05,
                                         paste0(results$`KS Uniform p-value`, "*"),
                                         as.character(results$`KS Uniform p-value`))
  
  results$`Skewness` <- ifelse(as.numeric(suppressWarnings(results$`Skewness`)) > 0.5,
                               paste0(results$`Skewness`, "*"),
                               as.character(results$`Skewness`))
  
  results$`JB p-value` <- ifelse(as.numeric(suppressWarnings(results$`JB p-value`)) < 0.05,
                                 paste0(results$`JB p-value`, "*"),
                                 as.character(results$`JB p-value`))
  
  all_results <- rbind(all_results, results)
}

# Print and optionally export Table 1 / Table S1
print(all_results)
write.csv(all_results, "TS1-GDD5_Analysis_Results.csv", row.names = FALSE)

### ============================================================
### Generate KDE Density Plots for Fig. S1
### ============================================================

# KDE plot settings
percentages <- c(5, 10, 20, 30)
subtitles <- c("(a) Removal 5%", "(b) Removal 10%", "(c) Removal 20%", "(d) Removal 30%")
plot_list <- list()

for (i in seq_along(percentages)) {
  percent <- percentages[i]
  
  kyoto_trimmed <- remove_extreme_percent(kyoto_bloom, percent)
  liestal_trimmed <- remove_extreme_percent(liestal_bloom, percent)
  washington_trimmed <- remove_extreme_percent(washington_bloom, percent)
  ensemble_trimmed <- remove_extreme_percent(ensemble_bloom, percent)
  
  trimmed_data <- data.frame(
    GDD5 = c(kyoto_trimmed, liestal_trimmed, washington_trimmed, ensemble_trimmed),
    City = rep(c("Kyoto", "Liestal", "Washington", "Ensemble"), 
               times = c(length(kyoto_trimmed), length(liestal_trimmed), 
                         length(washington_trimmed), length(ensemble_trimmed)))
  )
  
  p <- ggplot(trimmed_data, aes(x = GDD5, fill = City, color = City)) +
    geom_density(alpha = 0.3, size = 1.2) +
    scale_fill_manual(values = c("blue", "red", "green", "purple")) +
    scale_color_manual(values = c("blue", "red", "green", "purple")) +
    labs(title = subtitles[i], x = "GDD5 Accumulation", y = "Density") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 12, face = "bold"),
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 11)
    )
  
  plot_list[[i]] <- p
}

# Arrange plots into 2x2 layout and save as high-quality TIFF
final_plot <- grid.arrange(grobs = plot_list, ncol = 2, nrow = 2)
ggsave("FigS1-GDD5_KDE_Distributions.tiff", plot = final_plot, dpi = 300, 
       width = 10, height = 8, units = "in", compression = "lzw")
