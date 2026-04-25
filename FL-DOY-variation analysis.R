library(tidyverse)
library(broom)
library(lubridate)
library(lme4)
# load data
df <- read_csv("3cities_doy_weather_1940_2024.csv")

# quick check
glimpse(df)

df_bin <- df %>%
  filter(Year >= 1940, Year <= 2024) %>%
  mutate(
    bin_start = 1940 + 5 * floor((Year - 1940) / 5),
    bin_end   = bin_start + 4,
    bin_mid   = bin_start + 2
  )


bin_summary <- df_bin %>%
  group_by(City, bin_start, bin_end, bin_mid) %>%
  summarise(
    n_years = n(),
    doy_mean = mean(bloom_doy, na.rm = TRUE),
    doy_sd   = sd(bloom_doy, na.rm = TRUE),
    doy_iqr  = IQR(bloom_doy, na.rm = TRUE),
    doy_range = max(bloom_doy, na.rm = TRUE) - min(bloom_doy, na.rm = TRUE),
    .groups = "drop"
  )

summary(bin_summary)

m_sd <- lm(doy_sd ~ bin_mid * City, data = bin_summary)
summary(m_sd)

m_sd2 <- lmer(doy_sd ~ bin_mid + (1|City), data = bin_summary)
summary(m_sd2)

m_iqr <- lm(doy_iqr ~ bin_mid * City, data = bin_summary)
summary(m_iqr)


figS2<-ggplot(bin_summary, aes(x = City, y = doy_sd, fill = City)) +
  geom_boxplot(alpha = 0.6, width = 0.6) +
  stat_compare_means(
    method = "wilcox.test",
    comparisons = list(
      c("Kyoto", "Washington"),
      c("Kyoto", "Liestal"),
      c("Washington", "Liestal")
    ),
    label = "p.signif",
    step.increase = 0.12
  ) +
  
  labs(
    y = "DOY variability (SD, days)",
    x = NULL,
    title = "Comparison of DOY variability across cities"
  ) +
  theme_classic() +
  theme(legend.position = "none")
figS2

figS3<-ggplot(bin_summary,
       aes(x = bin_mid, y = doy_sd, color = City)) +
  
  # error bars (± SE of SD)
  geom_errorbar(
    aes(
      ymin = doy_sd - doy_sd / sqrt(2 * (n_years - 1)),
      ymax = doy_sd + doy_sd / sqrt(2 * (n_years - 1))
    ),
    width = 1.2,
    alpha = 0.6
  ) +
  
  # points
  geom_point(size = 2) +
  
  # linear trend
  geom_smooth(method = "lm", se = TRUE) +
  
  labs(
    x = "Calendar Year (5-year bin midpoint)",
    y = "DOY variability (SD, days)",
    title = "Five-year binned variability of cherry blossom DOY"
  ) +
  
  theme_classic() +
  theme(
    text = element_text(size = 14),
    legend.title = element_blank()
  )

figS3








city_slopes <- bin_summary %>%
  group_by(City) %>%
  do(tidy(lm(doy_sd ~ bin_mid, data = .))) %>%
  filter(term == "bin_mid")

city_slopes

tidy(m_sd, conf.int = TRUE)
glance(m_sd)



bin_long <- bin_summary %>%
  select(City, bin_mid, n_years, doy_sd, doy_iqr) %>%
  pivot_longer(cols = c(doy_sd, doy_iqr),
               names_to = "metric", values_to = "value") %>%
  mutate(metric = recode(metric,
                         doy_sd = "SD",
                         doy_iqr = "IQR"))

ggplot(bin_long, aes(x = bin_mid, y = value, color = City)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = TRUE) +
  facet_wrap(~ metric, scales = "free_y") +
  labs(
    x = "Calendar Year (5-year bin midpoint)",
    y = "Variability (days)",
    title = "DOY variability over time by city (SD and IQR)"
  ) +
  theme_classic() +
  theme(text = element_text(size = 14), legend.title = element_blank())





ggplot(bin_summary, aes(x = bin_mid, y = City, fill = doy_sd)) +
  geom_tile() +
  labs(
    x = "Calendar Year (5-year bin midpoint)",
    y = NULL,
    fill = "SD (days)",
    title = "Heatmap of DOY variability (SD) by city and time"
  ) +
  theme_minimal(base_size = 14)
