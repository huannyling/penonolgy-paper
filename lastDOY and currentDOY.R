library(tidyverse)

# 读取数据
df <- read_csv("3cities_doy_weather_1940_2024.csv")

# 按城市分组，构建上一年 DOY
df_lag <- df %>%
  arrange(City, Year) %>%
  group_by(City) %>%
  mutate(DOY_lag1 = lag(bloom_doy, 1)) %>%   # 上一年 DOY
  ungroup()

# 删除没有上一年的（1940）
df_lag <- df_lag %>%
  filter(!is.na(DOY_lag1))

df_lag %>%
  group_by(City) %>%
  summarise(
    cor_DOY = cor(bloom_doy, DOY_lag1),
    p_value = cor.test(bloom_doy, DOY_lag1)$p.value
  )


lm_test <- df_lag %>%
  group_by(City) %>%
  do({
    model <- lm(bloom_doy ~ DOY_lag1, data = .)
    tibble(
      R2 = summary(model)$r.squared,
      p_value = summary(model)$coefficients[2,4]
    )
  })

lm_test

ggplot(df_lag, aes(x = DOY_lag1, y = bloom_doy, color = City)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_classic(base_size = 16) +
  labs(
    x = "Previous year bloom DOY",
    y = "Current year bloom DOY",
    title = "Temporal autocorrelation in bloom timing"
  )
