library(tidyverse)
library(ranger)      # fast random forest
library(vip)         # variable importance plots (optional)

df <- read_csv("other_cities_weather_data_330city_average_with_bloom.csv")
glimpse(df)


vars <- c("TMAX","WIND","EVPR","GDD5_cum","CODE0","CODE2","CODE51","CODE63","lat","long","alt")

dat <- df %>%
  mutate(
    Continent = as.factor(continent)
  ) %>%
  select(continent, bloom_doy, all_of(vars)) %>%
  drop_na()


set.seed(123)

fit_one_continent <- function(d) {
  # random forest regression with permutation importance
  fit <- ranger(
    bloom_doy ~ .,
    data = d %>% select(bloom_doy, all_of(vars)),
    num.trees = 1000,
    mtry = floor(sqrt(length(vars))),
    min.node.size = 5,
    importance = "permutation",
    respect.unordered.factors = "order",
    seed = 123
  )
  
  imp <- ranger::importance(fit)
  
  tibble(
    variable = names(imp),
    importance = as.numeric(imp)
  ) %>%
    arrange(desc(importance)) %>%
    mutate(rank = row_number())
}

imp_by_continent <- dat %>%
  group_by(continent) %>%
  group_modify(~ fit_one_continent(.x)) %>%
  ungroup()

imp_by_continent


ggplot(imp_by_continent, aes(x = variable, y = rank, color = continent)) +
  geom_point(size = 3) +
  geom_line(aes(group = continent), alpha = 0.5) +
  scale_y_reverse(breaks = 1:length(vars)) +
  labs(
    x = "Predictor",
    y = "Importance rank (1 = most important)",
    title = "Driver rank order of bloom DOY differs across continents"
  ) +
  theme_classic() +
  theme(text = element_text(size = 14))


p<-ggplot(imp_by_continent, aes(x = reorder(variable, importance), y = importance)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~ continent, scales = "free_y") +
  labs(
    x = NULL,
    y = "Permutation importance",
    title = "Permutation importance of climate/geographic drivers by continent"
  ) +
  scale_x_discrete(labels = toupper)+
  theme_classic() +
  theme(
    text = element_text(size = 14),
    plot.title = element_text(face='bold')
        )

p
ggsave("perm_importance.png", plot=p, width=9, height=5, dpi = 300)





imp_norm <- imp_by_continent %>%
  group_by(continent) %>%
  mutate(importance_norm = importance / sum(importance)) %>%
  ungroup()

ggplot(imp_norm, aes(x = reorder(variable, importance_norm), y = importance_norm)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~ continent) +
  labs(
    x = NULL,
    y = "Normalized importance (sums to 1 within continent)",
    title = "Relative driver importance by continent"
  ) +
  theme_classic() +
  theme(text = element_text(size = 14))
