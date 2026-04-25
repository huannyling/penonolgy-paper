library(tidyverse)
library(rpart)
library(rpart.plot)

# ---------------------------
# Load data
# ---------------------------
df <- read_csv("3cities_doy_weather_1940_2024.csv")

# ---------------------------
# Define predictors
# (from TMAX to CODE65, plus optionally City)
# ---------------------------
clim_vars <- c(
  "TMAX","TMIN","TAVG","TDIFF","PRCP","WIND","EVPR","SUND","WATR",
  "SUND5_cum","GDD5_cum","CDD7_cum",
  "CODE3","CODE51","CODE71","CODE73","CODE61","CODE53","CODE63",
  "CODE55","CODE75","CODE2","CODE0","CODE1","CODE65"
)

# Ensure City is a factor
df <- df %>%
  mutate(City = as.factor(City)) %>%
  select(City, Year, bloom_doy, all_of(clim_vars)) %>%
  drop_na(bloom_doy)

# ---------------------------
# Train/test split (optional but good practice)
# ---------------------------
set.seed(123)
idx <- sample(seq_len(nrow(df)), size = floor(0.8 * nrow(df)))
train <- df[idx, ]
test  <- df[-idx, ]

# ---------------------------
# Fit regression tree with cross-validation
# ---------------------------
fit0 <- rpart(
  bloom_doy ~ City + Year + .,
  data = train %>% select(bloom_doy, City, Year, all_of(clim_vars)),
  method = "anova",
  control = rpart.control(cp = 0.0005, xval = 10, minsplit = 20)
)

# Choose best cp (1-SE rule is often a bit more conservative)
cp_tbl <- fit0$cptable
best_cp <- cp_tbl[which.min(cp_tbl[, "xerror"]), "CP"]

fit <- prune(fit0, cp = best_cp)

# ---------------------------
# Plot tree (like your example)
# ---------------------------
rpart.plot(
  fit,
  type = 2,             # split labels at branches
  extra = 101,          # show predicted value + % obs
  under = TRUE,
  fallen.leaves = TRUE,
  tweak = 1.6,
  box.palette = "RdYlGn" # optional palette
)

# ---------------------------
# Quick performance check
# ---------------------------
pred <- predict(fit, newdata = test)
rmse <- sqrt(mean((pred - test$bloom_doy)^2, na.rm = TRUE))
cat("Test RMSE (days):", round(rmse, 2), "\n")






library(patchwork)

make_tree <- function(dat_city) {
  fit0 <- rpart(
    bloom_doy ~ Year + .,
    data = dat_city %>% select(bloom_doy, Year, all_of(clim_vars)),
    method = "anova",
    control = rpart.control(cp = 0.0005, xval = 10, minsplit = 15)
  )
  best_cp <- fit0$cptable[which.min(fit0$cptable[, "xerror"]), "CP"]
  prune(fit0, cp = best_cp)
}

trees <- df %>%
  group_split(City) %>%
  setNames(levels(df$City)) %>%
  lapply(make_tree)

# Plot each city tree (one by one)
for (nm in names(trees)) {
  cat("\n---", nm, "---\n")
  rpart.plot(trees[[nm]],
             type = 2, extra = 101,
             fallen.leaves = TRUE,
             main = paste("Regression tree:", nm))
}
