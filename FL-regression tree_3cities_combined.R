library(tidyverse)
library(rpart)
library(rpart.plot)
library(magick)

# ---------------------------
# Load data
# ---------------------------
df <- read_csv("3cities_doy_weather_1940_2024.csv")

# ---------------------------
# Define predictors
# ---------------------------
clim_vars <- c(
  "TMAX","TAVG","PRCP","EVPR","SUND","WATR",
  "SUND5_cum","GDD5_cum","CDD7_cum",
  "CODE3","CODE51","CODE71","CODE73","CODE61","CODE53","CODE63",
  "CODE55","CODE75","CODE2","CODE0","CODE1","CODE65"
)

# ---------------------------
# Prepare data
# ---------------------------
df <- df %>%
  mutate(City = factor(City, levels = c("Kyoto", "Liestal", "Washington"))) %>%
  select(City, bloom_doy, all_of(clim_vars)) %>%
  drop_na(bloom_doy)

# ---------------------------
# Train/test split for overall tree
# ---------------------------
set.seed(123)
idx <- sample(seq_len(nrow(df)), size = floor(0.8 * nrow(df)))
train <- df[idx, ]
test  <- df[-idx, ]

# ---------------------------
# Overall regression tree
# City included here if you want pooled tree with city effect
# If you want pure environmental tree, remove City from formula/data
# ---------------------------
fit0 <- rpart(
  bloom_doy ~ City + .,
  data = train %>% select(bloom_doy, City, all_of(clim_vars)),
  method = "anova",
  control = rpart.control(cp = 0.0005, xval = 10, minsplit = 20)
)

cp_tbl <- fit0$cptable
best_cp <- cp_tbl[which.min(cp_tbl[, "xerror"]), "CP"]
fit <- prune(fit0, cp = best_cp)

# ---------------------------
# Plot overall tree
# ---------------------------
png("overall_tree_no_year.png", width = 2400, height = 1600, res = 300)
rpart.plot(
  fit,
  type = 2,
  extra = 101,
  under = TRUE,
  fallen.leaves = TRUE,
  compress = FALSE,
  cex = 1.1,
  tweak = 1.2,
  split.cex = 0.9,
  nn.cex = 0.8,
  branch.lwd = 2,
  branch.col = "grey40",
  box.palette = "RdYlGn",
  shadow.col = NA,
  main = "Overall regression tree",
  cex.main = 2.0
)
dev.off()

# ---------------------------
# Quick performance check
# ---------------------------
pred <- predict(fit, newdata = test)
rmse <- sqrt(mean((pred - test$bloom_doy)^2, na.rm = TRUE))
cat("Overall tree test RMSE (days):", round(rmse, 2), "\n")

# ============================================================
# City-specific trees (NO Year)
# ============================================================

make_tree <- function(dat_city) {
  fit0 <- rpart(
    bloom_doy ~ .,
    data = dat_city %>% select(bloom_doy, all_of(clim_vars)),
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

# ---------------------------
# Color palette for each city
# ---------------------------
city_palettes <- list(
  Kyoto = "Blues",
  Liestal = "Greens",
  Washington = "Oranges"
)

city_titles <- c(
  Kyoto = "Kyoto",
  Liestal = "Liestal",
  Washington = "Washington DC"
)

# ---------------------------
# Save individual tree plots
# ---------------------------
dir.create("city_trees", showWarnings = FALSE)

for (nm in names(trees)) {
  png(
    filename = file.path("city_trees", paste0(nm, "_tree.png")),
    width = 2400,
    height = 1600,
    res = 300
  )
  
  rpart.plot(
    trees[[nm]],
    type = 2,
    extra = 101,
    under = TRUE,
    fallen.leaves = TRUE,
    compress = FALSE,
    cex = 1.1,
    tweak = 1.2,
    split.cex = 0.9,
    nn.cex = 0.8,
    branch.lwd = 2,
    branch.col = "grey40",
    box.palette = city_palettes[[nm]],
    shadow.col = NA,
    nn = FALSE,
    main = city_titles[[nm]],
    cex.main = 2.2
  )
  
  dev.off()
}

# ---------------------------
# Combine 3 city tree PNGs into one figure
# ---------------------------
img_kyoto <- image_read("city_trees/Kyoto_tree.png")
img_liestal <- image_read("city_trees/Liestal_tree.png")
img_wash <- image_read("city_trees/Washington_tree.png")

target_height <- min(
  image_info(img_kyoto)$height,
  image_info(img_liestal)$height,
  image_info(img_wash)$height
)

img_kyoto <- image_resize(img_kyoto, paste0("x", target_height))
img_liestal <- image_resize(img_liestal, paste0("x", target_height))
img_wash <- image_resize(img_wash, paste0("x", target_height))

combined <- image_append(c(img_kyoto, img_liestal, img_wash))

image_write(
  combined,
  path = "city_trees/combined_city_trees.png",
  format = "png",
  density = 300
)

cat("Combined figure saved to: city_trees/combined_city_trees.png\n")
