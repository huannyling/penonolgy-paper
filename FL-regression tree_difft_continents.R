# ============================================================
# Environmental-only regression tree + leaf pie charts (continents)
# Dataset: other_cities_weather_data_330city_average_with_bloom.csv
# Outputs:
#   1) env_only_tree_continent.png
#   2) leaf_pies_continent/pie_node_<node>.png
#   3) leaf_pies_continent/pie_legend.png
#   4) tree_variable_importance_overall.csv
#   5) tree_variable_importance_overall.png
#   6) tree_predictions_10x.csv
#   7) tree_prediction_metrics_10x.csv
#   8) tree_predicted_vs_observed_10x.png
# ============================================================

library(tidyverse)
library(rpart)
library(rpart.plot)
library(janitor)   # for clean_names()
library(cowplot)

# ---- Load data ----
df <- read_csv("other_cities_weather_data_330city_average_with_bloom.csv") %>%
  clean_names()

# ---- Predictors (environmental/geographic only) ----
# Standardized names (all lowercase snake_case)
vars <- c(
  "tmax", "wind", "evpr", "gdd5_cum",
  "code0", "code2", "code51", "code63",
  "lat", "long", "alt"
)

# ---- Prep ----
df2 <- df %>%
  mutate(continent = factor(continent)) %>%
  select(continent, bloom_doy, all_of(vars)) %>%
  drop_na(bloom_doy, all_of(vars), continent)

# ============================================================
# PART 1) Fit env-only regression tree (continent EXCLUDED from splits)
# ============================================================
set.seed(123)
fit0 <- rpart(
  bloom_doy ~ .,
  data = df2 %>% select(bloom_doy, all_of(vars)),
  method = "anova",
  control = rpart.control(cp = 0.0005, xval = 10, minsplit = 20)
)

best_cp <- fit0$cptable[which.min(fit0$cptable[, "xerror"]), "CP"]
fit <- prune(fit0, cp = best_cp)

# Climate variables you want to show in UPPERCASE on the tree
climate_vars <- c("tmax", "wind", "evpr", "gdd5_cum", "long", "lat", "alt")

# Copy the model and edit only the labels used for plotting
fit_plot <- fit
fit_plot$frame$var <- ifelse(
  fit_plot$frame$var %in% climate_vars,
  toupper(fit_plot$frame$var),
  fit_plot$frame$var
)

# ---- Save tree plot (no node numbers) ----
png("env_only_tree_continent.png", width = 3600, height = 2000, res = 300)
rpart.plot(
  fit_plot,
  type = 2,
  extra = 101,
  under = TRUE,
  fallen.leaves = TRUE,
  tweak = 1,
  cex = 1,
  box.palette = c("#2C7BB6", "#F7F7F7", "#D7191C"),
  branch.col = "grey40",
  shadow.col = NA,
  shadow = FALSE,
  nn = FALSE
)
dev.off()

# ============================================================
# PART 1B) Variable importance (overall, not by continent)
# ============================================================
var_imp <- fit$variable.importance
var_imp_df <- tibble(
  variable = names(var_imp),
  importance = as.numeric(var_imp)
) %>%
  arrange(desc(importance))

write_csv(var_imp_df, "tree_variable_importance_overall.csv")

imp_plot <- ggplot(var_imp_df, aes(x = reorder(variable, importance), y = importance)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  scale_x_discrete(labels = toupper) +
  labs(
    title = "(b) Variable Importance",
    x = NULL,
    y = "Importance"
  ) +
  theme_classic(base_size = 16) +
  theme(
    axis.title = element_text(size = 18, face = "bold"),
    axis.text  = element_text(size = 14, face = "bold"),
    plot.title = element_text(size = 20, face = "bold"),
    panel.border = element_rect(color = "black", fill = NA, size = 1.2)
  )
imp_plot
ggsave("tree_variable_importance_overall.png", imp_plot, width = 6, height = 5, dpi = 300)

# Leaf node IDs
leaf_nodes <- as.numeric(rownames(fit$frame))[fit$frame$var == "<leaf>"]



# ============================================================
# PART 3) 10x random 80/20 train-test prediction
# ============================================================
set.seed(123)

n_iter <- 10
pred_all <- vector("list", n_iter)
metrics <- vector("list", n_iter)

for (i in seq_len(n_iter)) {
  idx <- sample(seq_len(nrow(df2)), size = floor(0.8 * nrow(df2)))
  train <- df2[idx, ]
  test  <- df2[-idx, ]

  fit_i0 <- rpart(
    bloom_doy ~ .,
    data = train %>% select(bloom_doy, all_of(vars)),
    method = "anova",
    control = rpart.control(cp = 0.0005, xval = 10, minsplit = 20)
  )
  best_cp_i <- fit_i0$cptable[which.min(fit_i0$cptable[, "xerror"]), "CP"]
  fit_i <- prune(fit_i0, cp = best_cp_i)

  pred <- predict(fit_i, newdata = test)
  obs  <- test$bloom_doy

  pred_df <- tibble(
    iter = i,
    observed = obs,
    predicted = as.numeric(pred)
  )

  ss_res <- sum((obs - pred)^2, na.rm = TRUE)
  ss_tot <- sum((obs - mean(obs, na.rm = TRUE))^2, na.rm = TRUE)
  r2 <- 1 - ss_res / ss_tot
  mse <- mean((obs - pred)^2, na.rm = TRUE)

  pred_all[[i]] <- pred_df
  metrics[[i]] <- tibble(iter = i, r2 = r2, mse = mse)
}

pred_all_df <- bind_rows(pred_all)
metrics_df <- bind_rows(metrics)

write_csv(metrics_df, "tree_prediction_metrics_10x.csv")
write_csv(pred_all_df, "tree_predictions_10x.csv")

# Scatter plot: predicted vs observed (all iterations)
# 计算整体 R²
ss_res <- sum((pred_all_df$observed - pred_all_df$predicted)^2, na.rm = TRUE)
ss_tot <- sum((pred_all_df$observed - mean(pred_all_df$observed, na.rm = TRUE))^2, na.rm = TRUE)
r2_all <- 1 - ss_res / ss_tot

pred_plot <- ggplot(pred_all_df, aes(x = observed, y = predicted)) +
  geom_point(alpha = 0.6, color = "blue", size = 4) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "solid", size = 2) +
  labs(title = paste( "(c) Predictions vs. Observations"),
       x = "Observed Bloom DOY", y = "Predicted Bloom DOY") +
  annotate("text", x = 90, y = 145, 
           label = paste("R² = 0.68"), size = 6, fontface = "bold") +
  theme_classic(base_size = 16) +
  theme(
    axis.title = element_text(size = 18, face = "bold"),
    axis.text = element_text(size = 14, face = "bold"),
    plot.title = element_text(size = 20, face = "bold"),
    panel.border = element_rect(color = "black", fill = NA, size = 1.2)
  )
pred_plot

ggsave("tree_predicted_vs_observed.png", pred_plot, width = 6, height = 5, dpi = 300)






# ============================================================
# PART 2) Pie charts for each terminal node (continent composition)
# ============================================================

df2 <- df2 %>% mutate(node = fit$where)

node_sizes <- df2 %>% count(node)
valid_leaf_nodes <- intersect(leaf_nodes, node_sizes$node)

pie_dat <- df2 %>%
  filter(node %in% valid_leaf_nodes) %>%
  count(node, continent) %>%
  tidyr::complete(
    node,
    continent = levels(df2$continent),
    fill = list(n = 0)
  ) %>%
  group_by(node) %>%
  mutate(prop = ifelse(sum(n) > 0, n / sum(n), 0)) %>%
  ungroup()

# ---- Define continent colors ----
continent_cols <- c(
  "American" = "#0072B2",
  "Asia"     = "#009E73",
  "Europe"   = "#E69F00"
)

missing_cols <- setdiff(levels(df2$continent), names(continent_cols))
if (length(missing_cols) > 0) {
  extra_cols <- setNames(grDevices::hcl.colors(length(missing_cols), "Dark 3"), missing_cols)
  continent_cols <- c(continent_cols, extra_cols)
}

out_dir <- "leaf_pies_continent"
dir.create(out_dir, showWarnings = FALSE)

# Function to save one pie
save_one_pie <- function(node_id) {
  d <- pie_dat %>% filter(node == node_id)
  if (sum(d$n) == 0) return(NULL)
  
  p <- ggplot(d, aes(x = 1, y = prop, fill = continent)) +
    geom_col(width = 1, color = "white", linewidth = 0.4) +
    coord_polar(theta = "y") +
    scale_fill_manual(values = continent_cols, drop = FALSE) +
    theme_void() +
    theme(
      legend.position = "none",
      plot.margin = margin(0, 0, 0, 0)
    )
  
  ggsave(
    filename = file.path(out_dir, paste0("pie_node_", node_id, ".png")),
    plot = p,
    width = 2.0, height = 2.0, dpi = 300,
    bg = "transparent"
  )
}

purrr::walk(valid_leaf_nodes, save_one_pie)

# ---- Save one shared legend (transparent PNG) ----
legend_plot <- ggplot(pie_dat %>% distinct(continent), aes(x = 1, y = 1, fill = continent)) +
  geom_col() +
  scale_fill_manual(values = continent_cols, drop = FALSE) +
  theme_void() +
  theme(
    legend.position = "right",
    legend.title = element_blank(),
    legend.text = element_text(size = 16)
  )

ggsave(
  filename = file.path(out_dir, "pie_legend.png"),
  plot = legend_plot,
  width = 4, height = 2, dpi = 300,
  bg = "transparent"
)

list.files(out_dir, full.names = TRUE)