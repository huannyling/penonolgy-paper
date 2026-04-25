library(tidyverse)
library(rpart)
library(rpart.plot)

# ---- Load data ----
df <- read_csv("3cities_doy_weather_1940_2024.csv")

clim_vars <- c(
  "TMAX","TMIN","TAVG","TDIFF","PRCP","WIND","EVPR","SUND","WATR",
  "SUND5_cum","GDD5_cum","CDD7_cum",
  "CODE3","CODE51","CODE71","CODE73","CODE61","CODE53","CODE63",
  "CODE55","CODE75","CODE2","CODE0","CODE1","CODE65"
)

df2 <- df %>%
  mutate(City = factor(City)) %>%
  select(City, Year, bloom_doy, all_of(clim_vars)) %>%
  drop_na(bloom_doy)

# ---- Fit env-only tree (City excluded) ----
set.seed(123)
fit0 <- rpart(
  bloom_doy ~ Year + .,
  data = df2 %>% select(bloom_doy, Year, all_of(clim_vars)),
  method = "anova",
  control = rpart.control(cp = 0.0005, xval = 10, minsplit = 20)
)

best_cp <- fit0$cptable[which.min(fit0$cptable[, "xerror"]), "CP"]
fit <- prune(fit0, cp = best_cp)

# ---- Save tree plot (with node numbers) ----
png("env_only_tree.png", width = 2400, height = 1400, res = 200)
rpart.plot(
  fit,
  type = 2,
  extra = 101,
  under = TRUE,
  fallen.leaves = TRUE,
  tweak = 1.5,
  box.palette = "RdYlGn",
  nn = TRUE           # IMPORTANT: show node numbers to match pies
)
dev.off()

# (Optional) see leaf node IDs
leaf_nodes <- as.numeric(rownames(fit$frame))[fit$frame$var == "<leaf>"]
leaf_nodes




library(tidyverse)

# df2 and fit assumed already created from Part 1

# Identify leaf nodes
leaf_nodes <- as.numeric(rownames(fit$frame))[fit$frame$var == "<leaf>"]

# Assign each observation to a node (rpart stores node membership in fit$where)
df2 <- df2 %>% mutate(node = fit$where)

# City counts per leaf node
pie_dat <- df2 %>%
  filter(node %in% leaf_nodes) %>%
  count(node, City) %>%
  group_by(node) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

# Fix city order + colors (consistent across pies)
city_levels <- levels(df2$City)
pie_dat <- pie_dat %>% mutate(City = factor(City, levels = city_levels))

city_cols <- c(
  "Kyoto" = "#1f77b4",
  "Liestal" = "#2ca02c",
  "Washington" = "#d62728"
)
# If your City names differ, update city_cols names to match levels(df2$City)

# Create output folder
dir.create("leaf_pies", showWarnings = FALSE)

# Function to save one pie
save_one_pie <- function(node_id) {
  d <- pie_dat %>% filter(node == node_id)
  
  p <- ggplot(d, aes(x = 1, y = prop, fill = City)) +
    geom_col(width = 1, color = "white", linewidth = 0.4) +
    coord_polar(theta = "y") +
    scale_fill_manual(values = city_cols, drop = FALSE) +
    theme_void() +
    theme(
      legend.position = "none",
      plot.margin = margin(0, 0, 0, 0)
    )
  
  # Save transparent PNG (great for PPT overlay)
  ggsave(
    filename = file.path("leaf_pies", paste0("pie_node_", node_id, ".png")),
    plot = p,
    width = 2.0, height = 2.0, dpi = 300,
    bg = "transparent"
  )
}

# Save pies for all leaf nodes
purrr::walk(leaf_nodes, save_one_pie)

# Also save a legend (optional, for PPT)
legend_plot <- ggplot(pie_dat, aes(x = 1, y = prop, fill = City)) +
  geom_col() +
  scale_fill_manual(values = city_cols, drop = FALSE) +
  theme_void() +
  theme(legend.position = "right")
legend_plot
ggsave("leaf_pies/pie_legend.png", legend_plot, width = 4, height = 2, dpi = 300, bg = "transparent")

leaf_nodes

