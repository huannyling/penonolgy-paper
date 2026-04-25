library(readr)
library(dplyr)
library(ggplot2)
library(stringr)
library(patchwork)
library(scales)
library(tibble)

root_dir <- "/Users/wenhuan/Documents/Cherry blossom project/Revision -Fangliang He"
data_dir <- file.path(root_dir, "data")

continent_colors <- c(
  "Asia" = "#D95F5F",
  "Europe" = "#2A9D8F",
  "North America" = "#4C78A8"
)

city_colors <- c(
  "Kyoto" = "#E76F51",
  "Liestal" = "#2A9D8F",
  "Washington DC" = "#4C78A8"
)

theme_city <- theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    panel.grid = element_blank(),
    plot.margin = margin(8, 8, 8, 8)
  )

theme_main <- theme_classic(base_size = 14) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 11),
    strip.text = element_text(size = 12, face = "bold"),
    strip.background = element_rect(fill = "grey96", color = "grey82"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    plot.margin = margin(8, 8, 8, 8)
  )

normalize_key <- function(x) {
  x %>%
    iconv(to = "ASCII//TRANSLIT") %>%
    str_replace_all("[^A-Za-z0-9]+", "") %>%
    str_to_lower()
}

format_p_value <- function(p) {
  if (is.na(p)) {
    "p = NA"
  } else if (p < 0.001) {
    "p < 0.001"
  } else if (p < 0.01) {
    "p < 0.01"
  } else {
    paste0("p = ", format(round(p, 2), nsmall = 2))
  }
}

make_eq_label_r2 <- function(df, slope_digits = 3) {
  mod <- lm(bloom_doy ~ year, data = df)
  coefs <- coef(mod)
  r2 <- summary(mod)$r.squared
  paste0(
    "DOY = ", round(coefs[1], 2),
    ifelse(coefs[2] < 0, " - ", " + "),
    abs(round(coefs[2], slope_digits)), " * Year\n",
    "R² = ", round(r2, 2)
  )
}

format_p_value_plotmath <- function(p) {
  if (is.na(p)) {
    "italic(p) == NA"
  } else if (p < 0.001) {
    "italic(p) < 0.001"
  } else if (p < 0.01) {
    paste0("italic(p) == ", format(round(p, 3), nsmall = 3))
  } else {
    paste0("italic(p) == ", format(round(p, 2), nsmall = 2))
  }
}

format_p_value_plotmath_city <- function(p, force_p_lt = FALSE) {
  if (is.na(p)) {
    "italic(p) == NA"
  } else if (force_p_lt && p < 0.01) {
    "italic(p) < 0.01"
  } else if (p < 0.001) {
    "italic(p) < 0.001"
  } else if (p < 0.01) {
    paste0("italic(p) == ", format(round(p, 3), nsmall = 3))
  } else {
    paste0("italic(p) == ", format(round(p, 2), nsmall = 2))
  }
}

make_eq_label_parse <- function(df, slope_digits = 3) {
  mod <- lm(bloom_doy ~ year, data = df)
  coefs <- coef(mod)
  r2 <- summary(mod)$r.squared
  p <- summary(mod)$coefficients[2, 4]

  paste0(
    "atop(",
    "DOY == ", round(coefs[1], 2),
    ifelse(coefs[2] < 0, " - ", " + "),
    abs(round(coefs[2], slope_digits)), " %.% Year",
    ", ",
    "R^2 == ", round(r2, 2), " * ', ' ~ ",
    format_p_value_plotmath(p),
    ")"
  )
}

make_eq_label <- function(df, digits = 3) {
  mod <- lm(bloom_doy ~ year, data = df)
  coefs <- coef(mod)
  r2 <- summary(mod)$r.squared
  p <- summary(mod)$coefficients[2, 4]
  paste0(
    "DOY = ", round(coefs[1], 2),
    ifelse(coefs[2] < 0, " - ", " + "),
    abs(round(coefs[2], digits)), " * Year\n",
    "R^2 = ", round(r2, 2), ", ", format_p_value(p)
  )
}

plot_city_trend <- function(df, title, color, label_x_offset, label_y = 148, force_p_lt = FALSE) {
  mod <- lm(bloom_doy ~ year, data = df)
  coefs <- coef(mod)
  r2 <- summary(mod)$r.squared
  p <- summary(mod)$coefficients[2, 4]
  eq <- paste0(
    "atop(",
    "DOY == ", round(coefs[1], 2),
    ifelse(coefs[2] < 0, " - ", " + "),
    abs(round(coefs[2], 3)), " %.% Year",
    ", ",
    "R^2 == ", round(r2, 2), " * ', ' ~ ",
    format_p_value_plotmath_city(p, force_p_lt = force_p_lt),
    ")"
  )

  ggplot(df, aes(x = year, y = bloom_doy)) +
    geom_line(color = alpha(color, 0.75), linewidth = 0.45) +
    geom_point(color = color, size = 1.6, alpha = 0.85) +
    geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed", linewidth = 0.9) +
    annotate(
      "text",
      x = min(df$year, na.rm = TRUE) + label_x_offset,
      y = label_y,
      label = eq,
      hjust = 0,
      vjust = 1,
      size = 4.5,
      parse = TRUE
    ) +
    coord_cartesian(ylim = c(70, 150)) +
    labs(title = title, x = "Year", y = "Blooming Day (DOY)") +
    theme_city
}

make_country_bloom <- function(file, continent, country) {
  read_csv(file.path(data_dir, file), show_col_types = FALSE) %>%
    mutate(
      location = str_trim(location),
      city = str_trim(str_extract(location, "[^/]+$")),
      continent = continent,
      country = country
    )
}

# ---------------------------
# Panels a-d from original Fig. 2
# ---------------------------
kyoto <- read_csv(file.path(data_dir, "kyoto.csv"), show_col_types = FALSE)
liestal <- read_csv(file.path(data_dir, "liestal.csv"), show_col_types = FALSE)
washingtondc <- read_csv(file.path(data_dir, "washingtondc.csv"), show_col_types = FALSE)

city_data <- bind_rows(
  kyoto %>% mutate(city = "Kyoto"),
  liestal %>% mutate(city = "Liestal"),
  washingtondc %>% mutate(city = "Washington DC")
) %>%
  mutate(
    year = as.numeric(year),
    bloom_doy = as.numeric(bloom_doy)
  )

panel_a <- city_data %>%
  filter(city == "Kyoto", year >= 1550, year <= 1850) %>%
  plot_city_trend("(a) Kyoto (1550-1850)", city_colors["Kyoto"], label_x_offset = 16)

panel_b <- city_data %>%
  filter(city == "Kyoto", year >= 1924, year <= 2024) %>%
  plot_city_trend("(b) Kyoto (1924-2024)", city_colors["Kyoto"], label_x_offset = 4, force_p_lt = TRUE)

panel_c <- city_data %>%
  filter(city == "Liestal", year >= 1924, year <= 2024) %>%
  plot_city_trend("(c) Liestal (1924-2024)", city_colors["Liestal"], label_x_offset = 4, force_p_lt = TRUE)

panel_d <- city_data %>%
  filter(city == "Washington DC", year >= 1924, year <= 2024) %>%
  plot_city_trend("(d) Washington, DC (1924-2024)", city_colors["Washington DC"], label_x_offset = 4, force_p_lt = TRUE)

# ---------------------------
# Bloom datasets for panel f
# ---------------------------
japan_bloom <- make_country_bloom("japan.csv", "Asia", "Japan")
korea_bloom <- make_country_bloom("south_korea.csv", "Asia", "South Korea")
swiss_bloom <- make_country_bloom("meteoswiss.csv", "Europe", "Switzerland")
usa_bloom <- make_country_bloom("usa_npn_like_south_korea.csv", "North America", "USA")

all_bloom <- bind_rows(japan_bloom, korea_bloom, swiss_bloom, usa_bloom) %>%
  mutate(
    year = as.numeric(year),
    bloom_doy = as.numeric(bloom_doy),
    location_key = normalize_key(location)
  ) %>%
  filter(is.finite(year), is.finite(bloom_doy))

# ---------------------------
# Climate inputs for panel e
# ---------------------------
weather_daily <- read_csv(
  file.path(data_dir, "other_cities_weather_data_final_combined_330_processed.csv"),
  show_col_types = FALSE
) %>%
  mutate(
    city = str_trim(city),
    location_key = normalize_key(city),
    TAVG = as.numeric(TAVG),
    PRCP = as.numeric(PRCP)
  )

eurasia_bloom_sites <- bind_rows(japan_bloom, korea_bloom, swiss_bloom) %>%
  distinct(location, lat, long, alt, continent, country) %>%
  mutate(location_key = normalize_key(location))

eurasia_climate <- weather_daily %>%
  group_by(location_key, city, year) %>%
  summarise(
    mat_year = mean(TAVG, na.rm = TRUE),
    map_year = sum(PRCP, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(location_key) %>%
  summarise(
    mat = mean(mat_year, na.rm = TRUE),
    map = mean(map_year, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  inner_join(eurasia_bloom_sites, by = "location_key")

usa_pheno <- read_csv(
  file.path(data_dir, "USA-NPN_individual_phenometrics_data.csv"),
  show_col_types = FALSE
)

climate_cols <- c(
  "Tmax_Winter", "Tmax_Spring", "Tmax_Summer", "Tmax_Fall",
  "Tmin_Winter", "Tmin_Spring", "Tmin_Summer", "Tmin_Fall",
  "Prcp_Winter", "Prcp_Spring", "Prcp_Summer", "Prcp_Fall"
)

usa_pheno[climate_cols] <- lapply(usa_pheno[climate_cols], function(x) {
  x <- as.numeric(x)
  x[x <= -9999] <- NA_real_
  x
})

usa_climate <- usa_pheno %>%
  transmute(
    site_id = Site_ID,
    state = State,
    lat = Latitude,
    long = Longitude,
    alt = Elevation_in_Meters,
    mat = rowMeans(cbind(
      (Tmax_Winter + Tmin_Winter) / 2,
      (Tmax_Spring + Tmin_Spring) / 2,
      (Tmax_Summer + Tmin_Summer) / 2,
      (Tmax_Fall + Tmin_Fall) / 2
    ), na.rm = TRUE),
    map = Prcp_Winter + Prcp_Spring + Prcp_Summer + Prcp_Fall
  ) %>%
  filter(is.finite(mat), is.finite(map), map > 0) %>%
  mutate(location = paste0("USA/", state, "/Site_", site_id)) %>%
  group_by(location, lat, long, alt) %>%
  summarise(
    mat = mean(mat, na.rm = TRUE),
    map = mean(map, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    continent = "North America",
    country = "USA"
  )

biome_points <- bind_rows(
  eurasia_climate %>% select(location, lat, long, alt, continent, country, mat, map),
  usa_climate %>% select(location, lat, long, alt, continent, country, mat, map)
) %>%
  filter(is.finite(mat), is.finite(map), map > 0) %>%
  mutate(precip_cm = map / 10)

biome_palette <- c(
  "Tundra" = "#C1E1DD",
  "Boreal forest" = "#A5C790",
  "Temperate seasonal forest" = "#97B669",
  "Temperate rain forest" = "#75A95E",
  "Tropical rain forest" = "#317A22",
  "Tropical seasonal forest/savanna" = "#A09700",
  "Subtropical desert" = "#FCD57A",
  "Temperate grassland/desert" = "#DCBB50",
  "Woodland/shrubland" = "#D16E3F"
)

biome_draw_order <- c(
  "Tundra",
  "Boreal forest",
  "Temperate seasonal forest",
  "Temperate rain forest",
  "Tropical rain forest",
  "Tropical seasonal forest/savanna",
  "Subtropical desert",
  "Temperate grassland/desert",
  "Woodland/shrubland"
)

biome_polys <- read_csv(
  file.path(data_dir, "Whittaker_biomes_plotbiomes.csv"),
  show_col_types = FALSE
) %>%
  mutate(
    biome = factor(biome, levels = biome_draw_order)
  ) %>%
  arrange(biome, biome_id)

biome_legend <- tibble(
  biome = biome_draw_order,
  fill = unname(biome_palette[biome_draw_order]),
  y = seq(402, 258, by = -18)
)

fig2e <- ggplot() +
  geom_polygon(
    data = biome_polys,
    aes(x = temp_c, y = precp_cm, group = biome_id, fill = biome),
    color = alpha("white", 0.97),
    linewidth = 1,
    alpha = 0.95,
    linejoin = "round"
  ) +
  scale_fill_manual(values = biome_palette, guide = "none") +
  annotate(
    "rect",
    xmin = -14.2, xmax = 5.0, ymin = 252, ymax = 421,
    fill = alpha("white", 0.78),
    color = "grey82",
    linewidth = 0.3
  ) +
  geom_segment(
    data = biome_legend,
    aes(x = -13.2, xend = -12.2, y = y, yend = y),
    color = biome_legend$fill,
    linewidth = 1.8,
    lineend = "round",
    show.legend = FALSE
  ) +
  geom_text(
    data = biome_legend,
    aes(x = -11.4, y = y, label = biome),
    hjust = 0,
    vjust = 0.5,
    size = 3.1,
    color = "grey15",
    show.legend = FALSE
  ) +
  geom_point(
    data = biome_points,
    aes(x = mat, y = precip_cm, color = continent),
    size = 1.55,
    alpha = 0.72
  ) +
  scale_color_manual(
    values = continent_colors,
    breaks = names(continent_colors),
    name = "Continent",
    guide = guide_legend(override.aes = list(size = 3.2, alpha = 1))
  ) +
  coord_cartesian(xlim = c(-15, 31), ylim = c(0, 460), expand = FALSE) +
  labs(
    title = "(e) Whittaker Biome Diagram",
    x = expression("Temperature (" * degree * "C)"),
    y = "Precipitation (cm)"
  ) +
  theme_main +
  theme(
    legend.position = "bottom",
    legend.justification = "center",
    legend.box.background = element_rect(fill = alpha("white", 0.95), color = NA),
    legend.key.width = grid::unit(1.3, "lines"),
    legend.key.height = grid::unit(0.9, "lines"),
    panel.grid.major = element_line(color = "grey78", linewidth = 0.45),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = NA)
  )

# ---------------------------
# Panel f
# ---------------------------
trend_data <- all_bloom %>%
  select(location, continent, year, bloom_doy) %>%
  distinct()

label_df <- trend_data %>%
  group_by(continent) %>%
  group_modify(~ tibble(label = make_eq_label_parse(.x))) %>%
  ungroup() %>%
  mutate(
    continent = factor(continent, levels = c("Asia", "Europe", "North America")),
    x = 1953
  ) %>%
  arrange(continent) %>%
  mutate(y = c(192, 176, 160)) %>%
  ungroup()

fig2f <- ggplot(trend_data, aes(x = year, y = bloom_doy, color = continent)) +
  annotate(
    "rect",
    xmin = 1951, xmax = 1991, ymin = 150, ymax = 198,
    fill = alpha("white", 0.86),
    color = "grey82",
    linewidth = 0.3
  ) +
  geom_point(alpha = 0.12, size = 0.8, show.legend = FALSE) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1.45, show.legend = TRUE) +
  geom_text(
    data = label_df,
    aes(x = x, y = y, label = label, color = continent),
    hjust = 0,
    vjust = 1,
    size = 4,
    parse = TRUE,
    inherit.aes = FALSE
  ) +
  scale_color_manual(values = continent_colors) +
  scale_x_continuous(
    limits = c(1950, 2024),
    breaks = c(1950, 1970, 1990, 2010, 2020),
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  scale_y_continuous(breaks = seq(60, 200, 20)) +
  coord_cartesian(ylim = c(60, 200)) +
  labs(
    title = "(f) Blooming Day Trends Across Cities",
    x = "Year",
    y = "Blooming Day (DOY)",
    color = "Continent"
  ) +
  theme_main +
  theme(
    legend.position = "bottom",
    legend.justification = "center",
    legend.box.background = element_rect(fill = alpha("white", 0.95), color = NA),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.35),
    panel.grid.minor = element_blank()
  )

# ---------------------------
# Combine full Fig. 2
# ---------------------------
fig2_all <- (panel_a | panel_b | panel_c) / (panel_d | fig2e | fig2f) +
  plot_layout(widths = c(1, 1, 1.35), heights = c(1, 1.08))

fig2_ef <- fig2e | fig2f

ggsave(
  filename = file.path(root_dir, "Fig2_combined_a_to_f.png"),
  plot = fig2_all,
  width = 19,
  height = 12,
  dpi = 300
)

ggsave(
  filename = file.path(root_dir, "Fig2_combined_a_to_f.tiff"),
  plot = fig2_all,
  width = 19,
  height = 12,
  dpi = 300,
  compression = "lzw"
)

ggsave(
  filename = file.path(root_dir, "Fig2ef_biome_and_trends.png"),
  plot = fig2_ef,
  width = 16,
  height = 7.4,
  dpi = 300
)

ggsave(
  filename = file.path(root_dir, "Fig2ef_biome_and_trends.tiff"),
  plot = fig2_ef,
  width = 16,
  height = 7.4,
  dpi = 300,
  compression = "lzw"
)

write_csv(
  biome_points,
  file.path(root_dir, "Fig2e_biome_points_used.csv")
)

write_csv(
  trend_data,
  file.path(root_dir, "Fig2f_trend_data_used.csv")
)

print(fig2_all)
