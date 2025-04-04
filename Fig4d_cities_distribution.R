# Load required libraries
library(ggplot2)
library(sf)
library(readr)

# Load world shapefile (ensure it’s in EPSG:4326 format)
continent_file <- "data/World_Continents.shp"
shape_data <- st_read(continent_file)

# Load city coordinates (North America)
cities_file <- "data/other_cities_location_map_NorthAmerica.csv"
cities_data <- read_csv(cities_file)

# Check coordinate columns
if (!all(c("lat", "long") %in% colnames(cities_data))) {
  stop("The CSV file must contain 'lat' and 'long' columns.")
}

# Ensure CRS is EPSG:4326
if (st_crs(shape_data)$epsg != 4326) {
  shape_data <- st_transform(shape_data, crs = 4326)
}

# Get world bounding box for plotting
world_bbox <- st_bbox(shape_data)

# Custom color palette
colors <- c("slateblue1", "grey", "orange", "lightgreen", "tomato", "powderblue", "mediumorchid1", "yellow")

# Plot the world map with North American cities
glo_fig <- ggplot() +
  geom_sf(data = shape_data, aes(fill = CONTINENT), color = "black") +
  scale_fill_manual(values = colors) +
  geom_point(data = cities_data, aes(x = long, y = lat), 
             shape = 21, fill = "red", color = "black", size = 4, alpha = 1, show.legend = FALSE) +
  scale_color_manual(values = c("Cities" = "red"), name = "North American Cities") +
  coord_sf(xlim = c(world_bbox["xmin"], world_bbox["xmax"]), 
           ylim = c(world_bbox["ymin"], world_bbox["ymax"]), expand = FALSE) +
  theme_void() +
  theme(
    legend.key.size = unit(1.5, "lines"),
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 18, face = "bold")
  ) +
  labs(fill = "Continent", title = "", x = "", y = "")

# Save TIFF
ggsave("world_map_with_cities_star.tiff", glo_fig, width = 10, height = 6, dpi = 300)





# Japan, South Korea, North Korea Map
library(ggplot2)
library(dplyr)
library(maps)
library(readr)

# Load base world map
world_map <- map_data("world")

# Filter Japan and Korea
asia_map_data <- world_map %>% filter(region %in% c("Japan", "South Korea", "North Korea"))

# Load city coordinates
cities_data <- read_csv("data/other_cities_location_map_japan.csv")

# Check coordinate columns
if (!all(c("lat", "long", "alt") %in% colnames(cities_data))) {
  stop("The CSV file must contain 'lat', 'long', and 'alt' columns.")
}

# Plot map
map_with_cities <- ggplot() +
  geom_polygon(data = asia_map_data, aes(x = long, y = lat, group = group),
               fill = "white", color = "black", size = 1.2) +
  geom_point(data = cities_data, aes(x = long, y = lat, color = alt),
             size = 3, alpha = 0.8) +
  scale_color_gradient(low = "blue", high = "red", name = "Elevation (m)") +
  coord_fixed(1.3) +
  theme_void() +
  theme(
    legend.title = element_text(size = 24, face = "bold"),
    legend.text = element_text(size = 22),
    plot.title = element_text(size = 26, face = "bold")
  )

ggsave("Japan_SouthKorea_MapWithCities.tiff", map_with_cities, width = 10, height = 7, dpi = 300)





# Filter Switzerland only
europe_map_data <- world_map %>% filter(region == "Switzerland")

# Load city coordinates
cities_data <- read_csv("data/other_cities_location_map_Europe.csv")

# Validate coordinates
if (!all(c("lat", "long", "alt") %in% colnames(cities_data))) {
  stop("The CSV file must contain 'lat', 'long', and 'alt' columns.")
}

# Plot map
map_with_cities <- ggplot() +
  geom_polygon(data = europe_map_data, aes(x = long, y = lat, group = group),
               fill = "white", color = "black", size = 1.2) +
  geom_point(data = cities_data, aes(x = long, y = lat, color = alt),
             size = 3, alpha = 0.8) +
  scale_color_gradient(low = "green", high = "brown", name = "Elevation (m)") +
  coord_fixed(1.3) +
  theme_void() +
  theme(
    legend.title = element_text(size = 24, face = "bold"),
    legend.text = element_text(size = 22),
    plot.title = element_text(size = 26, face = "bold")
  )

ggsave("Switzerland_Europe_MapWithCities.tiff", map_with_cities, width = 10, height = 7, dpi = 300)




# Filter USA and Canada, cropped to relevant lat/long range
na_map_data <- world_map %>%
  filter(region %in% c("USA", "Canada")) %>%
  filter(long > -140, long < -60, lat > 20, lat < 60)

# Load city coordinates
cities_data <- read_csv("data/other_cities_location_map_NorthAmerica.csv")

# Check lat/long/alt columns
if (!all(c("lat", "long", "alt") %in% colnames(cities_data))) {
  stop("The CSV file must contain 'lat', 'long', and 'alt' columns.")
}

# Plot
map_with_cities <- ggplot() +
  geom_polygon(data = na_map_data, aes(x = long, y = lat, group = group),
               fill = "white", color = "black", size = 1.2) +
  geom_point(data = cities_data, aes(x = long, y = lat, color = alt),
             size = 6, alpha = 1) +
  scale_color_gradient(low = "green", high = "red", name = "Elevation (m)") +
  coord_fixed(1.3) +
  theme_void() +
  theme(
    legend.title = element_text(size = 24, face = "bold"),
    legend.text = element_text(size = 22),
    plot.title = element_text(size = 26, face = "bold")
  )

ggsave("USA_Canada_MapWithCities.tiff", map_with_cities, width = 10, height = 7, dpi = 300)
