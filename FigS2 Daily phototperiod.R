# Load necessary package
library(lubridate)

# Define latitude in radians (Kyoto: 35.0120°N)
lat_deg <- 35.0120
lat_rad <- lat_deg * pi / 180

# Define days of year (DOY) from Feb 1 to Mar 31
dates <- seq(ymd("2024-02-01"), ymd("2024-03-31"), by = "1 day")
DOY <- yday(dates)

# Calculate solar declination (δ) in radians for each DOY
# Declination formula: δ = 23.44° * sin(360/365 * (DOY - 81))
delta_deg <- 23.44 * sin(2 * pi * (DOY - 81) / 365)
delta_rad <- delta_deg * pi / 180

# Calculate photoperiod (hours)
photoperiod_hr <- (2 / 15) * acos(-tan(lat_rad) * tan(delta_rad))

# Convert to minutes
photoperiod_min <- photoperiod_hr * 60

# Combine results into a dataframe
photoperiod_data <- data.frame(
  Date = dates,
  DOY = DOY,
  Photoperiod_Minutes = round(photoperiod_min, 1)
)

# View a sample
head(photoperiod_data)

model <- lm(Photoperiod_Minutes ~ Date, data = photoperiod_data)
summary(model)
coefs <- coef(model)
r2 <- summary(model)$r.squared

# Build simplified equation label (p < 0.01)
eq <- paste0("Photoperiod_Duration = ", round(coefs[1], 2),
             ifelse(coefs[2] < 0, " - ", " + "),
             abs(round(coefs[2], 2)), "*Date\n",
             "R² = ", round(r2, 2), ", p < 0.01")

# Optional: plot
library(ggplot2)
p<-ggplot(photoperiod_data, aes(x = Date, y = Photoperiod_Minutes)) +
  geom_point(color = "#2C3E50", size = 2, alpha = 0.7) +  # 散点
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed", size = 1) +
  annotate("text", x = min(photoperiod_data$Date), y = 12.6, label = eq, hjust = 0, size = 5, fontface = "italic") +
  labs(title = "Daily Photoperiod duration in Kyoto",
       x = "Date", y = "Photoperiod (hour)") +
  theme_minimal(base_size = 14)+
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold", size = 18),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14)
  )
p

ggsave("FigS2_photoperiod_duration.tiff", plot = p, dpi = 300, width = 6, height = 5, units = "in", compression = "lzw")




# Load required libraries
library(lubridate)
library(dplyr)
library(readr)

# Read cherry blossom data
data <- read_csv("data/Kyoto.csv")

# Ensure 'bloom_doy' and 'Year' columns exist
if (!all(c("year", "bloom_doy") %in% colnames(data))) {
  stop("The input data must contain 'Year' and 'bloom_doy' columns.")
}

# Latitude of Kyoto in radians
lat_deg <- 35.0120
lat_rad <- lat_deg * pi / 180

# Compute solar declination and photoperiod for each bloom DOY
photoperiod_bloom <- data %>%
  filter(!is.na(bloom_doy)) %>%
  mutate(
    # Calculate solar declination in degrees
    delta_deg = 23.44 * sin(2 * pi * (bloom_doy - 81) / 365),
    delta_rad = delta_deg * pi / 180,
    
    # Calculate photoperiod duration in hours
    photoperiod_days = (2 / 15) * acos(-tan(lat_rad) * tan(delta_rad)),
    
    # Convert to minutes
    daylight_duration = round(photoperiod_days * 60, 2)
  )

# View first few rows
head(photoperiod_bloom)

# Optional: Save results to CSV
write_csv(photoperiod_bloom, "data/Kyoto_Bloom_DOY_with_Photoperiod.csv")
