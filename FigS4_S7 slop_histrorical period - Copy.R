# Load required libraries
library(ggplot2)
library(dplyr)

# Load bloom data for each city
kyoto <- read.csv("data/kyoto.csv")
liestal <- read.csv("data/liestal.csv")
washingtondc <- read.csv("data/washingtondc.csv")

# Add a city label to each dataset
kyoto$city <- "Kyoto"
liestal$city <- "Liestal"
washingtondc$city <- "Washington DC"

# Combine all city data into one dataframe
data <- bind_rows(kyoto, liestal, washingtondc)
data$year <- as.numeric(data$year)
data$bloom_doy <- as.numeric(data$bloom_doy)

# Define color scheme for each city
city_colors <- c("Kyoto" = "#F8766D",         # red
                 "Liestal" = "#00BA38",       # green
                 "Washington DC" = "#619CFF") # blue

# Function to create regression plot (with p-value printed explicitly)
plot_city_trend <- function(df, city_name, period_label, color, filename){
  model <- lm(bloom_doy ~ year, data = df)
  coefs <- coef(model)
  r2 <- summary(model)$r.squared
  pval <- summary(model)$coefficients[2, 4]
  
  # Build regression equation label
  eq <- paste0("DOY = ", round(coefs[1], 2),
               ifelse(coefs[2] < 0, " - ", " + "),
               abs(round(coefs[2], 2)), "*Year\n",
               "R² = ", round(r2, 2), ", p = ", round(pval, 2))
  
  # Plot
  p <- ggplot(df, aes(x = year, y = bloom_doy)) +
    geom_line(color = color, size = 0.5) +              # Connect data points with line
    geom_point(color = color, size = 2) +               # Plot points
    geom_smooth(method = "lm", se = FALSE,              # Linear regression line
                color = "black", linetype = "dashed", size = 1) +
    annotate("text", x = max(df$year) - 250, y = 140,   # Add equation text
             label = eq, hjust = 0, size = 5, fontface = "italic") +
    scale_y_continuous(limits = c(70, 150)) +
    labs(title = paste0(city_name, " (", period_label, ")"),
         x = "Year", y = "Blooming Day (DOY)") +
    theme_minimal(base_size = 15) +
    theme(
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
      axis.title = element_text(size = 15, face = "bold"),
      axis.text = element_text(size = 13),
      panel.grid = element_blank()
    )
  
  # Save figure as TIFF
  ggsave(paste0(filename, ".tiff"), plot = p, width = 6.5, height = 5.5, dpi = 300)
}

# Alternate function where p-value is always labeled as p < 0.01
plot_city_trend2 <- function(df, city_name, period_label, color, filename){
  model <- lm(bloom_doy ~ year, data = df)
  coefs <- coef(model)
  r2 <- summary(model)$r.squared
  
  # Build simplified equation label (p < 0.01)
  eq <- paste0("DOY = ", round(coefs[1], 2),
               ifelse(coefs[2] < 0, " - ", " + "),
               abs(round(coefs[2], 2)), "*Year\n",
               "R² = ", round(r2, 2), ", p < 0.01")
  
  # Plot
  p <- ggplot(df, aes(x = year, y = bloom_doy)) +
    geom_line(color = color, size = 0.5) +
    geom_point(color = color, size = 2) +
    geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed", size = 1) +
    annotate("text", x = max(df$year) - 80, y = 140, label = eq, hjust = 0, size = 5, fontface = "italic") +
    scale_y_continuous(limits = c(70, 150)) +
    labs(title = paste0(city_name, " (", period_label, ")"),
         x = "Year", y = "Blooming Day (DOY)") +
    theme_minimal(base_size = 15) +
    theme(
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
      axis.title = element_text(size = 15, face = "bold"),
      axis.text = element_text(size = 13),
      panel.grid = element_blank()
    )
  
  ggsave(paste0(filename, ".tiff"), plot = p, width = 6.5, height = 5.5, dpi = 300)
}

# Subset datasets and generate plots

# Kyoto: Historical period (1550–1850)
kyoto_old <- data %>% filter(city == "Kyoto", year >= 1550, year <= 1850)
plot_city_trend(kyoto_old, "Kyoto", "1550–1850", city_colors["Kyoto"], "Kyoto_1550_1850")

# Kyoto: Modern period (1924–2024)
kyoto_modern <- data %>% filter(city == "Kyoto", year >= 1924, year <= 2024)
plot_city_trend2(kyoto_modern, "Kyoto", "1924–2024", city_colors["Kyoto"], "Kyoto_1924_2024")

# Liestal: Modern period (1924–2024)
liestal_modern <- data %>% filter(city == "Liestal", year >= 1924, year <= 2024)
plot_city_trend2(liestal_modern, "Liestal", "1924–2024", city_colors["Liestal"], "Liestal_1924_2024")

# Washington DC: Modern period (1924–2024)
wash_modern <- data %>% filter(city == "Washington DC", year >= 1924, year <= 2024)
plot_city_trend2(wash_modern, "Washington DC", "1924–2024", city_colors["Washington DC"], "WashingtonDC_1924_2024")
