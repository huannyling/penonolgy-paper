# Load required libraries
library(ggplot2)
library(dplyr)
library(boot)
library(rstanarm)
library(gridExtra)
library(lubridate)
library(readr)

# --- STEP 1: Read Phenology and Weather Data ---

# Bloom date data (Day of Year)
kyoto_bloom <- read_csv("data/kyoto.csv")
liestal_bloom <- read_csv("data/liestal.csv")
washington_bloom <- read_csv("data/washingtondc.csv")

# Corresponding daily weather data
kyoto_weather <- read_csv("data/kyoto_weather_final.csv")
liestal_weather <- read_csv("data/liestal_weather_final.csv")
washington_weather <- read_csv("data/washington_weather_final.csv")


# --- STEP 2: Function to Compute GDD5 from Feb 1 to Peak Bloom and 3 Days Before ---
# I used this data to run a Bayesian model to predict the threshold GDD5 

compute_gdd5 <- function(bloom_data, weather_data, city_name) {
  # Filter bloom data to match the available weather record range
  bloom_data <- bloom_data %>%
    filter(year >= min(year(weather_data$date)),
           year <= max(year(weather_data$date))) %>%
    select(year, bloom_doy)
  
  final_data <- data.frame()
  
  for (i in 1:nrow(bloom_data)) {
    year <- bloom_data$year[i]
    bloom_doy <- bloom_data$bloom_doy[i]
    
    # Define relevant dates
    bloom_date <- as.Date(paste0(year, "-01-01")) + days(bloom_doy - 1)
    start_date <- as.Date(paste0(year, "-02-01"))
    pre_bloom_date <- bloom_date - days(3)
    
    # Calculate accumulated GDD5 until bloom and pre-bloom dates
    gdd5_bloom <- weather_data %>%
      filter(date >= start_date & date <= bloom_date) %>%
      summarise(GDD5 = sum(GDD5, na.rm = TRUE)) %>%
      pull(GDD5)
    
    gdd5_pre_bloom <- weather_data %>%
      filter(date >= start_date & date <= pre_bloom_date) %>%
      summarise(GDD5 = sum(GDD5, na.rm = TRUE)) %>%
      pull(GDD5)
    
    # Store results (y = 1: bloom date; y = 0: 3 days before)
    final_data <- rbind(final_data, data.frame(date = bloom_date, GDD5 = gdd5_bloom, y = 1))
    final_data <- rbind(final_data, data.frame(date = pre_bloom_date, GDD5 = gdd5_pre_bloom, y = 0))
  }
  
  # Save results to CSV
  write_csv(final_data, paste0("data/", city_name, "_gdd5_from_Feb.csv"))
}

# Compute and save GDD5 for each city
compute_gdd5(liestal_bloom, liestal_weather, "liestal")
compute_gdd5(kyoto_bloom, kyoto_weather, "kyoto")
compute_gdd5(washington_bloom, washington_weather, "washington")


# --- STEP 3: Load Computed GDD5 and Prepare Bloom-only Data ---

kyoto_gdd5 <- read.csv("data/kyoto_gdd5_from_Feb.csv")
liestal_gdd5 <- read.csv("data/liestal_gdd5_from_Feb.csv")
washington_gdd5 <- read.csv("data/washington_gdd5_from_Feb.csv")

# Filter only rows corresponding to bloom day (y == 1)
kyoto_bloom <- kyoto_gdd5 %>% filter(y == 1) %>% select(GDD5)
liestal_bloom <- liestal_gdd5 %>% filter(y == 1) %>% select(GDD5)
washington_bloom <- washington_gdd5 %>% filter(y == 1) %>% select(GDD5)


# --- STEP 4: Histogram and Density Plot Function ---

plot_hist <- function(data, title, fill_color) {
  ggplot(data.frame(GDD5 = data), aes(x = GDD5)) +
    geom_histogram(aes(y = ..density..), bins = 30, fill = fill_color, color = "black", alpha = 0.7) +
    geom_density(color = "black", size = 1.5) +  # Overlay density line
    ggtitle(title) +
    theme_minimal(base_size = 16) +
    theme(
      axis.text = element_text(face = "bold", size = 14),
      axis.title = element_text(face = "bold", size = 16),
      plot.title = element_text(face = "bold", size = 18)
    )
}


# --- STEP 5: Generate GDD5 Distribution Plots for Each City ---

colors <- c("lightblue", "lightgreen", "gold")

p1 <- plot_hist(kyoto_bloom$GDD5, "(a) Kyoto", colors[1])
p2 <- plot_hist(liestal_bloom$GDD5, "(b) Liestal", colors[2])
p3 <- plot_hist(washington_bloom$GDD5, "(c) Washington DC", colors[3])


# --- STEP 6: Combined GDD5 Distribution (Pooled Analysis) ---

# Merge all cities and assign labels
all_gdd5 <- bind_rows(
  kyoto_bloom %>% mutate(city = "Kyoto"),
  liestal_bloom %>% mutate(city = "Liestal"),
  washington_bloom %>% mutate(city = "Washington DC")
)

# Trim extreme values: remove the 5 lowest and 5 highest years
sorted_gdd5 <- all_gdd5 %>% arrange(GDD5)
filtered_gdd5 <- sorted_gdd5[6:(nrow(sorted_gdd5) - 5), ]

# Plot pooled GDD5 distribution
p4 <- plot_hist(filtered_gdd5$GDD5, "(d) Pooled GDD5 Distribution (Trimmed)", "gray")

# Display or save plots
p4 



# --- STEP 7: Kyoto doy analysis ---
kyoto <- read.csv("data/kyoto.csv")
kyoto$city <- "Kyoto"

# plot doy distribution
p5 <- ggplot(kyoto, aes(x = bloom_doy, fill = city)) +
  geom_histogram(binwidth = 1, alpha = 0.6, color = "black") +  
  geom_vline(aes(xintercept = stats_kyoto$mean_doy), linetype = "dashed", size = 1, color = "blue") +  
  geom_vline(aes(xintercept = stats_kyoto$median_doy), linetype = "solid", size = 1, color = "red") + 
  labs(
    title = "(e) Cherry Blossom Bloom Dates in Kyoto",
    x = "Day of Year (DOY)",
    y = "Number of Years",
    fill = "City"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    axis.title.x = element_text(size = 16, face= "bold"),
    axis.title.y = element_text(size = 16, face= "bold"),
    axis.text.x = element_text(size = 14, face= "bold"),
    axis.text.y = element_text(size = 14, face= "bold"),
    legend.position = "none"
  ) +
  # add mean and median lines
  geom_text(
    aes(x = stats_kyoto$mean_doy+5, y = 60, label = paste("Mean =", round(stats_kyoto$mean_doy, 1))),
    vjust = 2, hjust = -0.1, color = "blue", size = 6
  ) +
  geom_text(
    aes(x = stats_kyoto$median_doy+5, y = 60, label = paste("Median =", round(stats_kyoto$median_doy, 1))),
    vjust = 4, hjust = -0.1, color = "red", size = 6
  ) 

p5


# --- STEP 7: Three cities doy analysis ---
# Cherry DOY Fig 1(f)
kyoto <- read.csv("data/kyoto.csv")
liestal <- read.csv("data/liestal.csv")
washingtondc <- read.csv("data/washingtondc.csv")

kyoto$city <- "Kyoto"
liestal$city <- "Liestal"
washingtondc$city <- "Washington DC"

data <- bind_rows(kyoto, liestal, washingtondc)

p <- ggplot(data, aes(x = year, y = bloom_doy, color = city)) +
  geom_line(size = 0.5) +  
  geom_point(size = 1.5) +  
  labs(title = "(f) Cherry Blossom Bloom Dates by City",
       x = "Year",
       y = "Day of Year (DOY)",
       color = "City") +
  theme_minimal() +  
  theme(
    plot.title = element_text(hjust = 0, size = 18, face = "bold"), 
    axis.title.x = element_text(size = 16, face = "bold"),  
    axis.title.y = element_text(size = 16, face = "bold"), 
    axis.text.x = element_text(size = 14, face = "bold"),   
    axis.text.y = element_text(size = 14, face = "bold"),   
    legend.title = element_text(size = 16, face = "bold"), 
    legend.text = element_text(size = 14, face = "bold"),   
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),  
    panel.background = element_blank(),  
    axis.line = element_line(color = "black",size=1.2)  
  )
p


# conbime 6 plots
final_plot <- grid.arrange(
  arrangeGrob(p1, p2, p3, ncol = 3),  
  arrangeGrob(p4, p5, ncol = 2), 
  arrangeGrob(p6, ncol = 1), 
  heights = c(1, 1.2,1.5)  
)

# save tiff
ggsave("Fig2.tiff", plot = final_plot, width = 15, height = 10, dpi = 300, compression = "lzw")