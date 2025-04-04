# Part 1: Simulated Histogram Comparison (Figure 1a–c)
# Load required libraries
library(ggplot2)
library(dplyr)
library(cowplot)

# Set seed for reproducibility
set.seed(123)

# Generate sample size
n <- 1000

# (a) Normal distribution centered at 200
data1 <- rnorm(n, mean = 200, sd = 100)

# (b) Uniform distribution between 50 and 350, with jitter
data2 <- runif(n, min = 50, max = 350) + rnorm(n, mean = 0, sd = 10)
data2 <- pmin(pmax(data2, 0), 400)  # Clip values to stay within 0–400

# (c) Skewed distribution: normal peak near 120 + right-tail gamma
data3 <- rnorm(n, mean = 120, sd = 50)
data3 <- data3[data3 >= 50 & data3 <= 300]
right_tail <- rgamma(n - length(data3), shape = 3, scale = 40) + 120
data3 <- c(data3, right_tail)

# Combine all distributions
df <- data.frame(
  GDD = c(data1, data2, data3),
  Group = rep(c("a. Normal (200 peak)", "b. Uniform (50–350)", "c. Skewed (120 peak)"),
              times = c(length(data1), length(data2), length(data3)))
)

# Define custom colors
colors <- c("a. Normal (200 peak)" = "lightblue",
            "b. Uniform (50–350)" = "lightgreen",
            "c. Skewed (120 peak)" = "gold")

# Plot histogram and density for each group
phist <- ggplot(df, aes(x = GDD, fill = Group, color = Group)) +
  geom_histogram(aes(y = ..density..), bins = 20, alpha = 0.6, position = "identity") +
  geom_density(color = "black", size = 1.5, alpha = 0.5) +
  scale_fill_manual(values = colors) +
  scale_color_manual(values = colors) +
  scale_x_continuous(limits = c(20, 400)) +
  facet_wrap(~ Group, scales = "free_y") +
  labs(x = "GDD", y = "Density") +
  theme_minimal(base_size = 14) +
  theme(
    strip.text = element_text(face = "bold", size = 16),
    axis.text = element_text(face = "bold", size = 14),
    axis.title = element_text(face = "bold", size = 16),
    legend.position = "none"
  )

# Display and save plot
print(phist)
ggsave("1-simulated_hist.tiff", plot = phist, width = 13, height = 4, dpi = 300, compression = "lzw")



# Part 2: Real Data Histograms (Figure 1d–f)
# Load required libraries
library(ggplot2)
library(dplyr)
library(patchwork)  # For combining subplots

# (d) Daylight Duration Data
df1 <- read.csv("data/Kyoto_Bloom_DOY_with_Photoperiod.csv")
p1 <- ggplot(df1, aes(x = daylight_duration)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue", color = "black", alpha = 0.6) +
  geom_density(color = "black", size = 1.5, alpha = 0.5) +
  labs(title = "(d) Daylight Duration", x = "Daylight Duration (Hours)", y = "Density") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.text = element_text(face = "bold", size = 12),
    axis.title = element_text(face = "bold", size = 14),
    legend.position = "none"
  )
p1
# (e) Leaf N Concentration
df2 <- read.csv("data/simulated_leaf_N_concentration.csv")
p2 <- ggplot(df2, aes(x = N_concentration)) +
  geom_histogram(aes(y = ..density..), bins = 50, fill = "lightblue", color = "black", alpha = 0.6) +
  geom_density(color = "black", size = 1.5, alpha = 0.5) +
  labs(title = "(e) Leaf N Concentration", x = "N (mg/g)", y = "Density") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.text = element_text(face = "bold", size = 12),
    axis.title = element_text(face = "bold", size = 14),
    legend.position = "none"
  )

# (f) Red Channel Intensity
df3 <- read.csv("data/simulated_red_channel_data.csv")
p3 <- ggplot(df3, aes(x = intensity_level)) +
  geom_histogram(aes(y = ..density..), bins = 50, fill = "lightblue", color = "black", alpha = 0.6) +
  geom_density(color = "black", size = 1.5, alpha = 0.5) +
  labs(title = "(f) Red Channel Intensity", x = "Intensity Level", y = "Density") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.text = element_text(face = "bold", size = 12),
    axis.title = element_text(face = "bold", size = 14),
    legend.position = "none"
  )

# Combine and export figure
final_plot <- p1 + p2 + p3 + plot_layout(ncol = 3)
print(final_plot)

ggsave("1-real_example_hist.tiff", plot = final_plot, width = 13, height = 4, dpi = 300, compression = "lzw")
