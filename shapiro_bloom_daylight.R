# Shapiro-Wilk normality test for bloom_doy and daylight_duration
# Input: Kyoto_Bloom_DOY_with_Photoperiod.csv

library(readr)

df <- read_csv("Kyoto_Bloom_DOY_with_Photoperiod.csv")

cols <- c("bloom_doy", "daylight_duration")

for (col in cols) {
  x <- df[[col]]
  x <- x[is.finite(x)]

  rng <- range(x, na.rm = TRUE)
  sw  <- shapiro.test(x)

  cat("\n", col, "\n", sep = "")
  cat("  N: ", length(x), "\n", sep = "")
  cat("  Range: ", rng[1], " to ", rng[2], "\n", sep = "")
  cat("  Shapiro-Wilk W: ", sw$statistic, "\n", sep = "")
  cat("  p-value: ", sw$p.value, "\n", sep = "")
}
