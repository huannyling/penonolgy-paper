# ===================================================
# Load required libraries
# ===================================================
library(lme4)      # For linear mixed-effects models
library(dplyr)     # For data manipulation
library(MuMIn)     # For calculating R² for mixed models

# ===================================================
# Read and prepare data
# ===================================================
data <- read.csv("data/3cities_doy_weather_1940_2024.csv")

# Ensure all columns are numeric
data <- data %>% mutate(across(everything(), as.numeric))

# ===================================================
# Define response and predictor variables
# ===================================================
y <- "bloom_doy"  # Response variable: day of year (DOY) of bloom
X <- c("TMAX", "TMIN", "TAVG", "TDIFF", "PRCP", "WIND", "EVPR", "SUND", "WATR", 
       "SUND5_cum", "GDD5_cum", "CDD7_cum", "CODE3", "CODE51", "CODE71", "CODE73", 
       "CODE61", "CODE53", "CODE63", "CODE55", "CODE75", "CODE2", "CODE0", 
       "CODE1", "CODE65")  # Candidate predictor variables

# ===================================================
# Fit null model (intercept only + random effect of City)
# ===================================================
null_model <- lmer(as.formula(paste(y, "~ 1 + (1|City)")), data = data, REML = FALSE)
null_R2 <- r.squaredGLMM(null_model)[1]
null_AIC <- AIC(null_model)

# ===================================================
# Evaluate individual contribution of each predictor
# ===================================================
variable_effects <- data.frame(Variable = X, R2_Change = NA, AIC_Change = NA)

for (i in seq_along(X)) {
  formula <- paste(y, "~", X[i], "+ (1|City)")
  model <- lmer(formula, data = data, REML = FALSE)
  model_R2 <- r.squaredGLMM(model)[1]
  model_AIC <- AIC(model)
  
  variable_effects$R2_Change[i] <- model_R2 - null_R2
  variable_effects$AIC_Change[i] <- null_AIC - model_AIC
}

# ===================================================
# Sort predictors by descending R² contribution
# ===================================================
sorted_vars <- variable_effects %>% 
  arrange(desc(R2_Change)) %>% 
  pull(Variable)

# ===================================================
# Forward stepwise model building
# Accumulate R² and AIC improvement at each step
# ===================================================
current_formula <- paste(y, "~ 1")
current_model <- null_model
current_R2 <- null_R2
current_AIC <- null_AIC

result <- data.frame(Variable = character(),
                     R2_Cumulative = numeric(),
                     AIC_Change = numeric())

for (var in sorted_vars) {
  new_formula <- paste(current_formula, "+", var)
  full_formula <- paste(new_formula, "+ (1|City)")
  
  new_model <- lmer(full_formula, data = data, REML = FALSE)
  new_R2 <- r.squaredGLMM(new_model)[1]
  new_AIC <- AIC(new_model)
  
  result <- rbind(result, data.frame(
    Variable = var,
    R2_Cumulative = new_R2,
    AIC_Change = current_AIC - new_AIC
  ))
  
  current_formula <- new_formula
  current_model <- new_model
  current_R2 <- new_R2
  current_AIC <- new_AIC
}

# ===================================================
# Format and save results (Table S2)
# ===================================================
result <- result %>% 
  mutate(R2_Cumulative = round(R2_Cumulative, 4),
         AIC_Change = round(AIC_Change, 2)) %>%
  arrange(desc(-R2_Cumulative))

print(result)
write.csv(result, "Table S2 variable_contributions.csv", row.names = FALSE)
