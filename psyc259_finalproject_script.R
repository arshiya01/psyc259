# Load required packages
library(tidyverse)
library(ggplot2)
library(lme4)
library(lmerTest)
library(car)

# Read and clean data
df <- read.csv('data/psyc259_finalproject_data.csv')

# Remove rows with missing values
original_length <- nrow(df)
df <- na.omit(df)
cat("rows dropped: ", original_length - nrow(df), "\n")

# Create a participant ID by combining transcript and speaker
df$participant_id <- paste(df$transcript, df$speaker, sep = "_")

# Scale predictors of interest
df$rationality_scaled <- scale(df$rationality)
df$social_impact_scaled <- scale(df$social_impact)
df$valence_scaled <- scale(df$valence)

# Compute correlation among predictors of interest
correlation_matrix <- cor(df[,c('rationality_scaled', 'social_impact_scaled', 'valence_scaled')])
print(correlation_matrix)

# Plot distributions by condition
for (col in c("rationality_scaled", "social_impact_scaled", "valence_scaled")) {
  p <- ggplot(df, aes_string(x = col, fill = "condition")) +
    geom_histogram(bins = 30, alpha = 0.5, position = "identity") +
    geom_density(aes(y = ..density..), alpha = 0.7) +
    facet_wrap(~ condition) +
    labs(
      title = paste("distribution of", col, "by condition"),
      x = col,
      y = "density"
    ) +
    theme_minimal()
  
  print(p)
}

# Function to fit multilevel models
# 1) Encodes the empathy condition as a binary variable
# 2) Fits a random-intercept model by participant_id
# 3) Prints the model summary
fit_multilevel_model <- function(df, dependent_var) {
  
  # Convert participant_id to factor
  df <- df %>%
    mutate(
      participant_id     = factor(participant_id),
      empathy_condition  = ifelse(condition == "empathy", 1, 0)
    )
  model_formula <- as.formula(paste(dependent_var, "~ empathy_condition + (1|participant_id)"))
  model <- lmer(model_formula, data = df)
  
  cat("Model for:", dependent_var, "\n")
  print(summary(model))
  
  return(model)
}

# Function for residual diagnostics
# 1) Creates histograms of residuals
# 2) Creates Q-Q plots
# 3) Plots residuals vs. fitted values
analyze_residuals <- function(df, dependent_var, model) {
  
  # Extract residuals and fitted values
  residuals_df <- data.frame(
    residuals    = residuals(model),
    fitted_values = fitted(model)
  )
  
  # Histogram of residuals
  p1 <- ggplot(residuals_df, aes(x = residuals)) +
    geom_histogram(aes(y = ..density..), bins = 30, alpha = 0.5) +
    geom_density(alpha = 0.7) +
    labs(
      title = paste("Histogram of Residuals (", dependent_var, ")"),
      x = "Residuals",
      y = "Density"
    ) +
    theme_minimal()
  print(p1)
  
  # Q-Q Plot of residuals
  qqPlot(residuals(model), main = paste("Q-Q Plot of Residuals (", dependent_var, ")"))
  
  # Residuals vs Fitted Values
  p2 <- ggplot(residuals_df, aes(x = fitted_values, y = residuals)) +
    geom_point() +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
    labs(
      title = paste("Residuals vs Fitted Values (", dependent_var, ")"),
      x = "Fitted Values",
      y = "Residuals"
    ) +
    theme_minimal()
  print(p2)
}

# Main loop: Fit each model and visualize diagnostics
for (dependent_var in c("rationality", "social_impact", "valence")) {
  
  # Fit model
  result <- fit_multilevel_model(df, dependent_var)
  
  # Residual diagnostics
  analyze_residuals(df, dependent_var, result)
}