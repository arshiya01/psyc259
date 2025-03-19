library(tidyverse)
library(ggplot2)
library(lme4)
library(lmerTest)
library(car)

# file path
df <- read.csv('C:/Users/hp/Desktop/conv/UCRSNL/emp_convo/a_affectR/affect3d_data.csv')

# remove missing values
df <- na.omit(df)

# create participant ID
df$participant_id <- paste(df$transcript, df$speaker, sep = "_")

# scale variables
df$rationality <- scale(df$rationality)
df$social_impact <- scale(df$social_impact)
df$valence <- scale(df$valence)

# compute correlation matrix
correlation_matrix <- cor(df[,c('rationality', 'social_impact', 'valence')])
print(correlation_matrix)

# plot distributions
ggplot(df, aes(x = rationality)) +
  geom_histogram(aes(fill = condition), bins = 30, alpha = 0.5) +
  geom_density(alpha = 0.7) +
  facet_wrap(~condition) +
  labs(title = "Distribution of Rationality by Condition", x = "Rationality", y = "Density") +
  theme_minimal()

ggplot(df, aes(x = social_impact)) +
  geom_histogram(aes(fill = condition), bins = 30, alpha = 0.5) +
  geom_density(alpha = 0.7) +
  facet_wrap(~condition) +
  labs(title = "Distribution of Social Impact by Condition", x = "Social Impact", y = "Density") +
  theme_minimal()

ggplot(df, aes(x = valence)) +
  geom_histogram(aes(fill = condition), bins = 30, alpha = 0.5) +
  geom_density(alpha = 0.7) +
  facet_wrap(~condition) +
  labs(title = "Distribution of Valence by Condition", x = "Valence", y = "Density") +
  theme_minimal()

# fit multilevel models for each variable
df$empathy_condition <- ifelse(df$condition == 'empathy', 1, 0)

model_rationality <- lmer(rationality ~ empathy_condition + (1|participant_id), data = df)
summary(model_rationality)

model_social_impact <- lmer(social_impact ~ empathy_condition + (1|participant_id), data = df)
summary(model_social_impact)

model_valence <- lmer(valence ~ empathy_condition + (1|participant_id), data = df)
summary(model_valence)

# residual analysis for each model
residuals_rationality <- residuals(model_rationality)
fitted_values_rationality <- fitted(model_rationality)

ggplot(data.frame(residuals = residuals_rationality), aes(x = residuals)) +
  geom_histogram(aes(y = ..density..), bins = 30, alpha = 0.5, fill = 'blue') +
  geom_density(alpha = 0.7, color = 'black') +
  labs(title = "Histogram of Residuals (Rationality)", x = "Residuals", y = "Density") +
  theme_minimal()

qqPlot(residuals_rationality, main = "Q-Q Plot of Residuals (Rationality)")

ggplot(data.frame(residuals = residuals_rationality, fitted_values = fitted_values_rationality), aes(x = fitted_values, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  labs(title = "Residuals vs Fitted Values (Rationality)", x = "Fitted Values", y = "Residuals") +
  theme_minimal()

# fixed effect visualization for each variable
fixed_effect_rationality <- fixef(model_rationality)[2]
ci_rationality <- confint(model_rationality)
ci_low_rationality <- ci_rationality[2, 1]
ci_high_rationality <- ci_rationality[2, 2]

ggplot(data.frame(Condition = c('Empathy Condition'), 
                  Effect = c(fixed_effect_rationality), 
                  ymin = c(ci_low_rationality), 
                  ymax = c(ci_high_rationality)), 
       aes(x = Condition, y = Effect, ymin = ymin, ymax = ymax)) +
  geom_bar(stat = "identity", fill = 'lightblue', color = 'black') +
  geom_errorbar(width = 0.2, linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  labs(title = "Mixed-effects Model Rationality Results for Empathy Condition", y = "Fixed Effect") +
  theme_minimal()
