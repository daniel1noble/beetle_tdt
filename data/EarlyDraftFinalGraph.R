# Load libraries (install if necessary)
library(readxl)
library(tidyverse)
library(janitor)
library(ggplot2)
library(patchwork)  # For combining plots
library(latex2exp)  # For LaTeX-style axis labels

# Read data
data <- read_excel("/Users/garrettball/Documents/GitHub/beetle_tdt/data/data_mockup.xlsx", 
                   sheet = "all_data")

# Clean column names
data <- clean_names(data)

# Convert '0H' and '48H' to binary mortality values
data <- data %>%
  mutate(
    initial_mortality = ifelse(x0h == "Y", 0, 1),
    final_mortality = ifelse(x48h == "Y", 0, 1),
    log_time = log(time)  # Log-transform time for modeling
  )

# Remove rows with missing values in key variables
data <- data %>%
  filter(!is.na(final_mortality), !is.na(temp), !is.na(log_time), !is.na(sex))

# Fit binomial GLMs separately for males and females
glm_male <- glm(final_mortality ~ temp + log_time, 
                data = filter(data, sex == "M"), 
                family = binomial(link = "logit"))

glm_female <- glm(final_mortality ~ temp + log_time, 
                  data = filter(data, sex == "F"), 
                  family = binomial(link = "logit"))

# Summary of models
summary(glm_male)
summary(glm_female)

# Create a grid of new data for predictions
prediction_grid <- expand.grid(
  temp = unique(data$temp),        # Use existing temperatures
  time = seq(min(data$time), max(data$time), length.out = 100)
) %>%
  mutate(log_time = log(time))  # Compute log(time)

# Get predictions for males
prediction_grid_male <- prediction_grid %>%
  mutate(predicted_mortality = predict(glm_male, newdata = ., type = "response"))

# Get predictions for females
prediction_grid_female <- prediction_grid %>%
  mutate(predicted_mortality = predict(glm_female, newdata = ., type = "response"))

# Plot for males
plot_male <- ggplot(prediction_grid_male, aes(x = log_time, y = predicted_mortality, color = factor(temp))) +
  geom_line(size = 1) +
  labs(
    title = "Mortality Rate (Males)",
    x = TeX("$\\log(Time \\ (minutes))$"),
    y = "Mortality Rate",
    color = "Temperature (°C)"
  ) +
  theme_minimal()

# Plot for females
plot_female <- ggplot(prediction_grid_female, aes(x = log_time, y = predicted_mortality, color = factor(temp))) +
  geom_line(size = 1) +
  labs(
    title = "Mortality Rate (Females)",
    x = TeX("$\\log(Time \\ (minutes))$"),
    y = "Mortality Rate",
    color = "Temperature (°C)"
  ) +
  theme_minimal()

# Combine plots side by side
final_plot <- plot_male + plot_female + plot_layout(guides = "collect")

# Print the final plot
print(final_plot)
