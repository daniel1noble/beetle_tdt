# Load libraries
install.packages("patchwork")  # Install patchwork if not installed
library(readxl)
library(tidyverse)
library(janitor)
library(ggplot2)
library(patchwork)

# Read data
data <- read_excel("/Users/garrettball/Documents/GitHub/beetle_tdt/data/data_mockup.xlsx", 
                   sheet = "all_data")

# Clean column names
data <- clean_names(data)

# Convert mortality columns to binary values
data <- data %>%
  mutate(
    initial_mortality = ifelse(x0h == "Y", 0, 1),
    final_mortality = ifelse(x48h == "Y", 0, 1),
    logtime = log(time)
  )

# Fit binomial GLMs
glm_male <- glm(final_mortality ~ logtime + temp + logtime:temp, 
                data = filter(data, sex == "M"), 
                family = binomial(link = "logit"))

glm_female <- glm(final_mortality ~ logtime + temp + logtime:temp, 
                  data = filter(data, sex == "F"), 
                  family = binomial(link = "logit"))

# Generate new data for prediction
time2 <- 10:500
newdata <- data.frame(logtime = log(time2),
                      temp = rep(c(38, 41, 44, 47, 50), each = length(time2)))

# Predict mortality for males
newdata$mortality_male <- predict(glm_male, newdata = newdata, type = "response")

# Predict mortality for females
newdata$mortality_female <- predict(glm_female, newdata = newdata, type = "response")

# Plot mortality for males
plot_male <- ggplot(newdata, aes(x = logtime, y = mortality_male)) +
  geom_smooth(color = "blue") +
  labs(title = "Predicted Mortality for Males", 
       y = "Mortality", 
       x = "Log(Time)") +
  theme_minimal()

# Plot mortality for females
plot_female <- ggplot(newdata, aes(x = logtime, y = mortality_female)) +
  geom_smooth(color = "red") +
  labs(title = "Predicted Mortality for Females", 
       y = "Mortality", 
       x = "Log(Time)") +
  theme_minimal()

# Combine plots using patchwork
plot_male + plot_female

view(newdata)
