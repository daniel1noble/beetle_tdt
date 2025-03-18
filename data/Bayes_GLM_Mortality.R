# Install packages (if necessary)
install.packages("patchwork")  
install.packages("readxl")    
install.packages("ggplot2")
install.packages("tidyverse")
install.packages("brms")

# load libraries
library(readxl)
library(tidyverse)
library(janitor)
library(ggplot2)
library(patchwork)
library(brms)

# Read data
data <- read_excel("/Users/garrettball/Documents/GitHub/beetle_tdt/data/data_mockup.xlsx", 
                   sheet = "all_data")

# Rename (clean) column titles
data <- clean_names(data)

# convert mortality to binary values (0 = dead, 1 = alive) --> can reverse desires 'Mortality' or "Survival Rate"
data <- data %>%
  mutate(
    initial_mortality = ifelse(x0h == "Y", 0, 1),
    final_mortality = ifelse(x48h == "Y", 0, 1),
    logtime = log(time)
  )

# Fit BAYESIAN generalized linear models (GLMs)
glm_bayes_male <- brm(final_mortality ~ logtime + temp + logtime:temp, 
                      iter = 5000, warmup = 1000, chains = 4,
                      data = filter(data, sex == "M"), family = bernoulli)

glm_bayes_female <- brm(final_mortality ~ logtime + temp + logtime:temp, 
                        iter = 5000, warmup = 1000, chains = 4,
                        data = filter(data, sex == "F"), family = bernoulli)

# Extract posterior draws
post_draws_male <- posterior_samples(glm_bayes_male)
post_draws_female <- posterior_samples(glm_bayes_female)

# time and temperature range for predictions --> may need to expand Sequence/length.out to get all info
time2 <- seq(10, 10000, length.out = 2000)  
newdata <- expand.grid(logtime = log(time2), temp = c(38, 41, 44, 47, 50))

# predict mortality using Bayesian posterior samples
posterior_predict_mortality <- function(model, newdata) {
  posterior <- posterior_epred(model, newdata = newdata)
  apply(posterior, 2, mean)  # Get mean posterior prediction
}

newdata$mortality_male <- posterior_predict_mortality(glm_bayes_male, newdata)
newdata$mortality_female <- posterior_predict_mortality(glm_bayes_female, newdata)
newdata$mortality_combined <- (newdata$mortality_male + newdata$mortality_female) / 2

# Function to find the time at which mortality reaches a given threshold
find_threshold_time <- function(data, mortality_col, threshold) {
  data %>%
    group_by(temp) %>%
    filter(.data[[mortality_col]] >= threshold) %>%
    slice(1) %>%
    select(temp, logtime) %>%
    mutate(time = exp(logtime), mortality_threshold = threshold)
}

# create tables for 0.5 and 0.75 mortality thresholds
threshold_05_male <- find_threshold_time(newdata, "mortality_male", 0.5)
threshold_075_male <- find_threshold_time(newdata, "mortality_male", 0.75)

threshold_05_female <- find_threshold_time(newdata, "mortality_female", 0.5)
threshold_075_female <- find_threshold_time(newdata, "mortality_female", 0.75)

threshold_05_combined <- find_threshold_time(newdata, "mortality_combined", 0.5)
threshold_075_combined <- find_threshold_time(newdata, "mortality_combined", 0.75)


# combine and print tables
threshold_table_male <- bind_rows(threshold_05_male, threshold_075_male)
threshold_table_female <- bind_rows(threshold_05_female, threshold_075_female)
threshold_table_combined <- bind_rows(threshold_05_combined, threshold_075_combined)

print(threshold_table_male)
print(threshold_table_female)
print(threshold_table_combined)

# Plot mortality for males
plot_male <- ggplot(newdata, aes(x = logtime, y = mortality_male, color = as.factor(temp))) +
  geom_line() +
  labs(title = "Bayesian Predicted Mortality for Males", 
       y = "Mortality", 
       x = "Log(Time)",
       color = "Temperature") +
  theme_minimal()

# Plot mortality for females
plot_female <- ggplot(newdata, aes(x = logtime, y = mortality_female, color = as.factor(temp))) +
  geom_line() +
  labs(title = "Bayesian Predicted Mortality for Females", 
       y = "Mortality", 
       x = "Log(Time)",
       color = "Temperature") +
  theme_minimal()

#plot combined mortality
plot_combined <- ggplot(newdata, aes(x = logtime, y = mortality_combined, color = as.factor(temp))) +
  geom_line() +
  labs(title = "Bayesian Predicted Combined Mortality", 
       y = "Mortality", 
       x = "Log(Time)",
       color = "Temperature") +
  theme_minimal()

# Combine all three plots (side by side in patchwork)
plot_male + plot_female + plot_combined