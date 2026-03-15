# Install packages (if necessary)
install.packages("patchwork")  
install.packages("readxl")    
install.packages("ggplot2")
install.packages("tidyverse")

#load libraries
library(readxl)
library(tidyverse)
library(janitor)
library(ggplot2)
library(patchwork)

# read data
data <- read_excel("/Users/garrettball/Documents/GitHub/beetle_tdt/data/data_mockup.xlsx", 
                   sheet = "all_data")

#rename (clean) column titles
data <- clean_names(data)

#turn mortality into binary values (0 = dead, 1 = alive)
data <- data %>%
  mutate(
    initial_mortality = ifelse(x0h == "Y", 0, 1),
    final_mortality = ifelse(x48h == "Y", 0, 1),
    logtime = log(time)
  )

# fit binomial generalized linear model (glm)
glm_male <- glm(final_mortality ~ logtime + temp + logtime:temp, 
                data = filter(data, sex == "M"), 
                family = binomial(link = "logit"))

glm_female <- glm(final_mortality ~ logtime + temp + logtime:temp, 
                  data = filter(data, sex == "F"), 
                  family = binomial(link = "logit"))

# Extend time range further to ensure all temperatures reach 0.50 mortality
    # maybe too many intervals? 

time2 <- seq(10, 10000, length.out = 2000)  
newdata <- expand.grid(logtime = log(time2), temp = c(38, 41, 44, 47, 50))

# predict mortality (males)
newdata$mortality_male <- predict(glm_male, newdata = newdata, type = "response")

# predict mortality for (female)
newdata$mortality_female <- predict(glm_female, newdata = newdata, type = "response")

# Calculate combined mortality (total)
newdata$mortality_combined <- (newdata$mortality_male + newdata$mortality_female) / 2

# plot mortality for males
plot_male <- ggplot(newdata, aes(x = logtime, y = mortality_male, color = as.factor(temp))) +
  geom_line() +
  labs(title = "Predicted Mortality for Males", 
       y = "Mortality", 
       x = "Log(Time)",
       color = "Temperature") +
  theme_minimal()

# plot mortality for females
plot_female <- ggplot(newdata, aes(x = logtime, y = mortality_female, color = as.factor(temp))) +
  geom_line() +
  labs(title = "Predicted Mortality for Females", 
       y = "Mortality", 
       x = "Log(Time)",
       color = "Temperature") +
  theme_minimal()

# plot combined mortality
plot_combined <- ggplot(newdata, aes(x = logtime, y = mortality_combined, color = as.factor(temp))) +
  geom_line() +
  labs(title = "Predicted Combined Mortality", 
       y = "Mortality", 
       x = "Log(Time)",
       color = "Temperature") +
  theme_minimal()

# Combine all three plots (side by side in patchwork)
plot_male + plot_female + plot_combined

# view data --> only if you want to 
view(newdata)