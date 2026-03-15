#load libraries
library(readxl)
library(tidyverse)
library(janitor)
library(ggplot2)

#read data 
data <- read_excel("/Users/garrettball/Documents/GitHub/beetle_tdt/data/data_mockup.xlsx", 
                   sheet = "all_data")

#clean column names + check column names? (if fails to run) (xOh and x48h now)
data <- clean_names(data)

#mutate '0H' and '48H' into binary values (renamed 'x0h' and 'x48h')
data <- data %>%
  mutate(
    initial_mortality = ifelse(x0h == "Y", 0, 1),
    final_mortality = ifelse(x48h == "Y", 0, 1)
  )


#Fit male/female binomial GLMs separately, against time
glm_male <- glm(final_mortality ~ time + temp, data = filter(data, sex == "M"), 
                family = binomial(link = "logit"))
glm_female <- glm(final_mortality ~ time + temp, data = filter(data, sex == "F"), 
                  family = binomial(link = "logit"))

# print summaries (optional)
summary(glm_male)
summary(glm_female)

#datasets for males and females are correctly filtered and missing values are removed
# match with glm_male's data
data_male <- data %>%
  filter(sex == "M") %>%
  na.omit() 

#ensures we use the same rows as glm_male  

# predictions for males
data_male <- data_male %>%
  mutate(predicted_mortality = predict(glm_male, newdata = data_male, type = "response"))

# Filter for females and remove missing values (Important) 
data_female <- data %>%
  filter(sex == "F") %>%
  na.omit()  

     # This ensures we use the same rows as glm_female --> surely we just remove 879 and 880 from OG dataset? 

#predictions for females
data_female <- data_female %>%
  mutate(predicted_mortality = predict(glm_female, newdata = data_female, type = "response"))

# Plot BGLM for each temp, over time
plot_male_temp <- ggplot(data_male, aes(x = time, y = final_mortality, color = as.factor(temp))) +
  geom_jitter(height = 0.05, width = 0, alpha = 0.3) +  # Scatter plot with jitter
  geom_smooth(aes(y = predicted_mortality), method = "glm", 
              method.args = list(family = "binomial"), se = FALSE) +
  labs(title = "Mortality vs. Time (Males) by Temperature", 
       y = "Final Mortality", x = "Time (Minutes)", color = "Temperature") +
  theme_minimal()

plot_female_temp <- ggplot(data_female, aes(x = time, y = final_mortality, color = as.factor(temp))) +
  geom_jitter(height = 0.05, width = 0, alpha = 0.3) + 
  geom_smooth(aes(y = predicted_mortality), method = "glm", 
              method.args = list(family = "binomial"), se = FALSE) +
  labs(title = "Mortality vs. Time (Females) by Temperature", 
       y = "Final Mortality", x = "Time (Minutes)", color = "Temperature") +
  theme_minimal()

#print
print(plot_male_temp)
print(plot_female_temp)

