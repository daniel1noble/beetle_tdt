    # load libraries --> Install?
library(readxl)
library(tidyverse)
library(janitor)
library(ggplot2)

##read dataset
data <- read_excel("/Users/garrettball/Documents/GitHub/beetle_tdt/data/data_mockup.xlsx", 
                   sheet = "all_data")

#clean column (Maybe Print Column Names?) (xOh and x48h now)
data <- clean_names(data)

# Convert '0H' and '48H' to binary values (likely renamed to 'x0h' and 'x48h')
data <- data %>%
  mutate(
    initial_mortality = ifelse(x0h == "Y", 0, 1),
    final_mortality = ifelse(x48h == "Y", 0, 1)
  )

#Seperate males and females for BGLM - time filter
glm_male <- glm(final_mortality ~ time, data = filter(data, sex == "M"), 
                family = binomial(link = "logit"))
glm_female <- glm(final_mortality ~ time, data = filter(data, sex == "F"), 
                  family = binomial(link = "logit"))

#summary 
summary(glm_male)
summary(glm_female)

#ensure the dataset's for males and females are correctly filtered (because of NA in dateset)
# Filter for males and remove missing values, match with glm_male's data
data_male <- data %>%
  filter(sex == "M") %>%
  na.omit()  # This ensures we use the same rows as glm_male

#predictions for males
data_male <- data_male %>%
  mutate(predicted_mortality = predict(glm_male, newdata = data_male, type = "response"))

# Filter for females and remove missing values, match with glm_female's data
data_female <- data %>%
  filter(sex == "F") %>%
  na.omit()  # This ensures we use the same rows as glm_female

    #Better to remove rows 879 and 880 from OG dataset?

#predictions for females
data_female <- data_female %>%
  mutate(predicted_mortality = predict(glm_female, newdata = data_female, type = "response"))

# Create datasets with predicted values for plotting
# Male plot
plot_male <- ggplot(data_male, aes(x = time, y = final_mortality)) +
  geom_jitter(height = 0.05, width = 0, alpha = 0.3, color = "blue") +  # Scatter plot with jitter
  geom_smooth(aes(y = predicted_mortality), method = "glm", 
              method.args = list(family = "binomial"), color = "blue") +
  labs(title = "Mortality vs. Time (Males)", y = "Final Mortality", x = "Time (Hours)") +
  theme_minimal()

# Female plot
plot_female <- ggplot(data_female, aes(x = time, y = final_mortality)) +
  geom_jitter(height = 0.05, width = 0, alpha = 0.3, color = "red") +
  geom_smooth(aes(y = predicted_mortality), method = "glm", 
              method.args = list(family = "binomial"), color = "red") +
  labs(title = "Mortality vs. Time (Females)", y = "Final Mortality", x = "Time (Hours)") +
  theme_minimal()

#print/show me
print(plot_male)
print(plot_female)
