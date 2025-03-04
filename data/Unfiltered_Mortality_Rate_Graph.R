# Install packages and load libraries

install.packages("dplyr")
install.packages("readxl")  # For reading Excel files
install.packages("ggplot2") # For creating bar graphs
install.packages("tidyr")

library(readxl)  # For reading .xlsx files
library(dplyr)
library(ggplot2)
library(tidyr)

data <- read_excel("/Users/garrettball/Documents/GitHub/beetle_tdt/data/data_mockup.xlsx", sheet = 2)


##BE CAREFUL## the code directly beneath this only necessary to be run ONCE, to remove the two data inserts that are unfinished *surely there is a better way to do this*

# remove data that has "NA" data entries (conufusing for now)
data_clean <- data %>%
  filter(row_number() != 879 & row_number() != 880)


# reshape the data to have a column for each observation time
data_long <- data_clean %>%
  pivot_longer(cols = c("0H", "24H", "48H"), 
               names_to = "time", 
               values_to = "status")

#count the number of 'Y' (alive) and 'N' (dead) for each Temp and Time
data_long <- data_long %>%
  group_by(Temp, time, status) %>%
  summarise(count = n()) %>%
  ungroup()   # Ungroup the data after summarizing

# Calculate mortality rate
mortality_data <- data_long %>%
  spread(key = status, value = count, fill = 0) %>%
  mutate(
    total = Y + N,  # Total count of observations
    mortality_rate = N / total  # Mortality rate = N (dead) / total
  )

# Bar graph
ggplot(mortality_data, aes(x = Temp, y = mortality_rate, fill = time)) +
  geom_bar(stat = "identity", position = "dodge") +  # Bar graph for mortality rate
  labs(title = "Mortality Rate of Soldier Beetles at Different Temperatures and Exposure Times",
       x = "Temperature (°C)",
       y = "Mortality Rate",
       fill = "Exposure Time") +
  theme_minimal()
