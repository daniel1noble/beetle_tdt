# 1. Thermocouple Data
data <- data.frame(
  temp = c(38, 41, 44, 47),
  times = c(2.50, 2.45, 2.57, 5.11)
)

# 2. Model
model <- lm(log(times) ~ temp, data = data)

# 3. Dynamic Prediction for 50°C
new_data <- data.frame(temp = 50)
log_pred <- predict(model, newdata = new_data)

predicted_minutes <- exp(log_pred)
print(predicted_minutes) 
