##------------------------------------###
##---- Final Analysis of Beetle TDT Data
##------------------------------------###

# Install packages and load libraries
pacman::p_load(readxl, ggplot2, tidyverse, janitor, patchwork, brms, glmmTMB)

# Load data
data <- read_excel("./data/data_mockup.xlsx", sheet = "all_data")

#rename (clean) column titles
data <- clean_names(data)

# Create binary mortality variables and log-transformed time
data <- data %>%
  mutate(
    initial_mortality = ifelse(x0h == "Y", 0, 1),
      final_mortality = ifelse(x48h == "Y", 0, 1),
              logtime = log(time),
			  c_temp = scale(temp, center = TRUE, scale = FALSE))  # Center temp for modeling

# Lets have a look at sample sizes across temps, sexes, and times
data %>%
  group_by(temp, sex, time) %>%
  summarise(n = n()) 

# Lets convert o a new data set the proportions alive and dead
data2 <- data %>% 
  group_by(temp, time, sex) %>%
  summarise(
	alive = sum(final_mortality == 0),  # Count of alive beetles
	dead = sum(final_mortality == 1),    # Count of dead beetles
	total = n(),
	prob_survival = alive / total)  %>% mutate(logtime = log(time), c_temp = scale(temp, center = TRUE, scale = FALSE))

## Plot the mortality rates across temps, times, and sexes
ggplot(data2, aes(x = logtime, y = prob_survival, colour = as.factor(temp))) +
  geom_line() +
  facet_wrap(~sex) + labs(x = "log(Time)", y = "Proportion Alive", colour = "Temperature (°C)")

# Fit 4-parameter logistic for survival probability
# Formula: Survival = (1-upper) + (upper-lower) / (1 + exp((logtime - ed50) / slope))
# Use inv_logit to constrain upper and lower to [0,1], exp to constrain slope > 0
# Simplified: only key parameters vary with predictors
form <- bf(
  alive | trials(total) ~ (1 - inv_logit(upper)) + (inv_logit(upper) - inv_logit(lower)) / (1 + exp((logtime - ed50) / exp(slope))),
  ed50  ~ 1 + c_temp,    # LT50 varies with c_temp and sex
  slope ~ 1 + c_temp,                  # Slope constant across groups
  lower ~ 1 + c_temp,                  # Baseline mortality constant
  upper ~ 1 + c_temp,           # Max mortality varies with temp only
  nl = TRUE)

fit <- brm(
  formula = form,
     data = data2,
   family = binomial(link = "identity"),
    prior = c(
      prior(normal(3, 2), nlpar = "ed50"),
      prior(normal(-1, 1), nlpar = "slope"),   # exp(-1) = 0.37
      prior(normal(-2, 1.5), nlpar = "lower"), # inv_logit(-2) = 0.12 (low baseline mortality)
      prior(normal(2, 1.5), nlpar = "upper")   # inv_logit(2) = 0.88 (high max mortality)
    ),
   chains = 4,
    cores = 4,
     iter = 4000,
  control = list(adapt_delta = 0.95), 
     file = "./output/models/beetle_tdt_model")

summary(fit)
plot(fit)
bayes_R2(fit)

##----------------------------------------------------------###
##----- Plot Model Predictions
##----------------------------------------------------------###

# Create prediction grid
temp_values <- unique(data2$temp)
logtime_seq <- seq(min(data2$logtime), max(data2$logtime), length.out = 100)

pred_grid <- expand.grid(
  logtime = logtime_seq,
  temp = temp_values
) %>%
  mutate(
    c_temp = temp - mean(data2$temp),  # Center temp to match model
    total = 10  # Arbitrary for prediction
  )

# Get fitted values (mean predictions with credible intervals)
preds <- fitted(fit, newdata = pred_grid, scale = "linear") %>%
  as.data.frame() %>%
  bind_cols(pred_grid) %>%
  rename(survival = Estimate, lower_ci = Q2.5, upper_ci = Q97.5)

# Plot predictions with observed data
ggplot() +
  # Observed data
  geom_point(data = data2, aes(x = logtime, y = prob_survival, colour = as.factor(temp)), 
             size = 2, alpha = 0.6) +
  # Model predictions (mean)
  geom_line(data = preds, aes(x = logtime, y = survival, colour = as.factor(temp)), 
            linewidth = 1) +
  # Credible intervals
  geom_ribbon(data = preds, aes(x = logtime, ymin = lower_ci, ymax = upper_ci, 
                                fill = as.factor(temp)), alpha = 0.2) +
  labs(x = "Log(Time)", y = "Survival Probability", 
       colour = "Temperature (°C)", fill = "Temperature (°C)",
       title = "4-Parameter Logistic Model: Observed vs Predicted") +
  theme_minimal() +
  theme(legend.position = "bottom")

##----------------------------------------------------------###
##----- Normal Bernoulli GLMM for comparison
##----------------------------------------------------------###

# Fit a simpler binomial GLMM for comparison
form2 <- bf(final_mortality ~ logtime * c_temp * sex)

fit2 <- brm(
  formula = form2,
	 data = data,
   family = bernoulli(link = "logit"),
   chains = 4,
   cores = 4,
	 iter = 4000,
  control = list(adapt_delta = 0.95), 
	 file = "./output/models/beetle_tdt_bernoulli_model")
summary(fit2)
plot(fit2)
conditional_effects(fit2)
bayes_R2(fit2)

##----------------------------------------------------------###
##----- Understanding 4-parameter logistic model parameters:
##-----------------------------------------------------------###

# - ed50: The time at which 50% mortality is observed. A lower ed50 indicates that the beetles reach 50% mortality faster, suggesting higher sensitivity to the treatment.	
# - slope: The steepness of the curve around the ed50 point. A steeper slope indicates a more rapid increase in mortality around the ed50 time, while a shallower slope suggests a more gradual increase in mortality.
# - lower: The minimum mortality rate (asymptote) observed at the beginning of
#   the experiment (time = 0). This parameter indicates the baseline mortality rate before any significant effects of the treatment are observed.
# - upper: The maximum mortality rate (asymptote) observed at the end of
#   the experiment (as time approaches infinity). This parameter indicates the maximum mortality rate that can be achieved under the treatment conditions.

# Simulation: 4-Parameter Logistic Function for SURVIVAL
# Direct calculation formula: Survival = L_surv + (U_surv - L_surv) / (1 + exp((x - ed50) / slope))

# Set sensible parameters for mortality over time
U = 1      # Upper asymptote: maximum mortality (95%)
L = 0      # Lower asymptote: baseline mortality (5%)
ed50 = 3      # ED50/LT50: log(time) at 50% survival (e^3 ≈ 20 hours)
slope = 0.1   # Slope: controls steepness (smaller = steeper)

# Generate time sequence
logtime = seq(0, 6, length.out = 200)  # log(time) from 0 to 6

# Method 1: Calculate mortality, then convert to survival
mortality_prob <- L + (U - L) / (1 + exp((ed50 - logtime) / slope))
survival_indirect <- 1 - mortality_prob

# Method 2: Calculate survival directly (flipped asymptotes + flipped sign)
survival_prob <- (1-U) + (U-L) / (1 + exp((logtime - ed50) / slope))

# Verify both methods give identical results
cat("Methods identical?", all.equal(survival_indirect, survival_prob), "\n")

# Create visualization
par(mfrow = c(1, 2))

# Plot 1: Survival vs Log(Time)
plot(logtime, survival_prob, type = "l", lwd = 2, col = "darkgreen",
     xlab = "Log(Time)", ylab = "Survival Probability",
     main = "4-Parameter Logistic:\nSurvival vs Log(Time)")
abline(h = c(1-U, 1-L), lty = 2, col = "gray50")
abline(h = 0.5, v = ed50, lty = 3, col = "blue")
text(ed50 + 0.5, 0.5, paste("LT50 =", ed50), pos = 4, col = "blue")

# Plot 2: Survival vs Actual Time (hours)
actual_time <- exp(logtime)
plot(actual_time, survival_prob, type = "l", lwd = 2, col = "darkgreen",
     xlab = "Time (hours)", ylab = "Survival Probability",
     main = "4-Parameter Logistic:\nSurvival vs Actual Time")
abline(h = c(1-U, 1-L), lty = 2, col = "gray50")
abline(h = 0.5, v = exp(ed50), lty = 3, col = "blue")
text(exp(ed50) + 50, 0.5, paste("50% survival at", round(exp(ed50), 1), "hrs"), 
     pos = 4, col = "blue")