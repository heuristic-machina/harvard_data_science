#Linear Models 16: Measurement error models

#1. Plot CO2 levels for the first 12 months of the co2 dataset
#and notice it seems to follow a sin wave with a frequency of 
#1 cycle per month. This means that a measurement error model 
#that might work is:

#yi = mu + A sin(2*pi*ti/12 + phi) + epsilon*i

#with ti the number for observation i.  Is this a linear model for 
#the parameters mu, A and phi?

#mu being a constant is linear, A multiplies against a fixed sin term and 
#is also linear.  Phi is a phase shift and changes sin and is non-linear.

#2. Using trigonometry, we can show that we can rewrite this model as:

#y(i) = beta0 + beta1 sin(2*pi*t(i)/12) + beta2 cos(2*pi*t(i)/12) + epsilon(i)

#the coefficients in this model is linear

#3 Find the least square estimates for the Bs using lm(). Plot y(i) versus t(i) 
#with a curve on the same plot showing y-hat versus t(i).

# Simulate data
set.seed(123)
t <- 1:24  # time points (e.g., months)
beta0 <- 10
beta1 <- 3
beta2 <- -2
epsilon <- rnorm(length(t), mean = 0, sd = 1)

y <- beta0 +
  beta1 * sin(2 * pi * t / 12) +
  beta2 * cos(2 * pi * t / 12) +
  epsilon

# Fit the model using lm()
fit <- lm(y ~ sin(2 * pi * t / 12) + cos(2 * pi * t / 12))

# View estimated coefficients
coef(fit)

# Predicted values
y_hat <- fitted(fit)

# Plot observed data
plot(t, y, pch = 19, col = "blue",
     xlab = "t (time)", ylab = "y",
     main = "Observed vs Fitted Seasonal Model")

# Add fitted curve
lines(t, y_hat, col = "red", lwd = 2)

# Add legend
legend("topright", legend = c("Observed", "Fitted"),
       col = c("blue", "red"), pch = c(19, NA), 
       lty = c(NA, 1), lwd = c(NA, 2))

#4. Now fit a measurement error model to the entire co2 dataset 
#that includes a trend term that is a parabola as well as the 
#sine wave model.

library(mecor)

# Assume co2_data has columns: y (CO2), t_obs (observed time in months)
# and we know the SD of measurement error in t_obs is sigma_t

# Define sine and cosine terms from observed time
co2_data$sin_term <- sin(2 * pi * co2_data$t_obs / 12)
co2_data$cos_term <- cos(2 * pi * co2_data$t_obs / 12)

# Fit measurement error model
fit <- mecor(
  y ~ t_obs + I(t_obs^2) + sin_term + cos_term,
  error = list(t_obs ~ sigma_t),
  data = co2_data
)

summary(fit)
