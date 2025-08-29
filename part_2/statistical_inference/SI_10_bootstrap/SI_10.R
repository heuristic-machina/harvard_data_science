#Statistical Inference 10: Bootstrap

#10.1 median income example
set.seed(1995)
n <- 10^6
income <- 10^(rnorm(n, log10(45000), log10(3)))
hist(income/10^3, nclass = 1000)

m <- median(income)
m
#> [1] 44939
 
#> if access to whole application is unavailable a sample
#> can be used instead
N<-100
x<-sample(income, N)
median(x)
#[1] 38461.33

#10.2 confidence intervals for the median
library(gridExtra)
B<-10^4
m<-replicate(B, {
  x<-sample(income, N)
  median(x)
})
hist(m, nclass=30)
qqnorm(scale(m)); abline(0,1)

#when we don't have the distribution we used the central
#limit theorem
median(x) + 1.96*sd(x)/sqrt(N)*c(-1,1)
#[1] 21017.93 55904.72
#comparing the above clt given a known distribution confidence
# interval
quantile(m, c(.025, .975))
#2.5%    97.5% 
#34607.70 58802.04 

#Bootstrap allows resampling of the sample using a monte carlo
#simulation with replacement then taking the median of the
#simulation.  The simulation takes the same size as the original
#dataset
B<-10^4
m_star<-replicate(B, {
  x_star<-sample(x, N, replace=TRUE)
  median(x_star)
})

#bootstrap mimics confidence interval below using the simulation
#median.  The results are very close to the theoretical 
#distribution.
quantile(m_star, c(0.025, 0.975))
#2.5%    97.5% 
#30252.87 57046.02 

#10.3 exercises
#1. Generate a random dataset like this:
y<-rnorm(100, 0, 1)
median(y)
#[1] -0.01357456

#Estimate the 75th quantile, which we know is:
qnorm(.75)
#[1] 0.6744898

#with the sample quantile:
quantile(y, .75)
#Run a monte carlo simulation to learn the expected value and 
#standard error of this random variable.
# Set parameters
n <- 100              # Sample size
B <- 10000            # Number of simulations
quantile_vals <- numeric(B)  # Storage for quantile estimates

# Run Monte Carlo simulation
set.seed(123)         # For reproducibility
for (i in 1:B) {
  y <- rnorm(n, 0, 1)
  quantile_vals[i] <- quantile(y, 0.75)
}

# Analyze results
expected_value <- mean(quantile_vals)
standard_error <- sd(quantile_vals)

# Output
cat("Expected value of 75th quantile:", expected_value, "\n")
#Expected value of 75th quantile: 0.6645913 
cat("Standard error:", standard_error, "\n")
#Standard error: 0.1345276 

#2. In practice, we can’t run a Monte Carlo simulation because
# we don’t know if rnorm is being used to simulate the data. 
#Use the bootstrap to estimate the standard error using just 
#the initial sample y. Use 10 bootstrap samples.
# Original sample
set.seed(42)
y <- rnorm(100, 0, 1)

# Bootstrap parameters
B <- 10
boot_quantiles <- numeric(B)

# Bootstrap loop
set.seed(123)  # For reproducibility
for (i in 1:B) {
  boot_sample <- sample(y, size = length(y), replace = TRUE)
  boot_quantiles[i] <- quantile(boot_sample, 0.75)
}

# Estimate standard error
boot_se <- sd(boot_quantiles)

# Output results
cat("Bootstrap estimates of 75th quantile:\n", round(boot_quantiles, 4), "\n")
#Bootstrap estimates of 75th quantile:
#  0.8465 0.7088 0.8497 0.8063 0.6538 0.6616 0.9207 0.6517 0.6538 0.6448 
cat("Estimated standard error from bootstrap:", round(boot_se, 4), "\n")
#Estimated standard error from bootstrap: 0.105 

#3. Redo exercise 12, but with 10,000 bootstrap samples.
# Original sample
set.seed(42)
y <- rnorm(100, 0, 1)

# Bootstrap parameters
B <- 10000
boot_quantiles <- numeric(B)

# Bootstrap loop
set.seed(123)  # For reproducibility
for (i in 1:B) {
  boot_sample <- sample(y, size = length(y), replace = TRUE)
  boot_quantiles[i] <- quantile(boot_sample, 0.75)
}

# Estimate standard error
boot_se <- sd(boot_quantiles)

# Output results
median(boot_quantiles)
#[1] 0.6615581
cat("Estimated standard error from bootstrap:", round(boot_se, 4), "\n")
#Estimated standard error from bootstrap: 0.09 