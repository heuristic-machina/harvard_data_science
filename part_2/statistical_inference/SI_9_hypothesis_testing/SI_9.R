#Statisitcal Inference 9: Hypothesis Testing

#9.1 p-values

#Phrasing research asking questions, for example:
#Does a medication extend the lives of cancer patients?
#Does an increase in gun sales correlate with more gun violence?
#Does class size affect test scores?

#Exercises 9.3

#1 Generate a sample of size N=1000 from an urn model with 50% 
#blue beads:
N <- 1000
theta <- 0.5
x <- rbinom(N, 1, 0.5)
x_hat<-mean(x)
x_hat
#[1] 0.478
se<-sqrt(x_hat*(1-x_hat)/N)
#[1] 0.01579465
z <- (x_hat - theta) / se
#[1] -1.456189
p_value <- 2 *(1-pnorm(abs(z)))
p_value
#[1] 0.1453403
#Hypothesis H0: p=0.5 vs H1 != 0.5 with an alpha=0.05
#the p_value > alpha, so the null hypothesis is not rejected

#then, compute a p-value if theta=0.5. Repeat this 10,000 times
# and report how often the p-value is lower than 0.05?
N <- 10000
theta <- 0.5
x <- rbinom(N, 1, 0.5)
x_hat<-mean(x)
x_hat
#[1] 0.5002
se<-sqrt(x_hat*(1-x_hat)/N)
#[1] 0.005
z <- (x_hat - theta) / se
#[1] 0.04
p_value <- 2 *(1-pnorm(abs(z)))
p_value
#[1] 0.9680931
#Hypothesis H0: p=0.5 vs H1 != 0.5 with an alpha=0.05
#the p_value > alpha, so the null hypothesis is not rejected

#Finding how often less than 0.05
set.seed(2)

M <- 1000
N <- 10000
p0 <- 0.5
p1 <- 0.505                       # try 0.505, 0.51, 0.52, etc.

pvals <- replicate(M, {
  k <- rbinom(1, N, p1)
  phat <- k / N
  se <- sqrt(phat * (1 - phat) / N)
  z <- (phat - p0) / se
  2 * (1 - pnorm(abs(z)))
})

mean(pvals < 0.05)                 # estimated power at p1
#[1] 0.168

#How often is it lower than 0.01?
set.seed(2)

M <- 1000
N <- 10000
p0 <- 0.5
p1 <- 0.505                       # try 0.505, 0.51, 0.52, etc.

pvals <- replicate(M, {
  k <- rbinom(1, N, p1)
  phat <- k / N
  se <- sqrt(phat * (1 - phat) / N)
  z <- (phat - p0) / se
  2 * (1 - pnorm(abs(z)))
})

mean(pvals < 0.01)                 # estimated power at p1
#[1] 0.054

#2 Make a histogram of the p-values you generated in exercise 1.
hist(pvals)
#c. The p-values are uniformly distributed.
#null pvals are uniformly distributed between 0 and 1
#the peak closest to 0 is where the alternative hypothesis live along
#with some potential false positives

#3. Demonstrate, mathematically, why we see the histogram we see in exercise 
# Hint: To compute the p-value, we need to calculate a test statistic, Z.
# We can approximate Z using the CLT, which tells us that Z approximately 
#follows a standard normal distribution. The p-value is calculated as: 
# p = 2{1 - phi(|z|)}  where: z is the observed value of Z and phi(z) 
#is the CDF of the standard normal distribution (pnorm(z) in R). To 
#understand the distribution of the p-values, consider the probability 
#that the p-value is less than or equal to some threshold alpha between
# 0 and 1: Pr(p <= alpha) = Pr({2{1 - phi(|z|)} <= alpha. Remember that 
#p follows a uniform distribution if Pr(p <= alpha) = a.

#answered in 1 & 2

#4 Generate a sample of size N=1000 from an urn model with 52% blue
# beads:
N <- 1000 
theta <- 0.52
x <- rbinom(N, 1, theta)

#Compute a p-value to test if theta=0.5. Repeat this 10,000 times 
#and report how often the p-value is larger than 0.05? Note that 
#you are computing 1 - power.

x_hat<-mean(x)
x_hat
#[1] 0.502
se<-sqrt(x_hat*(1-x_hat)/N)
#[1] 0.01581126
z <- (x_hat - theta) / se
z
#[1] -1.138429
p_value <- 2 *(1-pnorm(abs(z)))
p_value
#[1] 0.2549414

#Finding how often less than 0.05
set.seed(2)

M <- 1000
N <- 10000
p0 <- 0.52
p1 <- 0.5                      # try 0.505, 0.51, 0.52, etc.

pvals <- replicate(M, {
  k <- rbinom(1, N, p1)
  phat <- k / N
  se <- sqrt(phat * (1 - phat) / N)
  z <- (phat - p0) / se
  2 * (1 - pnorm(abs(z)))
})

mean(pvals < 0.01)                 # estimated power at p1

#[1] 0.969

#5 Repeat exercise for but for the following values:
values <- expand.grid(N = c(25, 50, 100, 500, 1000), 
                      theta = seq(0.51 ,0.75, 0.01))


set.seed(123)

# Parameters
M <- 10000   # number of simulations per combo
p0 <- 0.5    # null hypothesis proportion

# Value grid
values <- expand.grid(
  N = c(25, 50, 100, 500, 1000),
  theta = seq(0.51, 0.75, 0.01)
)

# Function to estimate power
estimate_power <- function(N, theta, M, p0) {
  pvals <- replicate(M, {
    k <- rbinom(1, N, theta)
    phat <- k / N
    se <- sqrt(phat * (1 - phat) / N)
    z <- (phat - p0) / se
    2 * (1 - pnorm(abs(z)))
  })
  mean(pvals < 0.05)  # power = proportion of p-values < alpha
}

# Apply to all combos
values$power <- mapply(estimate_power, values$N, values$theta,
                       MoreArgs = list(M = M, p0 = p0))

# Load ggplot2
library(ggplot2)

# Plot
ggplot(values, aes(x = N, y = power, color = factor(theta), group = theta)) +
  geom_line() +
  geom_point(size = 0.7) +
  labs(
    title = "Power Curves for Various Effect Sizes",
    subtitle = paste0("Null hypothesis p0 = ", p0, ", α = 0.05, ", M, " simulations each"),
    x = "Sample Size (N)",
    y = "Estimated Power",
    color = expression(theta)
  ) +
  theme_minimal()
