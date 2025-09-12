#Statistical Inference 11: Data-driven models

#11.3 Exercises

#dataset to answer exercises
library(dslabs)
x <- heights |> filter(sex == "Male") |>
  pull(height)

#1. Mathematically speaking, x is our population. Using the urn 
#analogy, we have an urn with the values of x in it. What are the
# average and standard deviation of our population?
mean(x)
#[1] 69.31475
sd(x)
#[1] 3.611024

#2. Call the population average computed above mu and the 
#standard deviation sd. Now take a sample of size 50, with 
#replacement, and construct an estimate for mu and sd.

set.seed(1)
n<-50
X<-sample(x, n, replace = TRUE)
mean(X)
#[1] 70.47293
sd(X)
#[1] 3.426742

#3. What does the theory tell us about the sample average X 
#and how it is related to mu?

#It is a random variable with an expected value mu and standard
#error sd/sqrt(samplesize)

#4. So, how is this useful? We are going to use an oversimplified
# yet illustrative example. Suppose we want to know the average
# height of our male students, but we can only measure 50 of the
# 708. We will use X as our estimate. We know from the answer to
# exercise 3 that the standard error of our estimate X-mu is 
#sd/sqrt(samplesize). We want to compute this, but we don’t know 
#sd. Based on what is described in this section, show your estimate of 
#sd.

sd_X<-sd(X)
se_hat_X<-sd_X/sqrt(n)
se_hat_X
#[1] 0.4846145


#5. Now that we have an estimate of sd, let’s call our estimate 
#s. Construct a 95% confidence interval for mu.
ci<-c(qnorm(0.025, mean(X), se_hat_X), qnorm(0.975, mean(X), se_hat_X))
ci
#[1] 69.52310 71.42276

#6. Now run a Monte Carlo simulation in which you compute 
#10,000 confidence intervals as you have just done. What 
#proportion of these intervals include mu?
x_mu<-mean(x)
set.seed(1)
n<-50
b<-10^4
result<-replicate(b, {
  X<-sample(x, n, replace = T)
  sd_X<-sd(X)
  se_hat_X<-sd_X/sqrt(n)
  interval<-c(qnorm(0.025, mean(X), se_hat_X), qnorm(0.075, mean(X), se_hat_X))
  between(x_mu, interval[1], interval[2])
})
mean(result)
#[1] 0.0542

#7. Use the qnorm and qt functions to generate quantiles. Compare
# these quantiles for different degrees of freedom for the 
#t-distribution. Use this to motivate the sample size of 30 
#rule of thumb.

#qt() quantiles of t-distribution
#qnorm() quantiles of normal distribution

#using the sample size of 50 from this exercise set, the degrees
#of freedom is 49
#95% confidence interval of two tails
qt(0.025, 49)
#1] -2.009575
#running with a n=30 sample
qt(0.025, 29)
#[1] -2.04523
#for the sample 50
qnorm(.70, .69, 3.6)
#[1] 2.577842