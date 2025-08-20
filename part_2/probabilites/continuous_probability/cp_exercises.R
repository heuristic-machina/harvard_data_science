#Continous probability
#Exercise 4.7

#1. Assume the distribution of female heights is approximated by a normal 
#distribution with a mean of 64 inches and a standard deviation of 3 inches. 
#If we pick a female at random, what is the probability that she is 5 feet or shorter?

x <- heights %>% filter(sex == "Female") %>% pull(height)
s <- 3
m <- 64
pnorm(60, m, s)
#[1] 0.09121122

#2.Assume the distribution of female heights is approximated by a normal 
#distribution with a mean of 64 inches and a standard deviation of 3 inches. 
#If we pick a female at random, what is the probability that she is 6 feet or taller?
1 - pnorm(72, m, s)
#[1] 0.003830381

#3. Assume the distribution of female heights is approximated by a normal 
#distribution with a mean of 64 inches and a standard deviation of 3 inches. 
#If we pick a female at random, what is the probability that she is between 61 and 67 inches?
pnorm(67, m, s) - pnorm(61, m, s)
#[1] 0.6826895

#4. Repeat the exercise above, but convert everything to centimeters. That is, 
#multiply every height, including the standard deviation, by 2.54. What is the answer now?
m <- 64*2.54
s <- 3*2.54
pnorm(67*2.54, m, s)-pnorm(61*2.54, m, s)
#[1] 0.6826895

#5Compute the probability that a randomly picked, normally distributed random 
#variable is within 1 SD from the average.
#same as 4 and 5

#6 To see the math that explains why the answers to questions 3, 4, and 5 are 
#the same, suppose we have a random variable with average m and standard error 
#s. Suppose we ask the probability of X being smaller or equal to a. 
#Remember that, by definition, a is (a−m)/s standard deviations s away from 
#the average m. The probability is: Pr(X≤a). Now we subtract μ to both sides 
#and then divide both sides by σ:Pr(X−m/s≤a−m/s)

#The quantity on the left is a standard normal random variable. It has an 
#average of 0 and a standard error of 1. We will call it Z: Pr(Z≤a−ms)
#So, no matter the units, the probability of X≤a is the same as the probability 
#of a standard normal variable being less than (a−m)/s. If mu is the average and 
#sigma the standard error, which of the following R code would give us the right
# answer in every situation:
m <-64
s <- 3
pnorm((67-m)/s)
#[1] 0.8413447

#7. Imagine the distribution of male adults is approximately normal with an 
#expected value of 69 and a standard deviation of 3. How tall is the male in the 
#99th percentile? Hint: use qnorm.
qnorm(.99, mean=69, sd=3)
#[1] 75.97904

#8. The distribution of IQ scores is approximately normally distributed. The 
#average is 100 and the standard deviation is 15. Suppose you want to know the 
#distribution of the highest IQ across all graduating classes if 10,000 people 
#are born each in your school district. Run a Monte Carlo simulation with B=1000 
#generating 10,000 IQ scores and keeping the highest. Make a histogram.
library(ggplot2)
m <- 100
s <- 15
B <- 10000
set.seed(1113)
iq <- replicate(B, {
     sim_data <- rnorm(10000, m, s)
     max(sim_data)
 })
hist(iq)