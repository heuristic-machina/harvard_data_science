#Statistical Inference 7: Central Limit Theorem

# X = a-m/s

#CLT plugs in a random variable estimate to find the standard error 
#when the probability is not known

se_rand_x = sqrt(x(1-x)/n)
#using the 12 blue and 13 red beads to plug in values for estimated x
x_est<- 0.48
se<-sqrt(x_est*(1-x_est)/25)
se
#[1] 0.09991997

#pnorm - gives percentage to the left (or less than) of a given point
#1-pnorm - gives percentage greater than a given point
#pnorm(q, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)

#below pnorm is used to find probability within 1% of p now that we have
#the standard error

#use the quantile, q of 1% and se of 0.0999 to find mean probability
pnorm(0.01/se)-pnorm(-0.01/se)
#[1] 0.07971926


#7.2 the spread
#mu=p-(1-p)=2p-1

#7.4 exercises

#1.1. Write an urn model function that takes the 
#proportion of Democrats and the sample size N as 
#arguments, and returns the sample average if 
#Democrats are 1s and Republicans are 0s. Call 
#the function take_sample.

take_sample<- function(p, N){
  x<-sample(c(1,0), size=N, replace=TRUE, prob=c(p, 1-p))
  mean(x)
}

#2. Now assume p <- 0.45 and that your sample size 
#is N=100. Take a sample 10,000 times and save the
#vector of mean(X)-p into an object called errors. 
#Hint: use the function you wrote for exercise 1 
#to write this in one line of code.
p<-0.45
N<-100
errors<-replicate(10000, take_sample(p, N)-p)

#3. The vector errors contains, for each simulated 
#sample, the difference between the actual p and 
#our estimate X. We refer to this difference as 
#the error. Compute the average and make a histogram
# of the errors generated in the Monte Carlo simulation 
#and select which of the following best describes their 
#distributions: mean(errors) hist(errors).
mean(errors)
#[1] 0.00049
hist(errors)

#The errors are symmetrically distributed around 0

#4 The error X-p is a random variable. In practice, 
#the error is not observed because we do not know p.
#Here, we observe it since we constructed the simulation. 
#What is the average size of the error if we define 
#the size by taking the absolute value |X-p|?

mean(abs(errors))
#1] 0.039886

#5. The standard error is related to the typical size
#of the error we make when predicting. For mathematical
#reasons related to the Central Limit Theorem, we 
#actually use the standard deviation of errors, rather 
#than the average of the absolute values, to quantify 
#the typical size. What is this standard deviation of 
#the errors?
sqrt(mean(errors^2))
#[1] 0.05016453

#6. The theory we just learned tells us what this 
#standard deviation is going to be because it is the
# standard error of X. What does theory tell us is
# the standard error of X for a sample size of 100?
p<-.45
n<-100
se_rvx<- sqrt(p*(1-p)/n)
se_rvx
#[1] 0.04974937

#7. In practice, we don’t know p,so we construct an estimate
#of the theoretical prediction based by plugging in X for
#p. Compute this estimate. Set the seed at 1 with set.seed(1).

set.seed(1)
n<-100
p<-.45
x<-sample(c(1,0), size=n, replace = TRUE, prob = c(p, 1-p))
sub_x<-mean(x)
estimate<-sqrt(sub_x*(1-sub_x)/n)
estimate
#[1] 0.04983974