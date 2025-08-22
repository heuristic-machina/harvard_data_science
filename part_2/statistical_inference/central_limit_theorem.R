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
  x<-sample(c(1,0), N, replace=TRUE, prob=c(p, 1-p))
  mean(x)
}
#arbitrary p and N
take_sample(.48, 1000)
#[1] 0.479
