#Probabilities: Statistical Inference
#Exercises 5.9

#1. In American Roulette, you can also bet on green. 
#There are 18 reds, 18 blacks and 2 greens (0 and 00). 
#What are the chances the green comes out?
g <- 2
r <- 18
b <- 18
pg <- g/(g+b+r)
pg
#[1] 0.05263158
png <- 1 - pg
#[1] 0.9473684

#2. The payout for winning on green is $17 dollars. 
#This means that if you bet a dollar and it lands on 
#green, you get $17. Create a sampling model using 
#sample to simulate the random variable 
#for your winnings. Hint: Refer to the example below 
#for how it should look like when betting on red.
X <- sample(c(17, -1), 10, replace = TRUE, prob = c(0.053, 0.947))
sum(X)
#[1] -10
X <- sample(c(17, -1), 10, replace = TRUE, prob = c(pg, png))
sum(X)
#[1] 26
#probability of landing on red
pr <- sample(c(1, -1), 10, replace = TRUE, prob = c(9/19, 10/19))
sum(pr)
#[1] 6