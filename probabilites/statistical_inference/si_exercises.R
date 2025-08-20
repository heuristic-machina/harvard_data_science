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
mean(pr)
#[1] 0.6

#3. Compute the expected value of X.
#monte carlo approximation
B <- 10^6
X <- sample(c(17, -1), B, replace = TRUE, prob = c(pg, png))
mean(X)
#[1] -0.051832

#maths instead of monte carlo comparison
y <- 17*pg + (-1*png)
y
#[1] -0.05263158

#4. Compute the standard error of X.  
#Note: store computation in se_pg instead of 'X"
se_pg <- abs(17 - -1) * sqrt(pg*png)
se_pg
#[1] 4.019344

#5. Now create a random variable S that is the 
#sum of your winnings after betting on green 
#1000 times. Hint: change the argument size 
#and replace in your answer to exercise 2. 
#Start your code by setting the seed to 1 with set.seed(1).
set.seed(1)
B <- 1000
S <- sample(c(17, -1), B, replace = TRUE, prob = c(pg, png))
sum(S)
#[1] -10

#6What is the expected value of S?
# EV = product number of bets times the piai of each probability pi
# ai is the payout of each pi
B*(pg*17 + png*-1)
#[1] -52.63158