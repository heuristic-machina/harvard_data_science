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
ev_S <- B*(pg*17 + png*-1)
#[1] -52.63158

#7 What is the standard error of S?
se_S <- sqrt(1000)*abs(17--1)*sqrt(pg*png)
#[1] 127.1028

#8. What is the probability that you end up winning money? 
#Hint: Use the Central Limit Theorem CLT.

mu <- n*(pg*17-1*(png))
mu
#[1] -52.631

#CLT standard error=sqrt(n)*abs(b-a)*sqrt(pg*png)
clt_se <- sqrt(1000)*abs(17--1)*sqrt(pg*png)
clt_se
#[1] 127.103

#percentage greater than or to the right of pnorm
#probability of winning
1-pnorm(0, mu, clt_se)
#[1] 0.3394053

#9. Create a Monte Carlo simulation that generates 
#1,000 outcomes of S. Compute the average and 
#standard deviation of the resulting list to confirm 
#the results of 6 and 7. Start your code by setting 
#the seed to 1 with set.seed(1)
set.seed(1)
g<-2
b<-18
r<-18
pg<-g/(g+b+r)
png<-1-pg
bet<-10^4
n<-10^3
roulette_winnings <- function(n){
  X <- sample(c(17, -1), n, replace=TRUE, prob=c(pg, png))
  sum(X)
}
S<-replicate(bet, roulette_winnings(n))
mean(S)
#[1] -52.3324
sd(S)
#[1] 126.9762

#10. Now check your answer to 8 using the Monte Carlo result.
mean(S>0)
#[1] 0.3391

#11. The Monte Carlo result and the CLT approximation are 
#close, but not that close. What could account for this?

#The CLT does not work as well when the probability of 
#success is small. In this case, it was 1/19. If we make 
#the number of roulette plays bigger, they will match better.

#12. Now create a random variable Y that is your average 
#winnings per bet, after playing off your winnings after 
#betting on green 1,000 times.
set.seed(1)
n<-1000
pg<-1/19
png<-1-pg
X<-sample(c(-1, 17), n, replace=TRUE, prob=c(png,pg))
Y<-mean(X)
Y
#[1] -0.01

#13. What is the expected value of Y?
ev_Y <- pg*17+png*-1
#[1] -0.05263158

#14. What is the standard error of Y?
n<-1000
se<-abs(-1-17)*sqrt(png*pg)/(sqrt(n))
#[1] 0.1271028

#15. What is the probability that you end 
#up with winnings per game that are positive? 
#Hint: use the CLT.
avg<-17*pg-1*png
se<-1/(sqrt(n))*(17--1)*sqrt(png*pg)
1-pnorm(0, avg, se)
#[1] 0.3394053

#16. Create a Monte Carlo simulation that 
#generates 2,500 outcomes of Y. Compute 
#the average and standard deviation of 
#the resulting list to confirm the results
# of 6 and 7. Start your code by setting 
#the seed to 1 with set.seed(1).
set.seed(1)
g<-2
b<-18
r<-18
pg<-g/(g+b+r)
png<-1-pg
bet<-2500
n<-10^3
roulette_winnings <- function(n){
  X <- sample(c(17, -1), n, replace=TRUE, prob=c(pg, png))
  sum(X)
}
Y<-replicate(bet, roulette_winnings(n))
mean(Y)
#[1] -54.3376
sd(Y)
#[1] 125.9509

#17. Now check your answer to 8 using the Monte Carlo result.
mean(Y>0)
#[1] 0.3372

#18. The Monte Carlo result and the CLT approximation are 
#now much closer. What could account for this?
#The CLT works better when the sample size is larger. 
#We increased from 1,000 to 2,500.

#19-23(first half) Bank Interest Rates: 2% customers default on loans.  Bank gives out
#n=1000 loans for $180,000.  The bank loses $200,000 per foreclosure.
#Find expected profit S for bank
n<-1000
loss_per_foreclosure<--200000
p<-0.02
defaults<-sample(c(0,1), n, replace=TRUE, prob=c(1-p, p))
sum(defaults*loss_per_foreclosure)
#[1] -5e+06

#monte carlo
#sample shows either does not lose money or customer defaults and loses money
B<-10000
losses<-replicate(B, {
  defaults<-sample(c(0,1), n, replace=TRUE, prob=c(1-p, p))
  sum(defaults*loss_per_foreclosure)
})

#plot
data.frame(losses_in_millions = losses/10^6) %>% 
  ggplot(aes(losses_in_millions)) +
  geom_histogram(binwidth = 0.6, col = 'green')

#random variable
#expected value of 1000 loans
ev_1000_loans<- n*(p*loss_per_foreclosure + (1-p)*0)
ev_1000_loans
#[1] -4e+06
#-$4million
#standard error of 1000 loans
se_1000_loans<- sqrt(n)*abs(loss_per_foreclosure)*sqrt(p*(1-p))
se_1000_loans
#[1] 885437.7

#23second half-26 Mitigate risk and set chance of losing money, 
#x to be 1 in 100,#Hint:  Pr(S<0)=0.01.

#calculating interest rate for 1% probability of losing money
l<-loss_per_foreclosure
z<-qnorm(0.01)
x<- -l*(n*p - z*sqrt(n*p*(1-p)))/ (n*(1-p) + z*sqrt(n*p*(1-p)))
#required profit when loan is not a foreclosure
x
#[1] 6249.181

#interest rate
x/180000
#[1] 0.03471767

#profit per loan expected value
ev_per_loan <- loss_per_foreclosure*p + x*(1-p)
ev_per_loan
#[1] 2124.198

#expected value over n loans
ev_total_loans <- n*ev_per_loan
ev_total_loans
#[1] 2124198

#27 Double check 23-26 using monte carlo
B<-100000
profit<- replicate(B,{
  one_percent_p<- sample(c(x, loss_per_foreclosure), n, replace=TRUE, prob = c(1-p,p))
  sum(one_percent_p)
})
mean(profit)
#[1] 2122739

#probability of losing money
mean(profit<0)
#[1] 0.01297

#28 An employee thinks selling more loans and off-setting risk of 
#using law of large numbers with higher probability of default and x.
more_loans<-loss_per_foreclosure*.04 + 9000*(.96)
more_loans
#[1] 640

#29. How much do we need to increase n by to assure the probability of 
#losing money is still less than 0.01?
z <- qnorm(0.01)
l <- loss_per_foreclosure
n <- ceiling((z^2*(x-l)^2*p*(1-p))/(l*p + x*(1-p))^2)
n 
#[1] 22163
ev_more_loans <- n*(loss_per_foreclosure*p + x*(1-p))
ev_more_loans
#expected profit
#[1] 14184320