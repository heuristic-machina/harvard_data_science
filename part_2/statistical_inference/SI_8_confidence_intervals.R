#Statistical Inference 8: Confidence Intervals

#create an interval with a 95% chance of including p
#(95% confidence interval)
p <- 0.45
N <- 1000
x <- sample(c(0, 1), size = N, replace = TRUE, prob = c(1 - p, p))
x_hat <- mean(x)
se_hat <- sqrt(x_hat*(1 - x_hat)/N)
c(x_hat - 1.96*se_hat, x_hat + 1.96*se_hat)
#> [1] 0.418 0.480
#> The above monte carlo has random start and end variables so every sample 
#> changes when run
x <- sample(c(0,1), size = N, replace = TRUE, prob = c(1 - p, p))
x_hat <- mean(x)
se_hat <- sqrt(x_hat*(1 - x_hat)/N)
c(x_hat - 1.96*se_hat, x_hat + 1.96*se_hat)
#> [1] 0.4042727 0.4657273

#using pnorm to prove 95% probability
pnorm(1.96) - pnorm(-1.96)
#> [1] 0.95

#finding probability for a larger percentage using qnorm
#Pr(-z <= Z <= z) = 0.99
z <- qnorm(0.995)
z
#> [1] 2.58
pnorm(qnorm(.995))
#[1] 0.995
pnorm(1-qnorm(.995))
pnorm(1-2.58)
pnorm(-1.575829)
#[1] 0.05753257

pnorm(z)-pnorm(-z)
pnorm(2.58)-pnorm(-2.58)
0.995-0.005
#0.99
#confidence interval arbitrary probability
alpha=0.05
z=qnorm(1-alpha/2)
z=qnorm(1-0.025)
z=qnorm(0.975)
#[1] 1.959964

#8.1 Monte Carlo simulation
install.packages('dplyr')
library('dplyr')
#mean, std error, 95% confidence interval including p95% of time
n<-1000
b<-10000
conf_int<-replicate(b, {
  x<-sample(c(0,1), n, replace=TRUE, prob=c(1-p, p))
  x_hat<-mean(x)
  se_hat<-sqrt(x_hat*(1 - x_hat)/n)
  between(p, x_hat - 1.96*se_hat, x_hat + 1.96*se_hat)
  })
mean(conf_int)
#[1] 0.9496

#8.2 Exercises
library(dslabs)
library(tidyverse)
#filter polls ending 1 week before election
polls<- polls_us_election_2016 |> 
  filter(enddate >= '2016-10-31' & state='US')

#1 For the first poll, obtain sample size and estimated Clinton percentage
# with:
N<-polls$samplesize[1]
x_hat<-polls$rawpoll_clinton[1]/100
#Assuming there are only two candidates, construct a 95% confidence
#interval for the election night proportion p
library(magrittr)
data('polls_us_election_2016')
se_hat<-sqrt(x_hat*(1-x_hat)/N)
x_hat + c(-1, 1)*pnorm(0.975)*se_hat
#[1] 0.4611527 0.4788473

#2.Now use dplyr to add a confidence interval as two columns, call 
#them lower and upper, to the object poll. Then use select to show the 
#pollster, enddate, x_hat, lower, upper variables. Hint: define temporary
# columns x_hat and se_hat.

polls %>% mutate(x_hat=polls$rawpoll_clinton/100, 
                 se_hat=sqrt(x_hat*(1-x_hat)/samplesize),
                 lower=x_hat-pnorm(0.975)*se_hat, 
                 upper=x_hat+pnorm(0.975)*se_hat, 
                 hit=lower<=0.482 & upper>=0.482) %>% 
  select(pollster, enddate, x_hat, lower, upper, hit) %>% 
  summarize(mean(hit))
#  mean(hit)
#1 0.1857143

#3.The final tally for the popular vote was Clinton 48.2% and Trump 
#46.1%. Add a column, call it hit, to the previous table stating if the 
#confidence interval included the true proportion p=0.482 or not.
polls %>% mutate(x_hat=polls$rawpoll_clinton/100,
                 se_hat=sqrt(x_hat*(1-x_hat)/samplesize),
                 lower=x_hat-pnorm(0.975)*se_hat, 
                 upper=x_hat+pnorm(0.975)*se_hat,
                 hit=lower<=0.482& upper>=0.482) %>%
  select(pollster, enddate, x_hat, lower, upper, hit) 

#4For the table you just created, what proportion of confidence intervals
# included p?

#taking answer from #2 mean(hit)=0.1857143 from 0.50 confidence interval
#gives confidence interval of .314

#5 If these confidence intervals are constructed correctly, and the 
#theory holds up, what proportion should include p? 

#.95 of confidence intervals include the true proportion

#6 A much smaller proportion of the polls than expected produce 
#confidence intervals containing p. If you look closely at the
# table, you will see that most polls that fail to include p
#are underestimating. The reason for this is undecided voters, 
#individuals polled that do not yet know who they will vote for
# or do not want to say. Because, historically, undecideds divide
# evenly between the two main candidates on election day, it is 
#more informative to estimate the spread or the difference between 
#the proportion of two candidates d, which in this election was 
#0.482−0.461=0.021. Assume that there are only two parties and 
#that d=2p−1, redefine polls as below and re-do exercise 1, but 
#for the difference.
polls <- polls_us_election_2016 %>% filter(enddate >= "2016-10-31" & state == "U.S.") %>% mutate(d_hat=rawpoll_clinton/100-rawpoll_trump/100)
N<-polls$samplesize[1]
d_hat<-polls$d_hat[1]
x_hat<-(d_hat+1)/2
se_hat<-2*sqrt(x_hat*(1-x_hat)/N)
ci<-c(d_hat-qnorm(0.975)*se_hat, d_hat+qnorm(0.975)*se_hat)
se_hat
## [1] 0.02120683

#7 Now repeat exercise 3, but for the difference.
polls %>% mutate(x_hat=(d_hat+1)/2,
                 se_hat=2*sqrt(x_hat*(1-x_hat)/samplesize),
                 lower=d_hat-pnorm(0.975)*se_hat,
                 upper=d_hat+pnorm(0.975)*se_hat,
                 hit=lower<=0.021 & upper>=0.021) %>%
  select(pollster, enddate, d_hat, lower, upper, hit)