#Statistical Inference 12: Bayesian statistics

#12.3 Exercises

#1 Probability of SIDS (sudden infant death syndrome) is 1 in 8,500.
#The chances of 2 babies dying to the same mother is the product
#8500 x 8500 = 72,250,000.
#This calculation assumes each death as an independent event 
#ignoring possible genetic causes.  If genetics plays a role then:

#Pr(second case of SIDS | first case of SIDS) > Pr(first case of SIDS)

#2. Let’s assume that there is, in fact, a genetic component 
#to SIDS and the probability of Pr(second case of SIDS | first 
#case of SIDS) = 1/100, is much higher than 1 in 8,500. What 
#is the probability of both of her sons dying of SIDS?
a<-1/8500
b<-1/100
Pr<-(a*b)
Pr
#1.176e-06

#4 Assume that the probability of a murderer finding a way to kill
# her two children without leaving evidence of physical harm is:

#Pr(two children found dead with no evidence of harm ∣ murder) = 0.50

#Assume that the murder rate among mothers is 1 in 1,000,000.

#Pr(murder) = 1/1,000,000

#According to Bayes’ rule, what is the probability of:
  
#Pr(murder | two children found dead with no evidence of harm)

Pr_1<-1/8500
Pr_2<-1/100
#p of brothers dying of SIDs
Pr_A<-Pr_1*Pr_2

#p of murder
Pr_B<-1/1000000

#p of sons dying murder
Pr_BA <- 0.5

#p that brothers die of SIDs and its a murder
Pr_AB = Pr_BA * Pr_B/Pr_A
Pr_AB
#[1] 0.425

#6
polls<- polls_us_election_2016 |>
  filter(state=="Florida" & enddate >='2016-11-04') |>
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)
#Take the average spread of these polls. The CLT tells 
#us this average is approximately normal. Calculate an 
#average and provide an estimate of the standard error. 
#Save your results in an object called results.

results<-polls %>% summarise(avg=mean(spread),
                             se=sd(spread)/sqrt(n()))
results
#results
#avg          se
#1 0.004154545 0.007218692

#8 The CLT tells us that our estimate of the spread mu_hat
#has normal distribution with expected value mu and standard
#deviation sigma calculated in exercise 6. Use the formulas we 
#provided for the posterior distribution to calculate the expected
# value of the posterior distribution if we set theta=0 and
#tau=0.01.
mu<-0
tau<-0.01
#store se from results
sigma<-results$se
#store average from results
Y<-results$avg
#store sigma tau relationship
B<-sigma^2/(sigma^2+tau^2)
#expected value of posterior distribution
B*mu + (1 - B) * Y

#9. Now compute the standard deviation of the posterior 
#distribution.
#the defined variables above for quick view
mu <- 0
tau <- 0.01
sigma <- results$se
Y <- results$avg
B <- sigma^2 / (sigma^2 + tau^2)
#standard error of posterior distribution is incredibly small
#as is expected from inferring from the observed dataset
1 / ((1 / sigma^2) +(1 / tau^2))
#[1] 3.42579e-05

#10. Using the fact that the posterior distribution is normal,
# create an interval that has a 95% probability of occurring
# centered at the posterior expected value. Note that we call
# these credible intervals.
mu <- 0
tau <- 0.01
sigma <- results$se
Y <- results$avg
B <- sigma^2 / (sigma^2 + tau^2)
se<- sqrt(1 / (1 / sigma^2) +(1 / tau^2))

#95% credible interval
estimate<- B * mu + (1 - B) * Y
ci <- c(estimate - qnorm(0.975) * se, estimate + qnorm(0.975) * se)
ci
#[1] -195.9937  195.9991

#11. According to this analysis, what was the probability that Trump 
#wins Florida?

#using pnorm function where spread < 0, in Trump's favor
estimate<- B * mu + (1 - B) * Y
se<- sqrt(1 / (1 / sigma^2) +(1 / tau^2))
pnorm(0, mean=estimate, sd=se)
#[1] 0.4999891

#12. Now use sapply function to change the prior variance 
#from seq(0.005, 0.05, len = 100) and observe how the 
#probability changes by making a plot.

#We had set the prior variance τ to 0.01, reflecting that
# these races are often close.  Change the prior variance 
#to include values ranging from 0.005 to 0.05 and observe
# how the probability of Trump winning Florida changes by 
#making a plot.
mu <- 0
sigma <- results$se
Y <- results$avg

#redefining taus
taus <- seq(0.005, .05, len=100)
# Create a function called `p_calc` that generates `B` and calculates the probability of the spread being less than 0
p_calc <- function(taus) {
  B <- sapply(taus, function(i) (sigma^2 / (sigma^2 + i^2)))
  se <- sapply(taus, function(i) sqrt( 1/ (1/sigma^2 + 1/i^2)))
  exp_value = sapply(B, function(i) (1 - i) * Y)
  pnorm(0, mean=exp_value, sd=se)
}

# Create a vector called `ps` by applying the function `p_calc`
# across values in `taus`
ps <- p_calc(taus)

# Plot `taus` on the x-axis and `ps` on the y-axis
# plot(taus, ps)
trump_percentage <- data.frame(taus, ps)
trump_percentage %>%
  ggplot(aes(taus, ps)) +
  geom_line() +
  ggtitle('Trumps winning chances')