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

#7. Now assume a Bayesian model that sets the prior distribution
# for Florida’s election night spread mu to follow a normal 
#distribution with expected value theta and standard deviation 
#tao. What are the interpretations of theta and tao?
