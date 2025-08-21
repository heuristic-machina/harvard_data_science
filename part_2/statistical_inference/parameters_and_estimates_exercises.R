#1. Suppose you poll a population in which a proportion p of voters
# are Democrats and 1-p are Republicans. Your sample size is N=25. 
#Consider the random variable S, which is the total number of 
#Democrats in your sample. What is the expected value of this 
#random variable? Hint: It’s a function of p.
n<-25
ev_S<-n*p
ev_S
#[1] 1

#2. What is the standard error of S? Hint: it’s a function of p.
se_S<-sqrt(p(1-p)*(1/25))
se_S
#[1] 0.03919184
#assuming p=.4 and plugging in 25 to the equation reduces to:
# SE[S]=5*sqrt(.4*.6)=2.45

#3. Consider the random variable S/N. This is equivalent to 
#the sample average, which we have been denoting as X. What
# is the expected value of the X? Hint: it’s a function of p. 

#expected value S/N = (#samples*success of binomial)/#samples = successes binomial
ev_sn<-(25*p)/25
ev_sn
#p

#4. What is the standard error of X? Hint: it’s a function of P.
se_rand_x<-sqrt((p*(1-p)*1/25))
se_rand_x
#[1] 0.03919184