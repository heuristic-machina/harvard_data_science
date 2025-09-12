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

#5. Write a line of code that gives you the standard error se 
#for the problem above for several values of p, specifically 
#for p <- seq(0, 1, length=100). Make a plot of se versus p.
p<- seq(0, 1, length=100)
se<-sqrt(p*(1-p)/25)
plot(p, se)

#6. Copy the code above and put it inside a for-loop to make 
#the plot for N=25, N=100, and N=1000.
for (N in c(25, 100, 1000)) {
  p<-seq(0, 1, length=100)
  SE<-sqrt(p*(1-p)/N)
  plot(p, SE)
}

#7. If we are interested in the difference in proportions, p−(1−p), 
#our estimate is d=X−(1−X). Use the rules we learned about sums 
#of random variables and scaled random variables to derive the 
#expected value of d.

#d=X-(1-X)=X-1+X=2X-1
#ev_p= p-(1-p)= p-1+p=2p-1

#8.What is the standard error of d?
#se_d=2*sqrt((p*(1-p))/n)

#9. If the actual p=.45, it means the Republicans are
# winning by a relatively large margin, since d= -.1 , which
# is a 10% margin of victory. In this case, what is
# the standard error of 2X-1 if we take a sample of N=25?
p<-.45
se<-2*sqrt((p*(1-p))/25)
se
#[1] 0.1989975

#10. Given the answer to 9, which of the following best 
#describes your strategy of using a sample size of N=25?

#b.Our standard error is larger than the difference, so the 
#chances of 2X-1 representing a large margin are not small.
#We should pick a larger sample size
