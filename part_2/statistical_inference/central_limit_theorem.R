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

#pnorm(q, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)
#now pnorm is used to find probability within 1% of p now that we have
#the standard error

pnorm(0.01/se)-pnorm(-0.01/se)
#[1] 0.07971926


