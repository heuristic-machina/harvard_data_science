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