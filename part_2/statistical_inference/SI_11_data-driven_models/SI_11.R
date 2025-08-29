#Statistical Inference 11: Data-driven models

#11.3 Exercises

#dataset to answer exercises
library(dslabs)
x <- heights |> filter(sex == "Male") |>
  pull(height)

#1. Mathematically speaking, x is our population. Using the urn 
#analogy, we have an urn with the values of x in it. What are the
# average and standard deviation of our population?
mean(x)
#[1] 69.31475
sd(x)
#[1] 3.611024

#2. Call the population average computed above mu and the 
#standard deviation sd. Now take a sample of size 50, with 
#replacement, and construct an estimate for mu and sd.

set.seed(1)
n<-50
X<-sample(x, n, replace = TRUE)
mean(X)
#[1] 70.47293
sd(X)
#[1] 3.426742

#3. What does the theory tell us about the sample average X 
#and how it is related to mu?

#It is a random variable with an expected value mu and standard
#error sd/sqrt(samplesize)