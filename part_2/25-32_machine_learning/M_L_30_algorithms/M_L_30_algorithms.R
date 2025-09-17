#Machine Learning 30: Examples of algorithms

#Naive Bayes

#setting the data folds
set.seed(1995)
y <- heights$height
test_index <- createDataPartition(y, times = 1,
                                  p = 0.5, 
                                  list = FALSE)
train_set <- heights |> slice(-test_index)
test_set <- heights |> slice(test_index)

#estimating averages and standard deviations
params <- train_set |> group_by(sex) |>
  summarize(avg = mean(height), sd = sd(height))
params
#> # A tibble: 2 × 3
#>   sex      avg    sd
#>   <fct>  <dbl> <dbl>
#> 1 Female  64.8  4.14
#> 2 Male    69.2  3.57

#estimate prevalence
pi <- train_set |>
  summarize(pi = mean(sex == "Female")) |>
  pull(pi)
pi
#> [1] 0.212

#create actual rule
x <- test_set$height
f0 <- dnorm(x, params$avg[2], params$sd[2])
f1 <- dnorm(x, params$avg[1], params$sd[1])
p_hat_bayes <- f1*pi / (f1*pi + f0*(1 - pi))

#the plot resembles logistic regression
qplot(p_hat_bayes)

#the problem:
#the female population in the dataset does not accurately
#represent the population
y_hat_bayes <- ifelse(p_hat_bayes > 0.5, "Female", "Male")
sensitivity(data = factor(y_hat_bayes),
            reference = factor(test_set$sex))
#> [1] 0.213
specificity(data = factor(y_hat_bayes),
            reference = factor(test_set$sex))
#> [1] 0.967

#the fix:
#controlling prevalence 
#forcing the prediction by 0.5 in the normal density function
#accounting for female population
p_hat_bayes_unbiased <- f1 * 0.5 / (f1 * 0.5 + f0 * (1 - 0.5)) 
y_hat_bayes_unbiased <- ifelse(p_hat_bayes_unbiased > 0.5,
                               "Female", "Male")

#better balanced
sensitivity(factor(y_hat_bayes_unbiased), factor(test_set$sex))
#> [1] 0.693
specificity(factor(y_hat_bayes_unbiased), factor(test_set$sex))
#> [1] 0.832



plot(x, p_hat_bayes_unbiased)
abline(h = 0.5, lty = 2) + 
  abline(v = 67, lty = 2)
#> integer(0)

