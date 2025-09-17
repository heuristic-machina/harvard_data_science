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


#30.3.3 Quadratic discriminant analysis MNIST dataset
params_27<- mnist_27$train |>
  group_by(y) |>
  summarize(avg_1=mean(x_1), avg_2=mean(x_2),
            sd_1=sd(x_1), sd_2=sd(x_2),
            r=cor(x_1, x_2))
params_27
## A tibble: 2 × 6
#     y     avg_1   avg_2   sd_1    sd_2     r
#   <fct>   <dbl>    <dbl>  <dbl>    <dbl>   <dbl>
#1    2     0.136   0.287   0.0670  0.0600  0.415
#2    7     0.238   0.290   0.0745  0.104   0.468



#Here we have 2 predictors and had to compute 4 means, 
#4 SDs, and 2 correlations. Notice that if we have 10 
#predictors, we have 45 correlations for each class.

#as number of predictors increase can lead to overfitting
# K x p(p-1)/2 

#quadratic discriminant takes the response group factor
#and non factor discriminators x_1 and x_2 shown as '.' 
train_qda <- MASS::qda(y ~ ., data = mnist_27$train)
y_hat <- predict(train_qda, mnist_27$test)$class

confusionMatrix(y_hat, mnist_27$test$y)$overall["Accuracy"] 
#> Accuracy 
#>    0.815
#>    
#>good fit but not as good as kernal smoother

mnist_27$train |> mutate(y = factor(y)) |> 
  ggplot(aes(x_1, x_2, fill = y, color = y)) + 
  geom_point(show.legend = FALSE) + 
  stat_ellipse(type = "norm") +
  facet_wrap(~y)


