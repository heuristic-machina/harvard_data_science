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
#>good fit but not as good as kernel smoother

mnist_27$train |> mutate(y = factor(y)) |> 
  ggplot(aes(x_1, x_2, fill = y, color = y)) + 
  geom_point(show.legend = FALSE) + 
  stat_ellipse(type = "norm") +
  facet_wrap(~y)


#30.4.3 Regression Trees
library(rpart)
fit <- rpart(margin ~ ., data = polls_2008)



#Exercises 30.6
#1. Create a dataset using the following code:
set.seed(1)
n <- 100
Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))
#Use the caret package to partition into a test and training
# set of equal size. Train a linear model and report the RMSE.
# Repeat this exercise 100 times and make a histogram of the
# RMSEs and report the average and standard deviation. Hint:
# adapt the code shown earlier like this:
#and put it inside a call to replicate.
set.seed(1)
rmse<- replicate(100, {
  y <- dat$y
  test_index <- createDataPartition(y,
                                    times = 1,
                                    p = 0.5,
                                    list = FALSE)
  train_set <- dat |> slice(-test_index)
  test_set <- dat |> slice(test_index)
  fit <- lm(y ~ x, data = train_set)
  y_hat <- fit$coef[1] + fit$coef[2]*test_set$x
  sqrt(mean((y_hat - test_set$y)^2))
})
mean(rmse)
#1 2.485441
sd(rmse)
#[1] 0.1316324

#2. Now we will repeat the above but using larger datasets.
# Repeat exercise 1 but for datasets with 
#n <- c(100, 500, 1000, 5000, 10000). Save the average and 
#standard deviation of RMSE from the 100 repetitions. 
#Hint: use the sapply or map functions.
set.seed(1)
n <- c(100, 500, 1000, 5000, 10000)
res <- sapply(n, function(n) {
  Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
  dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
    data.frame() %>% setNames(c("x", "y"))
  rmse<- replicate(100, {
    y <- dat$y
    test_index <- createDataPartition(y,
                                      times = 1,
                                      p = 0.5,
                                      list = FALSE)
    train_set <- dat |> slice(-test_index)
    test_set <- dat |> slice(test_index)
    fit <- lm(y ~ x, data = train_set)
    y_hat <- fit$coef[1] + fit$coef[2]*test_set$x
    sqrt(mean((y_hat - test_set$y)^2))
  })
  c(avg=mean(rmse), sd=sd(rmse))
})
res
#         [,1]      [,2]      [,3]      [,4]      [,5]
#avg 2.4812746 2.6821855 2.8174015 2.6447523 2.6899310
#sd  0.1276984 0.1527809 0.1992136 0.1487669 0.1942449

#3. Describe what you observe with the RMSE as the size of 
#the dataset becomes larger.

#On average, the RMSE does not change much as n gets larger, 
#while the variability of RMSE does decrease.