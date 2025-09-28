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

#4 Now repeat exercise 1, but this time make the correlation 
#between x and y larger by changing Sigma like this:
set.seed(1)
n <- 100
Sigma <- 9*matrix(c(1, 0.95, 0.95, 1), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

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
#[1] 0.9078124
sd(rmse)
#[1] 0.05821304

#The base matrix represents a correlation matrix:
#variance of x and y is 1
#correlation between x and y is .95
# 1   .95
# .95   1
#9 multiplies the scale

#generated 100 samples of the bivariate normal distribution with
#this covariance structure.  Then repeatedly splitting the data, fitting
#a linear model, & calculating RMSE on the test set.  The high correlation
#gives a well performing linear model.  Small std dev shows stable
#predictive performance acoss splits

#5. Which of the following best explains why the RMSE in 
#exercise 4 is so much lower than exercise 1:

#When we increase the correlation between x and y, x has
# more predictive power and thus provides a better estimate
# of y. This correlation has a much bigger effect on RMSE than
# n. Large n simply provide us more precise estimates of the 
#linear model coefficients.

#6. Create a dataset using the following code:
set.seed(1)
Sigma <- matrix(c(1, 3/4, 3/4, 3/4, 1, 0, 3/4, 0, 1), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) |>
  data.frame() |> setNames(c("y", "x_1", "x_2"))

#   1   .75   .75
# .75     1     0
# .75     0     1

#Error in MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) : 
#'Sigma' is not positive definite'

#Axion: A matrix is positive definite if all its 
#eigenvalues are positive. For a covariance matrix, this 
#ensures:
#All variables have non-negative variance.
#The matrix defines a valid multivariate normal distribution.
#No linear combination of variables has zero or negative variance.

#softening the matrix 0s to .25 allows all variables to share some
#covariance and the matrix becomes more positive definite
#   1   .75   .75
# .75     1   .25
# .75   .25     1



set.seed(1)
Sigma <- matrix(c(1.0, 0.75, 0.75,
                  0.75, 1.0, 0.25,
                  0.75, 0.25, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))

cor(dat)
#            y       x_1       x_2
#y   1.0000000 0.7287301 0.7096290
#x_1 0.7287301 1.0000000 0.1890924
#x_2 0.7096290 0.1890924 1.0000000

#visualization
# Run PCA (centered and scaled)
pca <- prcomp(dat, center = TRUE, scale. = TRUE)

# Visualize loadings
loadings <- as.data.frame(pca$rotation)
loadings$Variable <- rownames(loadings)

ggplot(loadings, aes(x = PC1, y = PC2, label = Variable)) +
  geom_segment(aes(xend = 0, yend = 0), 
               arrow = arrow(length = unit(0.2, "cm")),
               color = "steelblue") +
  geom_text(vjust = -0.5, size = 5) +
  xlim(-1, 1) + ylim(-1, 1) +
  coord_fixed() +
  labs(title = "PCA Loadings Plot", x = "PC1", y = "PC2") +
  theme_minimal()

#Use the caret package to partition into a test and training 
#set of equal size. Compare the RMSE when using just x_1, just
# x_2, and both x_1 and x_2. Train a linear model and report the
# RMSE.

set.seed(1)
test_index <- createDataPartition(dat$y,
                                  times = 1,
                                  p = 0.5,
                                  list = FALSE)
train_set <- dat %>% slice(-test_index)
test_set <- dat %>% slice(test_index)

fit <- lm(y ~ x_1, data = train_set)
y_hat <- predict(fit, newdata = test_set)
sqrt(mean((y_hat-test_set$y)^2))
#[1] 0.6175662
fit <- lm(y ~ x_2, data = train_set)
y_hat <- predict(fit, newdata = test_set)
sqrt(mean((y_hat-test_set$y)^2))
#[1] 0.5881607
fit <- lm(y ~ x_1 + x_2, data = train_set)
y_hat <- predict(fit, newdata = test_set)
sqrt(mean((y_hat-test_set$y)^2))
#[1] 0.3161433

#lowest RMSE is both covariates since both are highly 
#correlated to y


#Axion heatmap visual
library(ggplot2)
library(reshape2)

# Compute correlation matrix
cor_mat <- cor(dat)

# Melt for ggplot
cor_melt <- melt(cor_mat)

# Plot
ggplot(cor_melt, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1,1), space = "Lab",
                       name = "Correlation") +
  theme_minimal() +
  coord_fixed() +
  labs(title = "Correlation Heatmap", x = "", y = "") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))



#Axion visual of why centering mean at zero is visually helpful
library(MASS)
library(ggplot2)
library(dplyr)

set.seed(1)
Sigma <- matrix(c(1, 0.75, 0.75,
                  0.75, 1, 0.25,
                  0.75, 0.25, 1), 3, 3)

# Centered at origin
dat_centered <- mvrnorm(n = 100, mu = c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2")) %>%
  mutate(type = "Centered")

# Shifted mean
dat_shifted <- mvrnorm(n = 100, mu = c(70, 70, 70), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2")) %>%
  mutate(type = "Shifted")

# Combine
dat_all <- bind_rows(dat_centered, dat_shifted)

# Plot
ggplot(dat_all, aes(x = x_1, y = x_2, color = type)) +
  geom_point(alpha = 0.6) +
  labs(title =
         "Effect of Mean Vector on Multivariate Normal Samples",
       x = "x₁", y = "x₂") +
  theme_minimal()


#7. Repeat exercise 6 but now create an example in which x_1 and x_2 
#are highly correlated:
n <- 1000
Sigma <- matrix(c(1.0, 0.75, 0.75,
                  0.75, 1.0, 0.95,
                  0.75, 0.95, 1.0),
                3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) |>
  data.frame() |> setNames(c("y", "x_1", "x_2"))

cor(dat)
#           y       x_1       x_2
#y   1.0000000 0.7376090 0.7304403
#x_1 0.7376090 1.0000000 0.9514117
#x_2 0.7304403 0.9514117 1.0000000
set.seed(1)
test_index <- createDataPartition(dat$y,
                                  times = 1,
                                  p = 0.5,
                                  list = FALSE)
train_set <- dat %>% slice(-test_index)
test_set <- dat %>% slice(test_index)

fit <- lm(y ~ x_1, data = train_set)
y_hat <- predict(fit, newdata = test_set)
sqrt(mean((y_hat-test_set$y)^2))
#[1] 0.828522
fit <- lm(y ~ x_2, data = train_set)
y_hat <- predict(fit, newdata = test_set)
sqrt(mean((y_hat-test_set$y)^2))
#[1] 0.7530944
fit <- lm(y ~ x_1 + x_2, data = train_set)
y_hat <- predict(fit, newdata = test_set)
sqrt(mean((y_hat-test_set$y)^2))
#[1] 0.8330126

#visualization
# Run PCA (centered and scaled)
pca <- prcomp(dat, center = TRUE, scale. = TRUE)

# Visualize loadings
loadings <- as.data.frame(pca$rotation)
loadings$Variable <- rownames(loadings)

ggplot(loadings, aes(x = PC1, y = PC2, label = Variable)) +
  geom_segment(aes(xend = 0, yend = 0), 
               arrow = arrow(length = unit(0.2, "cm")),
               color = "steelblue") +
  geom_text(vjust = -0.5, size = 5) +
  xlim(-1, 1) + ylim(-1, 1) +
  coord_fixed() +
  labs(title = "PCA Loadings Plot", x = "PC1", y = "PC2") +
  theme_minimal()

#8. Compare the results in 6 and 7 and choose the 
#statement you agree with:

#Adding extra predictors can improve RMSE substantially,
#but not when they are highly correlated with another predictor.

#9 Define the following dataset:
set.seed(2)
make_data <- function(n = 1000, p = 0.5, 
                      mu_0 = 0, mu_1 = 2, 
                      sigma_0 = 1,  sigma_1 = 1){
  
  y <- rbinom(n, 1, p)
  f_0 <- rnorm(n, mu_0, sigma_0)
  f_1 <- rnorm(n, mu_1, sigma_1)
  x <- ifelse(y == 1, f_1, f_0)
  
  test_index <- createDataPartition(y, 
                                    times = 1,
                                    p = 0.5,
                                    list = FALSE)
  
  list(train = data.frame(x = x, y = as.factor(y)) %>%
         slice(-test_index),
       test = data.frame(x = x, y = as.factor(y)) %>%
         slice(test_index))
}
dat <- make_data()

#Note that we have defined a variable x that is predictive of a 
#binary outcome y.
#color=y shows both binary outcomes
dat$train %>% ggplot(aes(x, color = y)) + geom_density()

#10. Repeat the simulation from exercise 1 100 times and compare the 
#average accuracy for each method and notice they give practically 
#the same answer.
set.seed(1)
n <- 100
Sigma <- 9*matrix(c(1.0, 0.5,
                    0.5, 1.0),
                  2, 2)
dat <- MASS::mvrnorm(n = 100,
                     c(69, 69),
                     Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))
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
  y_hat <- predict(fit, newdata = test_set)
  #rmse - a measure of prediction accuracy
  sqrt(mean((y_hat - test_set$y)^2))
})
mean(rmse)
#[1] 2.485441
sd(rmse)
#[1] 0.1316324
hist(rmse, breaks = 20, col = "skyblue",
     main = "RMSE Distribution", xlab = "RMSE")

#11. Generate 25 different datasets changing the difference between
# the two class: delta <- seq(0, 3, len = 25). Plot accuracy versus
# delta.

set.seed(1)
delta <- seq(0, 3, len = 25)
res <- sapply(delta, function(d){
  dat <- make_data(mu_1 = d)
  fit_glm <- dat$train %>%
    glm(y ~ x, family = "binomial", data = .)
  y_hat_glm <- ifelse(predict(fit_glm,
                              dat$test) > 0.5, 1, 0) %>%
    factor(levels = c(0, 1))
  mean(y_hat_glm == dat$test$y)
})
qplot(delta, res)

#12
library(dslabs)
mnist_127$train |> ggplot(aes(x_1, x_2,
                              color = y)) + geom_point()

#Fit QDA using the qda function in the MASS package the create
#a confusion matrix for predictions on the test. Which of the 
#following best describes the confusion matrix:

library(MASS)
library(caret)  # for confusionMatrix()

# Fit QDA model
qda_fit <- MASS::qda(y ~ ., data = mnist_127$train)
# Predict on test
qda_pred <- predict(qda_fit, mnist_127$test)
# Confusion matrix
confusionMatrix(qda_pred$class, mnist_127$test$y)

#3 class: 3x3 table

             Reference
Prediction   
#             1       2       7
#       1     111     9       11
#       2     10      86      21
#       7     21      28      102

# Overall Statistics

#Accuracy : 0.7494          
#95% CI : (0.7038, 0.7912)
#No Information Rate : 0.3559          
#P-Value [Acc > NIR] : <2e-16          

#Kappa : 0.6235
#shows qda model is substantially performing better than 
#random guessing

#Mcnemar's Test P-Value : 0.2429          

#Statistics by Class:

#                     Class: 1 Class: 2 Class: 7
#Sensitivity            0.7817   0.6992   0.7612
#Specificity            0.9222   0.8877   0.8151
#Pos Pred Value         0.8473   0.7350   0.6755
#Neg Pred Value         0.8843   0.8688   0.8710
#Prevalence             0.3559   0.3083   0.3358
#Detection Rate         0.2782   0.2155   0.2556
#Detection Prevalence   0.3283   0.2932   0.3784
#Balanced Accuracy      0.8519   0.7934   0.7881

#negative prediction value:
#Actual 1s= sum(column 1)=142 
#False Negatives, predicted as 2 and 7: 10 + 21 = 31 
#True Negatives, all non-1s (actual 2s and 7s) predicted as 2 or 7: 
#Actual 2s predicted as 2 or 7: 86 + 28= 114 
#Actual 7s predicted as 2 or 7: 21 + 102= 123 
#Total TN = 237 NPV = 237/237+31 = .88

#13. The byClass component returned by the confusionMatrix 
#object provides sensitivity and specificity for each class. 
#Because these terms only make sense when data is binary, each row
#represents sensitivity and specificity when a particular class is
#1 (positives) and the other two are considered 0s (negatives). 
#Based on the values returned by confusionMatrix, which of the following
# is the most common mistake:

#answer:Calling 2s either a 1 or 7.  2 had the highest number of false 
#negatives and the lowest sensitivity

#14 Create a grid of x_1 and x_2 using:
GS <- 150
new_x <- with(mnist_127$train,
              expand.grid(x_1 = seq(min(x_1),
                                    max(x_1),
                                    len = GS),
                          x_2 = seq(min(x_2),
                                    max(x_2),
                                    len = GS)))

#then visualize the decision rule by coloring the regions of the 
#Cartesian plan to represent the label that would be called in that 
#region.

#This adds a label column to new_x, showing which digit (1, 2, or 7)
# QDA would assign to each point in the 2D space.
qda_pred_grid <- predict(qda_fit, newdata = new_x)
new_x$label <- qda_pred_grid$class

#Axion' plot
#geom_tile paints the prediction tiles
#geom_points are the actual train data points
ggplot() +
  geom_tile(data = new_x, aes(x = x_1, y = x_2,
                              fill = label), alpha = 0.4) +
  geom_point(data = mnist_127$train,
             aes(x = x_1, y = x_2, color = y),
             size = 1.5) +
  scale_fill_manual(values = c("1" = "skyblue",
                               "2" = "orange",
                               "7" = "purple")) +
  scale_color_manual(values = c("1" = "blue",
                                "2" = "darkorange",
                                "7" = "darkviolet")) +
  labs(title = "QDA Decision Boundaries on MNIST 1-2-7",
       fill = "Predicted Label",
       color = "Actual Label") +
  theme_minimal()

#lda comparision
#1. fit the model
lda_fit <- lda(y ~ x_1 + x_2, data = mnist_127$train)
#2. predict on the grid
lda_pred_grid <- predict(lda_fit, newdata = new_x)
new_x$label_lda <- lda_pred_grid$class
#plot
ggplot() +
  geom_tile(data = new_x, aes(x = x_1, y = x_2,
                              fill = label_lda),
            alpha = 0.4) +
  geom_point(data = mnist_127$train, aes(x = x_1, y = x_2,
                                         color = y), size = 1.5) +
  scale_fill_manual(values = c("1" = "skyblue",
                               "2" = "orange",
                               "7" = "purple")) +
  scale_color_manual(values = c("1" = "blue",
                                "2" = "darkorange",
                                "7" = "darkviolet")) +
  labs(title = "LDA Decision Boundaries on MNIST 1-2-7",
       fill = "Predicted Label",
       color = "Actual Label") +
  theme_minimal()

#14 Which of the following explains why LDA has worse accuracy:

#LDA separates the space with lines making it too rigid.

#15. Now repeat exercise 13 for kNN with k=31 and compute and 
#compare the overall accuracy for all three methods.

library(class)

# Extract training and test features and labels
train_x <- mnist_127$train[, c("x_1", "x_2")]
train_y <- mnist_127$train$y
test_x  <- mnist_127$test[, c("x_1", "x_2")]
test_y  <- mnist_127$test$y

# Predict using kNN with k = 31
knn_pred <- knn(train = train_x, test = test_x, cl = train_y, k = 31)

# QDA predictions
qda_test_pred <- predict(qda_fit, mnist_127$test)$class

# LDA predictions
lda_test_pred <- predict(lda_fit, mnist_127$test)$class


# Accuracy calculations
mean(qda_test_pred == test_y)  # QDA accuracy
#[1] 0.7493734
mean(lda_test_pred == test_y)  # LDA accuracy
#[1] 0.6290727
mean(knn_pred == test_y)       # kNN accuracy
#[1] 0.7568922

#16 To understand how a simple method like kNN can outperform a model 
#that explicitly tries to emulate Bayes’ rule, explore the conditional
# distributions of x_1 and x_2 to see if the normal approximation holds.
# Generative models can be very powerful, but only when we are able to 
#successfully approximate the joint distribution of predictors conditioned
#on each class.

#Gaussian class-conditional distributions are normal approximations and
#relevant for LDA and QDA.  The workflow:

#combine test data with labels
library(ggplot2)
library(dplyr)

#Ensure its a dataframe
plot_data <- as.data.frame(mnist_127$train)
plot_data <- plot_data[, c('x_1', 'x_2', 'y')]
plot_data$y <- as.factor(plot_data$y)

#Density plot x_1
ggplot(plot_data, aes(x = x_1, fill = y)) +
  geom_density(alpha=.5) +
  labs(title = 'Conditional Density of x_1 by Class',
       x='x_1',
       y = 'Density') +
  theme_minimal()
#Density plot x_2
ggplot(plot_data, aes(x = x_2, fill = y)) +
  geom_density(alpha=.5) +
  labs(title = 'Conditional Density of x_2 by Class',
       x='x_2',
       y = 'Density') +
  theme_minimal()

#QDA assumes each class has its own covariance matrix (flexible)
#LDA assumes equal covariance across classes (restrictive)
#kNN when distributions are skewed, multimodal, or overlapping

#17 Earlier we used logistic regression to predict sex from 
#height. Use kNN to do the same. Use the code described in this 
#chapter to select the F1 measure and plot it against k. Compare 
#to the F1 of about 0.6 we obtained with regression.
library(dslabs)
library(tidyverse)
library(caret)
data('heights')

set.seed(1)
test_index <- createDataPartition(heights$sex,
                                  times=1,
                                  p=.5,
                                  list=FALSE)
test_set <- heights[test_index, ]
train_set <- heights[-test_index, ]

ks <- seq(1, 101, 3)
f1 <- sapply(ks, function(k){
  fit <- knn3(sex ~ height, data = train_set, k=k)
  y_hat <- predict(fit, test_set, type='class') %>%
    factor(levels=levels(train_set$sex))
  F_meas(data=y_hat, reference=test_set$sex)
})
plot(ks, f1)

#18 Create a simple dataset where the outcome grows .75 units on average
#for every increase in a predictor:
n <- 1000
sigma <- 0.25
x <- rnorm(n, 0, 1)
y <- 0.75 * x + rnorm(n, 0, sigma)
dat <- data.frame(x = x, y = y)

#use rpart to fit a regression tree and save the result to fit

library(rpart)
n <- 1000
sigma <- 0.25
set.seed(1, sample.kind = 'Rounding')
x <- rnorm(n, 0, 1)
y <- 0.75 * x + rnorm(n, 0, sigma)
dat <- data.frame(x=x, y=y)
fit1 <- rpart(y ~ ., data=dat)

#19 Plot the final tree so that you can see where the 
#partitions occurred.
library(rpart)
library(rpart.plot)

#fit the tree
fit1 <- rpart(y ~ ., data=dat)
#plot
rpart.plot(fit1, type=2, extra=101, under=TRUE, faclen=0,
           box.palette='Blues', shadow.col = 'gray', nn =TRUE)

#20 Make a scatterplot y vs x along with the predicted values 
#based on the fit

#add predictions
dat$y_hat <- predict(fit1)
#plot
ggplot(dat, aes(x=x)) +
  geom_point(aes(y=y), color='gray60', alpha=0.6) +
  geom_line(aes(y=y_hat), color='blue', size=1.2) +
  labs(title='Regression Tree Fit: y vs x',
       x = 'x', y = 'y / Predicted y') +
  theme_minimal()

#21 Now model with a random forest instead of a regression tree using
#randomForest from the randomForest package, and remake the scatterplot
#with the prediction line

#22 Use the function plot to see if the random forest has converged
#or if we need more trees.

#we use mean square error to visualize how the classification error
#changes as more trees are added.  If its stable or add more to increase
#performance

#traceable error fit
rf_fit <- randomForest(y~x, data=dat, ntree=500, keep.inbag=TRUE)
#plot error vs. number of trees
plot(rf_fit)
#plot flattens at 200 and appears converged
#fit random forest
install.packages('randomForest')
library(randomForest)
rf_fit <- randomForest(y ~ x, data=dat)
#predict
dat$y_hat <- predict(rf_fit)
#plot
ggplot(dat, aes(x=x)) +
  geom_point(aes(y=y), color='gray60', alpha=.6) +
  geom_line(aes(y=y_hat), color='forestgreen', size=1.2) +
  labs(title = 'Random Forest Fit: y vs x',
       x='x', y='y /Predicted y') +
  theme_minimal()

#23 It seems that the default values for the random forest result in
#an estimate that is too flexible (not smooth).  Re-run the random 
#forest but this time with nodesize set at 50 and maxnodes set at 25.
#Remake the plot.
rf_fit <- randomForest(y ~ x, data=dat, nodesize=50)
#predict
dat$y_hat <- predict(rf_fit)
ggplot(dat, aes(x=x)) +
  geom_point(aes(y=y), color='gray60', alpha=.6) +
  geom_line(aes(y=y_hat), color='forestgreen', size=1.2) +
  labs(title = 'Random Forest Fit: y vs x',
       x='x', y='y /Predicted y') +
  theme_minimal()

#as nodesize increases it loses the forest look and approaches
#a linear curve

#overlap plot
rf_5  <- randomForest(y ~ x, data = dat, nodesize = 5)
rf_50 <- randomForest(y ~ x, data = dat, nodesize = 50)

dat$y_hat_5  <- predict(rf_5)
dat$y_hat_50 <- predict(rf_50)

ggplot(dat, aes(x = x)) +
  geom_point(aes(y = y), color = "gray60", alpha = 0.6) +
  geom_line(aes(y = y_hat_5), color = "forestgreen", size = 1.2) +
  geom_line(aes(y = y_hat_50), color = "darkorange", size = 1.2, linetype = "dashed") +
  labs(title = "Random Forest Predictions: nodesize 5 vs 50",
       x = "x", y = "y / Predicted y") +
  theme_minimal()


#24 This dslabs dataset includes the tissue_gene_expression with a 
#matrix x:
library(dslabs)
dim(tissue_gene_expression$x)

#with the gene expression measured on 500 genes for 189 biological
#samples representing seven different tissues. The tissue type is stored
#in tissue_gene_expression$y
table(tissue_gene_expression$y)
#Fit a random forest using the randomForest function in the package
#randomForest. Then use varImp function to see which are the top 10
#most predictive genes. Make a histogram of the reported importance
#to get an idea of the distribution of the importance values.

library(dslabs)
library(randomForest)
library(caret)
library(ggplot2)
data("tissue_gene_expression")

#fit forest model
#predict tissue type
rf_model <- randomForest(x= tissue_gene_expression$x,
                         y= tissue_gene_expression$y,
                         importance=TRUE)
#extract variable importance
#caret's varImp for importance scores

imp <- varImp(rf_model, scale = FALSE)

# Convert to data frame and add gene names
imp_df <- data.frame(Gene = rownames(imp), Importance = imp[, 1])

# Sort and slice top 10
top_genes <- imp_df %>%
  arrange(desc(Importance)) %>%
  slice(1:10)

# Plot
ggplot(top_genes, aes(x = reorder(Gene, Importance), y = Importance)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 10 Predictive Genes by Importance",
       x = "Gene", y = "Importance Score") +
  theme_minimal()
