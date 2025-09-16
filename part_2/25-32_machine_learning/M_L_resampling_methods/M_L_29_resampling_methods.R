#Machine Learning 29: Resampling Methods


#k-nearest neighbors
#1.define distance between all observations based on the features
#2.estimate p(x) using mathBfx0 identifying k nearest (neighborhood)
#3.take average of ys associated with p(x)
#larger ks result in smoother estimates

#algorithm implementation
library(dslabs)
library(caret)
knn_fit <- knn3(y ~ .,
                data = mnist_27$train, k = 5)


#predict probability of each class x_1 and x_2
y_hat_knn <- predict(knn_fit,
                     mnist_27$test, type = "class")
confusionMatrix(y_hat_knn,
                mnist_27$test$y)$overall["Accuracy"]
#> Accuracy 
#>    0.815
#>    
qplot(y_hat_knn, data = mnist_27$test)


y_hat_knn <- predict(knn_fit, mnist_27$train, type = "class")
confusionMatrix(y_hat_knn,
                mnist_27$train$y)$overall["Accuracy"]
#> Accuracy 
#>     0.86

y_hat_knn <- predict(knn_fit, mnist_27$test, type = "class")
confusionMatrix(y_hat_knn,
                mnist_27$test$y)$overall["Accuracy"]
#> Accuracy 
#>    0.815

#Axion improvement on code clarity with side by side plots
#of knn-5 estimate and true conditional probability
library(dslabs)
library(caret)
library(ggplot2)
library(dplyr)

# Fit kNN model
knn_fit <- knn3(y ~ ., data = mnist_27$train, k = 5)

# Predict probabilities on grid
grid <- mnist_27$true_p
grid$knn_prob <- predict(knn_fit, grid, type = "prob")[,2]  # Probability of class "7"

# Plot kNN estimate
p1 <- ggplot(grid, aes(x = x_1, y = x_2, fill = knn_prob)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0.5) +
  geom_contour(aes(z = knn_prob), breaks = 0.5, color = "black") +
  ggtitle("kNN-5 estimate") +
  theme_minimal()

# Plot true conditional probability
p2 <- ggplot(grid, aes(x = x_1, y = x_2, fill = p)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0.5) +
  geom_contour(aes(z = p), breaks = 0.5, color = "black") +
  ggtitle("True conditional probability") +
  theme_minimal()

# Display side-by-side
library(gridExtra)
grid.arrange(p1, p2, ncol = 2)

  
summary(mnist_27$true_p$x_2)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.0000  0.1528  0.3056  0.3056  0.4583  0.6111 

#Exercises 29.8

#1. Using train() to find accuracy of random generated 
#dataset's subset
set.seed(1996)
n <- 1000
p <- 10000
x <- matrix(rnorm(n * p), n, p)
colnames(x) <- paste("x", 1:ncol(x), sep = "_")
y <- rbinom(n, 1, 0.5) |> factor()

x_subset <- x[ ,sample(p, 100)]

fit <- train(x_subset, y, method = "glm")
fit$results
#parameter  Accuracy       Kappa    AccuracySD    KappaSD
#1 none     0.5052329 0.009740464   0.02993012    0.06042542

#2. t-test y=0 or y=1 to find the most predictive outcome
devtools::install_bioc("genefilter")
install.packages("genefilter")
library(genefilter)
tt <- colttests(x, y)

# a vector of the p-values
pvals<-tt$p.value

#3.predictors statistically significantly associated with y
#p-value cutoff=0.01
ind<-which(pvals<=0.01)
length(ind)
#[1] 108

#4.using significantly signficant index x=length 108 columns 
#rerun cross validation using x
x_subset<-x[,ind]
fit <- train(x_subset, y, method = "glm")
fit$results
#parameter  Accuracy     Kappa    AccuracySD    KappaSD
#1none    0.7547533   0.5088535   0.02094083    0.04247884

#5.cross-validation with kNN
#grid tuning parameters: k=seq(101, 301, 25)
fit <- train(x_subset, y, method = "knn",
             tuneGrid = data.frame(k = seq(101, 301, 25)))
ggplot(fit)

fit$results
#   k       Accuracy     Kappa      AccuracySD    KappaSD
#7 251      0.7366656   0.4690416   0.03328032    0.06331109


#6 We used the entire dataset to select the columns used in 
#the model. This step needs to be included as part of the 
#algorithm. The cross validation was done after this selection.

#7 Re-do cross validation with selection step in cross 
#validation 
#Axion: prevents data leaks from test fold to training folds
#inflates performance

library(caret)
library(genefilter)

# Set up folds
set.seed(1)
folds <- createFolds(y, k = 10)

accuracies <- numeric(length(folds))

for (i in seq_along(folds)) {
  # Split data
  test_idx <- folds[[i]]
  x_train <- x[-test_idx, ]
  y_train <- y[-test_idx]
  x_test <- x[test_idx, ]
  y_test <- y[test_idx]
  
  # Feature selection on training set only
  tt <- colttests(x_train, y_train)
  ind <- which(tt$p.value <= 0.01)
  
  # Subset both train and test sets
  x_train_subset <- x_train[, ind]
  x_test_subset <- x_test[, ind]
  
  # Fit model (e.g., logistic regression)
  fit <- train(x_train_subset, y_train, method = "glm")
  
  # Predict and compute accuracy
  y_hat <- predict(fit, x_test_subset)
  accuracies[i] <- mean(y_hat == y_test)
}

# Final estimate
mean(accuracies)
#[1] 0.487
