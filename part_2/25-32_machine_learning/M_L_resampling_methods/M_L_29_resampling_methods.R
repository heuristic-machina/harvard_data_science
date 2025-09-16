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