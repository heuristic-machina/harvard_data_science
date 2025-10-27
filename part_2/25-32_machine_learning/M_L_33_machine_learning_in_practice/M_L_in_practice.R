#Machine Learning 33:  Machine Learning in Practice

#Exercises 33.8

library(ggplot2)
library(randomForest)
set.seed(1)
n <- 100
Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

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

#1. In the exercises in Chapter 31 we saw that changing maxnodes 
#or nodesize in the randomForest function improved our estimate. 
#Let’s use the train function to help us pick these values. From 
#the caret manual we see that we can’t tune the maxnodes parameter
#or the nodesize argument with randomForest, so we will use the 
#Rborist package and tune the minNode argument. Use the train 
#function to try values minNode <- seq(5, 250, 25). See which 
#value minimizes the estimated RMSE.

library(caret)
library(Rborist)
library(MASS)
library(dplyr)
library(ggplot2)

set.seed(1)
n <- 100
Sigma <- 9 * matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = n, mu = c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

# Define tuning grid for minNode
tune_grid <- expand.grid(predFixed = 1, minNode = seq(5, 250, 25))

# Train model using caret and Rborist
set.seed(1)
rf_rborist <- train(
  y ~ x,
  data = dat,
  method = "Rborist",
  tuneGrid = tune_grid,
  trControl = trainControl(method = "cv", number = 5),
  metric = "RMSE"
)

# View results
print(rf_rborist)
plot(rf_rborist)

# Best minNode value
best_minNode <- rf_rborist$bestTune$minNode
best_rmse <- min(rf_rborist$results$RMSE)

cat("Best minNode:", best_minNode, "\n")
cat("Minimum estimated RMSE:", round(best_rmse, 4), "\n")
