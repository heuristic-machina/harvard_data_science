#Machine Learning 26: Evaluation Metrics


#F_1 score predicted height being male or female
y_hat <- factor(ifelse(x > 62, "Male", "Female"),
                levels(test_set$sex))

cutoff <- seq(61, 70)
F_1 <- sapply(cutoff, function(x){
  y_hat <- factor(ifelse(train_set$height > x,
                         "Male", "Female"),
                  levels(test_set$sex))
  F_meas(data = y_hat, reference = factor(train_set$sex))
})

par(mar = c(5, 5, 4, 2))
plot(cutoff, F_1, type = "b",
     pch = 19, col = "firebrick",
     xlab = "Tested Heights", ylab = "Predicted F_1 Score")

#the plot shows the best F_1 score
max(F_1)
#> [1] 0.647
#> which this score can be used to find the best height predictor
best_cutoff <- cutoff[which.max(F_1)]
best_cutoff
#> [1] 66
#> 
#> now best_cutoff can be used to verify in our test dataset
y_hat <- ifelse(test_set$height > best_cutoff,
                "Male", "Female") |> 
  factor(levels = levels(test_set$sex))
sensitivity(data = y_hat, reference = test_set$sex)
#> [1] 0.63
specificity(data = y_hat, reference = test_set$sex)
#> [1] 0.833

