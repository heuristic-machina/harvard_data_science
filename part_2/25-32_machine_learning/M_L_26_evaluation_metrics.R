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

#Exercises 26.9 

library(lubridate)
dat <- mutate(reported_heights,
              date_time = ymd_hms(time_stamp)) |>
  filter(date_time >= make_date(2016, 01, 25) & 
           date_time < make_date(2016, 02, 1)) |>
  mutate(type = ifelse(day(date_time) == 25 &
                         hour(date_time) == 8 & 
                         between(minute(date_time), 15, 30),
                       "inclass", "online")) |>
  select(sex, type)
x <- dat$type
y <- factor(dat$sex, c("Female", "Male"))

#1. Show summary statistics that indicate that the type is 
#predictive of sex.

summary_table<-table(dat$type, dat$sex)
summary_table

#           Female Male
#inclass     26   13
#online      42   69

#Axion:
chisq.test(summary_table)
#Pearson's Chi-squared test with Yates' continuity correction
#data:  summary_table
#X-squared = 8.5502, df = 1, p-value = 0.003455

#p-value < 0.05 concluding type and sex are not independent
#i.e. type is predictive of sex