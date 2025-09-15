#Machine Learning 28: Smoothing

#n=500 for both Y defined as 1 if digit 7 and 0 if digit 2
#n=500 features x1, ... , xn
#xi=(xi1, xi2)T
library(caret)
library(dslabs)
mnist_27$train |> ggplot(aes(x_1, x_2, color = y)) + geom_point()

#28.5 Local weighted regression
#weighted range of 3 weeks smoothes line

total_days <- diff(range(polls_2008$day))
span <- 21/total_days
fit <- loess(margin ~ day, degree = 1, span = span, data = polls_2008)
polls_2008 |> mutate(smooth = fit$fitted) |>
  ggplot(aes(day, margin)) +
  geom_point(size = 3, alpha = .5, color = "grey") +
  geom_line(aes(day, smooth), color = "red")