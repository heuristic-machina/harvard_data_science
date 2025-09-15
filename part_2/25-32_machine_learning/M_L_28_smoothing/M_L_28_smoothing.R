#Machine Learning 28: Smoothing

#n=500 for both Y defined as 1 if digit 7 and 0 if digit 2
#n=500 features x1, ... , xn
#xi=(xi1, xi2)T
library(caret)
library(dslabs)
mnist_27$train |> ggplot(aes(x_1, x_2, color = y)) + geom_point()

#28.5 Local weighted regression
#weighted range of 3 weeks smooths line
#default degrees=2 is for parabolas resulting in more noise

total_days <- diff(range(polls_2008$day))
span <- 21/total_days
fit <- loess(margin ~ day, degree = 1, span = span, data = polls_2008)
polls_2008 |> mutate(smooth = fit$fitted) |>
  ggplot(aes(day, margin)) +
  geom_point(size = 3, alpha = .5, color = "grey") +
  geom_line(aes(day, smooth), color = "red")

#Exercises 28.7
#1. The dslabs package provides the following dataset with 
#mortality counts for Puerto Rico for 2015-2018.

#Remove data from before May 2018, then use the loess function
# to obtain a smooth estimate of the expected number of deaths 
#as a function of date. Plot this resulting smooth function. 
#Make the span about two months long.
library(dslabs)
head(pr_death_counts)

dat<-pr_death_counts %>% filter(date <= "2018-05-01")
  
span <- 60 / as.numeric(diff(range(dat$date)))
fit <- dat %>% mutate(x = as.numeric(date)) %>%
  loess(deaths ~ x, data = ., span = span, degree = 1)

dat %>%
  mutate(smooth = predict(fit, as.numeric(date))) %>%
  ggplot() +
  geom_point(aes(date, deaths)) +
  geom_line(aes(date, smooth), lwd = 2, col = 2)