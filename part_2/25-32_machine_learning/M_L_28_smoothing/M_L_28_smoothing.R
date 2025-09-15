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

#2. Plot the smooth estimates against day of the year, all on 
#the same plot but with different colors.

span <- 60 / as.numeric(diff(range(dat$date)))
fit <- dat %>%
  mutate(x = as.numeric(date)) %>%
  loess(deaths ~ x, data = ., span = span, degree = 1)

dat %>% 
  mutate(smooth = predict(fit, as.numeric(date)),
         day = yday(date),
         year = as.character(year(date))) %>%
  ggplot(aes(day, smooth, col = year)) +
  geom_line(lwd = 2)

#3. Suppose we want to predict 2s and 7s in our mnist_27 dataset
# with just the second covariate. Can we do this? On first 
#inspection it appears the data does not have much predictive 
#power. In fact, if we fit a regular logistic regression, the 
#coefficient for x_2 is not significant!
library(broom)
library(dslabs)
mnist_27$train |> 
  glm(y ~ x_2, family = "binomial", data = _) |> 
  tidy()

# A tibble: 2 × 5
#term           estimate   std.error statistic  p.value
#<chr>            <dbl>     <dbl>     <dbl>   <dbl>
#1 (Intercept)   -0.123     0.251    -0.491   0.623
#2 x_2            0.410     0.835     0.491   0.623

qplot(x_2, y, data = mnist_27$train)

#Fit a loess line to the data above and plot the results. 
#Notice that there is predictive power, except the conditional 
#probability is not linear.
mnist_27$train %>% 
  mutate(y = ifelse(y=="7", 1, 0)) %>%
  ggplot(aes(x_2, y)) + 
  geom_smooth(method = "loess")