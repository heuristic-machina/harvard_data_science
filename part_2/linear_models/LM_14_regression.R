#Linear Model 14: Regression

library(tidyverse)
library(HistData)

set.seed(1983)
galton_heights <- GaltonFamilies |>
  filter(gender == "male") |>
  group_by(family) |>
  sample_n(1) |>
  ungroup() |>
  select(father, childHeight) |>
  rename(son = childHeight)

galton_heights |> 
  summarize(mean(father), sd(father), mean(son), sd(son))
#> # A tibble: 1 × 4
#>   `mean(father)` `sd(father)` `mean(son)` `sd(son)`
#>            <dbl>        <dbl>       <dbl>     <dbl>
#> 1           69.1         2.55        69.2      2.71
#> galton_heights |> ggplot(aes(father, son)) + 
geom_point(alpha = 0.5)

#correlation coefficient
#average of the product of the standardized values

#14.3 Conditional Expectations
#father son strata boxplot showing median, iqr, 25th, 7th quartile, 
#and outlier anomolies
conditional_avg <- galton_heights |> 
  filter(round(father) == 72) |>
  summarize(avg = mean(son)) |> 
  pull(avg)
conditional_avg
#> [1] 70.5
galton_heights |> mutate(father_strata = factor(round(father))) |> 
  ggplot(aes(father_strata, son)) + 
  geom_boxplot() + 
  geom_point()


#14.4 Regression Line
mu_x <- mean(galton_heights$father)
mu_y <- mean(galton_heights$son)
s_x <- sd(galton_heights$father)
s_y <- sd(galton_heights$son)
r <- cor(galton_heights$father, galton_heights$son)

galton_heights |> 
  ggplot(aes(father, son)) + 
  geom_point(alpha = 0.5) +
  geom_abline(slope = r * s_y/s_x, intercept = mu_y - r * s_y/s_x * mu_x) 

#the regression formula implies when standardizing the variables:
#subtracting the average and dividing the standard deviation, then
#regression line has intercept 0 and a slope equal to the correlation rho
#the plot above can then be input as the following:
galton_heights |> 
  ggplot(aes(scale(father), scale(son))) + 
  geom_point(alpha = 0.5) +
  geom_abline(intercept = 0, slope = r) 
