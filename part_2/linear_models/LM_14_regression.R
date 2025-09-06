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

#14.16 Exercises

#1. Load the GaltonFamilies data from the HistData. The children in 
#each family are listed by gender and then by height. Create a dataset 
#called galton_heights by picking a male and female at random.

library(tidyverse)
library(HistData)
library(gridExtra)
data('GaltonFamilies')
set.seed(1951)
galton_heights <- GaltonFamilies

#2. Make a scatterplot for heights between mothers and daughters, 
#mothers and sons, fathers and daughters, and fathers and sons.
fs<-filter(galton_heights, gender=="male") |> 
  select(father, childHeight) |>
  rename(son = childHeight)
fd<-filter(galton_heights, gender=="female") |> 
  select(father, childHeight) |> 
  rename(daughter=childHeight)
md<-filter(galton_heights, gender=="female") |> 
  select(mother, childHeight) |> 
  rename(daughter=childHeight)
ms<-filter(galton_heights, gender=="male") |> 
  select(mother, childHeight) |> 
  rename(son=childHeight)
statfs<- fs |> summarize(
  mean(father), 
  sd(father), 
  mean(son), 
  sd(son))
statfd<- fd |> summarize(
  mean(father), 
  sd(father), 
  mean(daughter), 
  sd(daughter))
statmd<- md |> summarize(
  mean(mother), 
  sd(mother), 
  mean(daughter), 
  sd(daughter))
statms<- ms |> summarize(
  mean(mother), 
  sd(mother), 
  mean(son), 
  sd(son))
fsplot<-fs |> 
  ggplot(aes(father, son)) + 
  geom_point(alpha=0.5)
fdplot<-fd |> 
  ggplot(aes(father, daughter)) + 
  geom_point(alpha=.5)
mdplot<-md |> 
  ggplot(aes(mother, daughter)) + 
  geom_point(alpha=.5)
msplot<-ms |> 
  ggplot(aes(mother, son)) + 
  geom_point(alpha=.5)
#Four scatterplots marking the heights between each parent gender 
#and child gender#.
grid.arrange(mdplot, msplot, fdplot, fsplot,nrow=2)

#3. Compute the correlation in heights between mothers and daughters, 
#mothers and sons, fathers and daughters, and fathers and sons.
fs |> summarise(r=cor(father, son)) |> pull(r)
#[1] 0.3923835
fd |> summarise(r=cor(father, daughter)) |> pull(r)
#[1] 0.428433
ms |> summarise(r=cor(mother, son)) |> pull(r)
#[1] 0.323005
md |> summarise(r=cor(mother, daughter)) |> pull(r)
#[1] 0.3051645