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