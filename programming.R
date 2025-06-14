#>3 Programming basics:
#>3.1 Conditional expressions
#>Example 1
library(dslabs)
murder_rate <- murders$total / murders$population*100000
ind <- which.min(murder_rate)

if (murder_rate[ind] < 0.5) {
  print(murders$state[ind]) 
} else{
  print("No state has murder rate that low")
}
#> [1] "Vermont"
#> if changed to (murder_rate[ind] < 0.25)
#> [1] "No state has murder rate that low"

#>Example 2
#> ifelse function takes 3 arguments: a logical and 2 possible answers.  If true
#> returns 2nd argument else false returns 3rd argument
a <- 0
ifelse(a > 0, 1/a, NA)
#> [1] NA

#>Example 3 ifelse vector
a <- c(0, 1, 2, -4, 5)
result <- ifelse(a > 0, 1/a, NA)
#>[1]  NA 1.0 0.5  NA 0.2

#>Example 4 changing NA entries in na_example data frame with '0' using ifelse
no_nas <- ifelse(is.na(na_example), 0, na_example) 
sum(is.na(no_nas))
#> [1] 0

#>Example 5 any() and all()
#>any() returns true if any value in a vector is true
#>all() returns true if all values in vector are true

z <- c(TRUE, TRUE, FALSE)
any(z)
#> [1] TRUE
all(z)
#> [1] FALSE