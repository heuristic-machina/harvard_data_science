#> 2.11 exercises

#>1. What is the sum of the first 100 positive integers? The formula for the sum 
#>of integers 1 through n is n(n + 1)/2.  Define and then use R to compute the 
#>sum of 1 through 100 using the formula. What is the sum?
n <- 100
x <- seq(1, n)
sum(x)
#>[1] 5050

#>2.
n <- 1000
x <- seq(1, n)
sum(x)
#>[1] 500500

#>3.  seq creates a list of numbers and sum adds them up

#>4.Use one line of code to compute the log, in base 10, of the square root of 100.
log(sqrt(100), base=10)
log10(sqrt(100))
