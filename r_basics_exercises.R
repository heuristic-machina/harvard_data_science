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

#>5. Which of the following will always return the numeric value stored in x?
#>5.a
#>log(10^1) (is the natural log of 10)
#> log(1, 10)
#>0 not 1
#>
#>5.b
#>log10(x^10)
#> log(10, 1)
#>is 0 not 1
#>
#>5.c
#>log(exp(x))
#>correct answer
#>
#>5.d
#>exp(log(x, base = 2))
#>is 0 not 2

#>6. Make sure the US murders dataset is loaded. Use the function str to examine
#> the structure of the murders object. Which of the following best describes the
#> variables represented in this data frame?
#> 
#> The state name, the abbreviation of the state name, the state’s region, and 
#> the state’s population and total number of murders for 2010.

#> 7. What are the column names used by the data frame for these five variables?
#> state, abb, region, population, total

#> 8.Use the accessor $ to extract the state abbreviations and assign them to 
#> the object a. What is the class of this object?
a <- murders$abb
a
#>[1] "AL" "AK" "AZ" "AR" "CA" "CO" "CT" "DE" "DC" "FL" "GA" "HI" "ID" "IL" "IN" "IA" "KS"
#>[18] "KY" "LA" "ME" "MD" "MA" "MI" "MN" "MS" "MO" "MT" "NE" "NV" "NH" "NJ" "NM" "NY" "NC"
#>[35] "ND" "OH" "OK" "OR" "PA" "RI" "SC" "SD" "TN" "TX" "UT" "VT" "VA" "WA" "WV" "WI" "WY"
class(a)
#>[1] "character"

#>9. Now use the square brackets to extract the state abbreviations and assign them 
#>to the object b. Use the identical function to determine if a and b are the same.
#>
b <- murders[, 2]
b
#>[1] "AL" "AK" "AZ" "AR" "CA" "CO" "CT" "DE" "DC" "FL" "GA" "HI" "ID" "IL" "IN" "IA" "KS"
#>[18] "KY" "LA" "ME" "MD" "MA" "MI" "MN" "MS" "MO" "MT" "NE" "NV" "NH" "NJ" "NM" "NY" "NC"
#>[35] "ND" "OH" "OK" "OR" "PA" "RI" "SC" "SD" "TN" "TX" "UT" "VT" "VA" "WA" "WV" "WI" "WY"
a == b
#>[1] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
#>[18] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
#>[35] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
identical(a,b)
#>[1] TRUE

#>