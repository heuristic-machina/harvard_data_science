#>3.6 Programming Exercises

#>3.6.1. What will this conditional expression return?
x <- c(1,2,-3,4)

if(all(x>0)){
  print("All Postives")
} else{
  print("Not all positives")
}
#> 'Not all positive'

#>3.6.2. Which of the following expressions is always FALSE when at least one entry 
#>of a logical vector x is TRUE?

#> all(x)

#>3.6.3. The function nchar tells you how many characters long a character vector is.
#> Write a line of code that assigns to the object new_names the state abbreviation
#>  when the state name is longer than 8 characters.
new_names <- ifelse(nchar(murders$state)>8, murders$abb, murders$state)
new_names
#> [1] "Alabama"  "Alaska"   "Arizona"  "Arkansas" "CA"       "Colorado" "CT"
#>"Delaware"[9] "DC"       "Florida"  "Georgia"  "Hawaii"   "Idaho"    "Illinois" 
#>"Indiana"  "Iowa"[17] "Kansas"   "Kentucky" "LA"       "Maine"    "Maryland" "MA"
#>"Michigan" "MN"[25] "MS"       "Missouri" "Montana"  "Nebraska" "Nevada"   "NH"
#>"NJ" "NM" [33] "New York" "NC"       "ND"       "Ohio"     "Oklahoma" "Oregon"
#>"PA" "RI"[41] "SC"       "SD"       "TN"       "Texas"    "Utah"     "Vermont"
#>  "Virginia" "WA" [49] "WV"       "WI"       "Wyoming" 

#>3.6.4 Programming Functions
#>Create a function sum_n that for any given value, say n, computes the sum of 
#>the integers from 1 to n (inclusive). Use the function to determine the sum of
#>integers from 1 to 5,000.
sum_n <- function(n) {
  x <- 1:n
  sum(x)
}
sum_n(5000)
#>[1] 12502500

#>3.6.5. Create a function altman_plot that takes two arguments, x and y, and 
#>plots the difference against the sum.
#>the difference `y-x` in the y-axis against the sum `x+y` in the x-axis
altman_plot <- function(x,y) {
  plot(x + y, y - x)
}
altman_plot(8,9)

#>3.6.6 function scope
x <- 8
my_func <- function(y){
  x <- 9
  print(x)
  y + x
}

my_func(x) 
#>(function scoped)
#>[1] 9
#>[1] 17
print(x)
#>(global scoped)

#>3.6.7. Write a function compute_s_n that for any given n computes the sum 
#>Sn = 1^2 + 2^2 + 3^2 + ...n^2
#>Report the value of the sum when n = 10.
compute_s_n <- function(n) {
  x <- (1:n)
  sum(x^2)
}
compute_s_n(10)
#>[1] 385

#>3.6.8 Define an empty numerical vector s_n of size 25 using s_n <- vector
#>("numeric", 25) and store in the results of S1, S2, ...S25 using a for-loop.
#>
#>
#>define function and store in compute_s_n
compute_s_n <- function(n){
  x <- 1:n
  sum(x^2)
}
#>
#> create an empty vector to store results
s_n <- vector("numeric", 25)
#>
#>assign values to n and s_n:
for (i in 25) {
  s_n[i] <- compute_s_n(i)
}
#>create the plot
plot(n, s_n)

#>3.6.9 9. Repeat exercise 8, but this time use sapply.
s_n <- sapply(n, compute_s_n)
plot(n, s_n)

#>3.6.10 create the plot sn vs.n.  Use points defined by n = 1,...,25.
#>same plots from 3.6.8-3.6.9
plot(n, s_n)

#3.6.11 Confirm that the formula for this sum is Sn = n(n + 1)(2n + 1)/6.

#>sum of squares grows roughly like n^3
#>proof using induction
#>Base case (n=1)
#>S1 = 1^2 = 1
#>using the formula:
#>S1 = 1 * (1 + 1) * (2 X 1 + 1) / 6
#>S1 = 1 * 2 * 3 / 6 = 1
#>
identical(s_n, n*(n+1)*(2*n+1)/6)
#>[1] TRUE