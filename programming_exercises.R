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

#>3.6.3
#>3. The function nchar tells you how many characters long a character vector is.
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