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

#>10. With one line of code, use the functions levels and length to determine the 
#>number of regions defined by this dataset.
x <- length(levels(murders$region))
x
#>[1] 4

#>11. The function table takes a vector and returns the frequency of each element.
#>You can quickly see how many states are in each region by applying this function.
#>Use this function in one line of code to create a table of number of states per
#> region.
table(state.region)
#.state.region
#>Northeast         South North Central          West 
#>9            16            12            13 

#>12. Use the function c to create a vector with the average high temperatures 
#>in January for Beijing, Lagos, Paris, Rio de Janeiro, San Juan, and Toronto, 
#>which are 35, 88, 42, 84, 81, and 30 degrees Fahrenheit. Call the object temp.
#>13. Now create a vector with the city names and call the object city.
#>14. Use the names function and the objects defined in the previous exercises to
#> associate the temperature data with its corresponding city.
cities <- c("beijing", "lagos", "paris", "rio de janeiro", "san juan", "toronto")
temp <- c(35, 88, 42, 84, 81, 30)
names(cities) <- temp
cities
#>35               88               42               84               81 
#>"beijing"          "lagos"          "paris" "rio de janeiro"       "san juan" 
#>30 
#>"toronto"

#>15. Use the [ and : operators to access the temperature of the first three cities
#> on the list.
cities[1:3]
#>       35        88        42 
#>"beijing"   "lagos"   "paris" 

#>16. Use the [ operator to access the temperature of Paris and San Juan.
cities[c(3,5)]
#>42         81 
#>"paris" "san juan"

#>17. Use the : operator to create a sequence of numbers 12, 13 14,...,73.
seq(1, 73)
#>[1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28
#>[29] 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56
#>[57] 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73

#>18. Create a vector containing all the positive odd numbers smaller than 100.
seq(1, 100, 2)
#>[1]  1  3  5  7  9 11 13 15 17 19 21 23 25 27 29 31 33 35 37 39 41 43 45 47 49 51 53 55
#>[29] 57 59 61 63 65 67 69 71 73 75 77 79 81 83 85 87 89 91 93 95 97 99

#>19.Create a vector of numbers that starts at 6, does not pass 55, and adds numbers
#>in increments of 4/7: 6, 6 + 4/7, 6 + 8/7, and so on. How many numbers does the
#>list have? Hint: use seq and length.
seq(6, 55, 4/7)
#>[1]  6.000000  6.571429  7.142857  7.714286  8.285714  8.857143  9.428571 10.000000
#>[9] 10.571429 11.142857 11.714286 12.285714 12.857143 13.428571 14.000000 14.571429
#>[17] 15.142857 15.714286 16.285714 16.857143 17.428571 18.000000 18.571429 19.142857
#>[25] 19.714286 20.285714 20.857143 21.428571 22.000000 22.571429 23.142857 23.714286
#>[33] 24.285714 24.857143 25.428571 26.000000 26.571429 27.142857 27.714286 28.285714
#>[41] 28.857143 29.428571 30.000000 30.571429 31.142857 31.714286 32.285714 32.857143
#>[49] 33.428571 34.000000 34.571429 35.142857 35.714286 36.285714 36.857143 37.428571
#>[57] 38.000000 38.571429 39.142857 39.714286 40.285714 40.857143 41.428571 42.000000
#>[65] 42.571429 43.142857 43.714286 44.285714 44.857143 45.428571 46.000000 46.571429
#>[73] 47.142857 47.714286 48.285714 48.857143 49.428571 50.000000 50.571429 51.142857
#>[81] 51.714286 52.285714 52.857143 53.428571 54.000000 54.571429
length(seq(6, 55, 4/7))
#>[1] 86

#>20.What is the class of the following object a <- seq(1, 10, 0.5)?
class(seq(1, 10, 0.5))
#>[1] "numeric"

#>21. What is the class of the following object a <- seq(1, 10)?
class(seq(1, 10))
#>[1] "integer"

#>22. The class of class(a<-1) is numeric, not integer. R defaults to numeric 
#>and to force an integer, you need to add the letter L. Confirm that the class
#>of 1L is integer.
class(a<-1L)
#>[1] "integer"
class(a<-1)
#>[1] "numeric"

#>23. Define the following vector:
x <- c("1", "3", "5")
#>and coerce it to get integers.
x <- c("1", "3", "5")
y <- as.integer(x)
y
#>[1] 1 3 5