#>2.6 Not available
x <- c("1", "b", "3")
as.numeric(x)
#> Warning: NAs introduced by coercion
#> [1]  1 NA  3


#>2.7 Sorting 
#>sort() sorts in increasing order.  Must use with order() to make sensible
library(dslabs)
sort(murders$total)
#>  [1]    2    4    5    5    7    8   11   12   12   16   19   21   22
#> [14]   27   32   36   38   53   63   65   67   84   93   93   97   97
#> [27]   99  111  116  118  120  135  142  207  219  232  246  250  286
#> [40]  293  310  321  351  364  376  413  457  517  669  805 1257


#>2.7.2 order
ind <- order(murders$total) 
murders$abb[ind] 
#>  [1] "VT" "ND" "NH" "WY" "HI" "SD" "ME" "ID" "MT" "RI" "AK" "IA" "UT"
#> [14] "WV" "NE" "OR" "DE" "MN" "KS" "CO" "NM" "NV" "AR" "WA" "CT" "WI"
#> [27] "DC" "OK" "KY" "MA" "MS" "AL" "IN" "SC" "TN" "AZ" "NJ" "VA" "NC"
#> [40] "MD" "OH" "MO" "LA" "IL" "GA" "MI" "PA" "NY" "FL" "TX" "CA"
#> this formula is tweaked in vector arithmetic 2.8.2 (two vectors) 
#> for murders per every 100000 giving a more accurate representation of safety
#> than the above example


#>2.7.3 max and which.max
max(murders$total)
#> [1] 1257
#> which.max
i_max <- which.max(murders$total)
murders$state[i_max]
#> [1] "California"

#>2.7.4 rank 
x <- c(31, 4, 15, 92, 65)
rank(x)
#> [1] 3 1 2 5 4


#>2.8 vector arithmetic 
#>2.8.1 rescaling a vector by multiplication
inches <- c(69, 62, 66, 70, 70, 73, 67, 73, 67, 70)
inches * 2.54
#>  [1] 175 157 168 178 178 185 170 185 170 178
#>2.8.1 rescaling a vector by subtraction
inches - 69
#>  [1]  0 -7 -3  1  1  4 -2  4 -2  1

#>2.8.2 two vectors
murder_rate <- murders$total / murders$population * 100000
murders$abb[order(murder_rate)]
#>  [1] "VT" "NH" "HI" "ND" "IA" "ID" "UT" "ME" "WY" "OR" "SD" "MN" "MT"
#> [14] "CO" "WA" "WV" "RI" "WI" "NE" "MA" "IN" "KS" "NY" "KY" "AK" "OH"
#> [27] "CT" "NJ" "AL" "IL" "OK" "NC" "NV" "VA" "AR" "TX" "NM" "CA" "FL"
#> [40] "TN" "PA" "AZ" "GA" "MS" "MI" "DE" "SC" "MD" "MO" "LA" "DC"

#>2.8.3 two vectors of unequal length causes recycling (warning w/no error)
x <- c(1, 2, 3)
y <- c(10, 20, 30, 40, 50, 60, 70)
x + y
#> Warning in x + y: longer object length is not a multiple of shorter
#> object length
#> [1] 11 22 33 41 52 63 71


#>2.9 indexing
#>2.9.1 subsetting with logicals
#>finds if states have a murder rate of less than .71
library(dslabs)
murder_rate <- murders$total / murders$population * 100000
ind <- murder_rate < 0.71
murders$state[ind]
#>
#>logical vectors get coerced to numeric with TRUE coded as 1 and FALSE as 0 using
#>sum
sum(ind)
#> [1] 5

#>2.9.2 logical operators
#>refresher
TRUE & TRUE
#> [1] TRUE
TRUE & FALSE
#> [1] FALSE
FALSE & FALSE
#> [1] FALSE
#> finding a western state with murder rate at most 1
west <- murders$region == "West"
safe <- murder_rate <= 1
ind <- safe & west
murders$state[ind]
#> [1] "Hawaii"  "Idaho"   "Oregon"  "Utah"    "Wyoming"

#>2.9.3 which
#>finding the murder rate of california
ind <- which(murders$state == "California")
murder_rate[ind]
#> [1] 3.37

#>2.9.4 match
#>finds out the murder rates for several states by the index
ind <- match(c("New York", "Florida", "Texas"), murders$state)
ind
#> [1] 33 10 44
murder_rate[ind]
#> [1] 2.67 3.40 3.20

#>2.9.5 %in%
#>finding if each element of a first vector is in a second
c("Boston", "Dakota", "Washington") %in% murders$state
#> [1] FALSE FALSE  TRUE
#> 
#> similarity of match and which producing same index but in different order
match(c("New York", "Florida", "Texas"), murders$state)
#> [1] 33 10 44
which(murders$state %in% c("New York", "Florida", "Texas"))
#> [1] 10 33 44