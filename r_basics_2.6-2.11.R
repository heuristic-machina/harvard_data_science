#>2.6 Not availables
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