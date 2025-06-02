coef_a <- 1
coef_b <- 1
coef_c <- -1

(-coef_b + sqrt(coef_b^2 - 4*coef_a*coef_c))/(2*coef_a)
#> [1] 0.618

#> Data types used in the console
#> numeric
a <- 2
class(a)
#>[1] "numeric"

#>data frame
library(dslabs)
class(murders)
#> [1] "data.frame"


#>Examining objects shows the 50 states plus DC with 5 variables
str(murders)
#>'data.frame':	51 obs. of  5 variables:
#>$ state     : chr  "Alabama" "Alaska" "Arizona" "Arkansas" ...
#>$ abb       : chr  "AL" "AK" "AZ" "AR" ...
#>$ region    : Factor w/ 4 levels "Northeast","South",..: 2 4 4 2 4 4 1 2 2 2 ...
#>$ population: num  4779736 710231 6392017 2915918 37253956 ...
#>$ total     : num  135 19 232 93 1257 ...

#>head() shows the first 6 lines
head(murders)