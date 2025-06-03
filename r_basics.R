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

#>str function names() gives the column names
names(murders)
#>[1] "state"      "abb"        "region"     "population" "total"

#>accessor $ returns specified column data for the entire table
murders$population
#> [1]  4779736   710231  6392017  2915918 37253956  5029196  3574097   897934   601723
#>[10] 19687653  9920000  1360301  1567582 12830632  6483802  3046355  2853118  4339367
#>[19]  4533372  1328361  5773552  6547629  9883640  5303925  2967297  5988927   989415
#>[28]  1826341  2700551  1316470  8791894  2059179 19378102  9535483   672591 11536504
#>[37]  3751351  3831074 12702379  1052567  4625364   814180  6346105 25145561  2763885
#>[46]   625741  8001024  6724540  1852994  5686986   563626

#>vectors refer to objects with several entries
#>function length gives count of entries in the vector
pop <- murders$population
length(pop)
#>[1] 51

#>factors are useful for categorical data using the levels() function
class(murders$region)
#> [1] "factor"
#> categories save space vs character data types
levels(murders$region)
#> [1] "Northeast"     "South"         "North Central" "West"
#> factors can be reordered based on the sum
region <- murders$region
value <- murders$total
region <- reorder(region, value, FUN = sum)
levels(region)
#> [1] "Northeast"     "North Central" "West"          "South"

#>extract list data using accessor $
record <- list(name = "John Doe",
               student_id = 1234,
               grades = c(95, 82, 91, 97, 93),
               final_grade = "A")
class(record)
#> [1] "list"
record$student_id
#> [1] 1234
#> nameless lists accessed with index notation
record2 <- list("John Doe", 1234)
record2[[1]]
#> [1] "John Doe"
 