#>2.2 Basics

coef_a <- 1
coef_b <- 1
coef_c <- -1

(-coef_b + sqrt(coef_b^2 - 4*coef_a*coef_c))/(2*coef_a)
#> [1] 0.618

#> 2.3 Data types used in the console
#> numeric
a <- 2
class(a)
#>[1] "numeric"

#>2.3.1 data frame
library(dslabs)
class(murders)
#> [1] "data.frame"


#>2.3.2 Examining objects shows the 50 states plus DC with 5 variables
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

#>2.3.3 accessor $ returns specified column data for the entire table
murders$population
#> [1]  4779736   710231  6392017  2915918 37253956  5029196  3574097   897934   601723
#>[10] 19687653  9920000  1360301  1567582 12830632  6483802  3046355  2853118  4339367
#>[19]  4533372  1328361  5773552  6547629  9883640  5303925  2967297  5988927   989415
#>[28]  1826341  2700551  1316470  8791894  2059179 19378102  9535483   672591 11536504
#>[37]  3751351  3831074 12702379  1052567  4625364   814180  6346105 25145561  2763885
#>[46]   625741  8001024  6724540  1852994  5686986   563626

#>2.3.4 vectors refer to objects with several entries
#>function length gives count of entries in the vector
pop <- murders$population
length(pop)
#>[1] 51

#>2.3.5 factors are useful for categorical data using the levels() function
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

#>2.3.6 extract list data using accessor $
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


 
#>2.3.7  matrix(data specified, rows, columns)
mat <- matrix(1:12, 4, 3)
mat
#>      [,1] [,2] [,3]
#> [1,]    1    5    9
#> [2,]    2    6   10
#> [3,]    3    7   11
#> [4,]    4    8   12
#> 
#> specify matrix entries using square brackets
mat[2, 3]
#> [1] 10
#> 
#> selecting a row or column only
mat[2, 3]
#> [1] 10
mat[2, ]
#> [1]  2  6 10
#> 
#> accessing more than one column or row creates a new matrix
mat[, 2:3]
#>      [,1] [,2]
#> [1,]    5    9
#> [2,]    6   10
#> [3,]    7   11
#> [4,]    8   12
#> 
#> subsetting both rows and columns
mat[1:2, 2:3]
#>      [,1] [,2]
#> [1,]    5    9
#> [2,]    6   10
#> 
#> accessing data frame rows and columns with square brackets
murders[25, 1]
#> [1] "Mississippi"
murders[2:3, ]
#>     state abb region population total
#> 2  Alaska  AK   West     710231    19
#> 3 Arizona  AZ   West    6392017   232


#>2.4.1 creating vectors
#>2.4.2 assign names to a vector with use of c concatenation
codes <- c(380, 124, 818)
country <- c("italy","canada","egypt")
names(codes) <- country
codes
#>  italy canada  egypt 
#>    380    124    818
#>    
#>2.4.3 generating sequences
seq(1, 10)
#>  [1]  1  2  3  4  5  6  7  8  9 10
#>  add argument for increments
seq(1, 10, 2)
#> [1] 1 3 5 7 9
class(1:10)
#> [1] "integer"
#> class changes to numeric if sequence includes non-integers
class(seq(1, 10, 0.5))
#> [1] "numeric"


#>2.4.4 subsetting vectors using square brackets
codes[2]
#> canada 
#>    124
#>multi-entry vector as an index
codes[c(1,3)]
#> italy egypt 
#>   380   818
  
#> accessing consecutive elements
codes[1:2]
#>  italy canada 
#>    380    124
  
#>accessing elements by names
codes["canada"]
#> canada 
#>    124
codes[c("egypt","italy")]
#> egypt italy 
#>   818   380



#>2.5 coercion
x <- c(1, "canada", 3)
x
#> [1] "1"      "canada" "3"
class(x)
#> [1] "character"
#> 
#> coerce to character
x <- 1:5
y <- as.character(x)
y
#> [1] "1" "2" "3" "4" "5"
#> 
#> coerce to numeric
as.numeric(y)
#> [1] 1 2 3 4 5 