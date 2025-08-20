#>Programming Namespaces 3,3
search()
#> [1] ".GlobalEnv"        "package:dplyr"     "package:dslabs"    "tools:rstudio"    
#>[5] "package:stats"     "package:graphics"  "package:grDevices" "package:utils"    
#>[9] "package:datasets"  "package:methods"   "Autoloads"         "package:base" 
#>
#>using double colons(::) forces selected namespace:
#>stats::filter or if wanting to use dplyr filter is dply::filter
#>double ?? displays all packages with the same name functions for example:
#>??filter 

#>Programming For-Loops 3.4
#>Compute various values of n in the sum of series using for-loops
compute_s_n <- function(n) { 
  sum(1:n)
}
m <- 25
s_n <- vector(length = m) # create an empty vector
for (n in 1:m) {
  s_n[n] <- compute_s_n(n)
}
#>each iteration of n is stored in the nth entry of s_n allowing plot creation:
n <- 1:m
plot(n, s_n) 

#>Programming Vectorization and functionals
#>Functionals apply the same function to each entry in a vector, matrix, data frame, 
#>or list.  The sapply() behaves like a for-loop and is more commonly used in r programming
n <- 1:25
s_n <- sapply(n, compute_s_n)
#>produces identical plot as the for-loop
plot(n, s_n)