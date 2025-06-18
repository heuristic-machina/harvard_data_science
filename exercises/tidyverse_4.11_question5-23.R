#>The Tidyverse exercises 4.11
#>
#>4.11.5
#>Use the function mutate to add a murders column named rate with the per 100,000
#> murder rate and redefine murders as to keep using the variable.

library(dslabs)
data(murders)
View(murders)
murders <- mutate(murders, rate = total/population * 100000)

#>4.11.6 dplyr functions
#>6. If rank(x) gives you the ranks of x from lowest to highest, rank(-x) gives 
#>you the ranks from highest to lowest. Use the function mutate to add a column 
#>rank containing the rank of murder rate from highest to lowest. Make sure you 
#>redefine murders so we can keep using this variable.
library(dplyr)
library(dslabs)
data(murders)
#>create rank column
murders <- mutate(murders, rank = rank(-rate))
#>the above function above did not automatically order the rank
#>use helper function order() 
murders <- murders[order(murders$rank),]

#>4.11.7
#>Use select to show the state names and abbreviations in murders. Do not redefine
#> murders, just show the results.
#> select() is for columns
select(murders, state, abb)

#>4.11.8
#>Use filter() to show the top 5 states with the highest murder rates without mutating.
#>The columns have already been added from the above exercises.
#>filter() is for manipulating row data.   
filter(murders, rank <=5)