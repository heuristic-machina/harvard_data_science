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

#>4.11.9
#>Create a new data frame called no_south that removes states from the South region. How 
#>many states are in this category? You can use the function nrow for this.
#>nrow and ncol return number of rows and columns respectively
no_south <- filter(murders, region!='South')
nrow(no_south)
#>[1] 51

#>4.11.10
#>Create a new data frame called murders_nw with only the states from the 
#>Northeast and the West. How many states are in this category?
murders_nw <- filter(murders, region %in% c('Northeast', 'West'))
#>
nrow(murders_nw)
#>[1] 22

#>#>4.11.11
#>11. Suppose you want to live in the Northeast or West and want the murder rate
#> to be less than 1.  Create a table called my_states that contains rows for 
#> states satisfying both the conditions.  Use select to show only the state name,
#>the rate, and the rank.
my_states <- filter(murders, region %in% c('Northeast', 'West'), rate < 1)
my_states
select(my_states, state, rate, rank)