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

#>4.11.11
#>11. Suppose you want to live in the Northeast or West and want the murder rate
#> to be less than 1.  Create a table called my_states that contains rows for 
#> states satisfying both the conditions.  Use select to show only the state name,
#>the rate, and the rank.
my_states <- filter(murders, region %in% c('Northeast', 'West'), rate < 1)
my_states
select(my_states, state, rate, rank)

#>4.11.12
#>Repeat the previous exercise, but now instead of creating a new object, show the
#>result and only include the state, rate, and rank columns. Use a pipe |> to do 
#>this in just one line.
my_states |> select(state, rate, rank)

#>4.11.13
#>For exercises 13-19, we will be using the data from the survey collected by
#> the United States National Center for Health Statistics (NCHS). This center has
#>conducted a series of health and nutrition surveys since the 1960’s. Starting 
#>in 1999, about 5,000 individuals of all ages have been interviewed every year 
#>and they complete the health examination component of the survey. Part of the 
#>data is made available via the NHANES package. Once you install the NHANES 
#>package, you can load the data like this:
install.packages('NHANES')
NHANES
#>returns tibble: 10,000 x 76 dataframe
ref <- tab |> summarize(average = mean(BPSysAve, na.re = TRUE), standard_deviation 
                        = sd(BPSysAve, na.rm = TRUE))
#>4.11.14
#>Using a pipe, assign the average to a numeric variable ref_avg. Hint: Use the code
#>from the previous exercise and then pull.
ref <- NHANES %>% filter(AgeDecade == " 20-29" & Gender == "female") %>% summarize(
  average = mean(BPSysAve, na.re = TRUE), standard_deviation = sd(
    BPSysAve, na.rm = TRUE)) %>% .$average
ref
#>[1] NA
class(ref)
#>[1] "numeric"

#>4.11.15
#>Find min and max from exercise 14 switching out summarize() arguments
ref <- NHANES %>% filter(AgeDecade == " 20-29" & Gender == "female") %>% summarize(
  minbp=min(BPSysAve, na.rm = TRUE), maxbp = max(BPSysAve, na.rm = TRUE))
ref
# A tibble: 1 × 2
#     minbp maxbp
#     <int> <int>
#  1    84   179

#>4.11.16
#>common data science operation splitting data table into groups then computing summary stats
#>for each group.  Filter by gender then group_by age groups for average and standard deviation
NHANES |> filter(Gender == 'female') |> group_by(AgeDecade) |> summarize(
  avg = mean(BPSysAve, na.rm = TRUE), stn_dev = sd(BPSysAve, na.rm = TRUE))
# A tibble: 9 × 4
# Groups:   AgeDecade [9]
#   AgeDecade na.rm   avg stn_dev
#   <fct>     <lgl> <dbl>   <dbl>
# 1 " 0-9"    TRUE   100.    9.07
# 2 " 10-19"  TRUE   104.    9.46
# 3 " 20-29"  TRUE   108.   10.1 
# 4 " 30-39"  TRUE   111.   12.3 
# 5 " 40-49"  TRUE   115.   14.5 
# 6 " 50-59"  TRUE   122.   16.2 
# 7 " 60-69"  TRUE   127.   17.1 
# 8 " 70+"    TRUE   134.   19.8 
# 9  NA       TRUE   142.   22.9 

#>4.11.17
#repeat exercise 16 for the male group
NHANES |> filter(Gender == 'male') |> group_by(AgeDecade, na.rm = TRUE) |> summarize(
  avg = mean(BPSysAve, na.rm = TRUE), stn_dev = sd(BPSysAve, na.rm = TRUE))

#>4.11.18
#>For males between the ages of 40-49, compare systolic blood pressure across race
#>as reported in the Race1 variable. Order the resulting table from lowest to highest
#> average systolic blood pressure.
NHANES |> filter(Gender == 'male',AgeDecade == " 40-49") |> group_by(Race1) |> summarize(
  avg = mean(BPSysAve, na.rm = TRUE), stn_dev = sd(BPSysAve, na.rm = TRUE)) |> arrange(avg)