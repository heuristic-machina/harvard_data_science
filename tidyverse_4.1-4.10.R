#>The tidyverse 4.2
library(dslabs)
library(tidyverse)
data(murders)

#>4.2.1 Adding columns
#>refine data frame column for rate using mutate(data frame, name = values) syntax
murders <- mutate(murders, rate = total/population*100000)

#>4.2.2 Row-wise subsetting
#>show only entries with murder rates lower than 0.71 
#>using filter(data frame, conditional) syntax
filter(murders, rate <= .71)
#>          state abb        region population total      rate
#>1        Hawaii  HI          West    1360301     7 0.5145920
#>2          Iowa  IA North Central    3046355    21 0.6893484
#>3 New Hampshire  NH     Northeast    1316470     5 0.3798036
#>4  North Dakota  ND North Central     672591     4 0.5947151
#>5       Vermont  VT     Northeast     625741     2 0.3196211

#>4.2.3 Column-wise subsetting
#>new object created using dplyr select() streamlining data frame
#>filter() passes the conditional to the new object
new_dataframe <- select(murders, state, region, rate)
filter(new_dataframe, rate <= .71)
#>         state          region         rate
#>1        Hawaii         West           0.5145920
#>2        Iowa           North Central  0.6893484
#>3        New Hampshire  Northeast      0.3798036
#>4        North Dakota   North Central  0.5947151
#>5        Vermont        Northeast      0.3196211

#>helper functions
#>example shows where() helper function selecting all numeric columns
new_dataframe <- select(murders, where(is.numeric))
names(new_dataframe)
#>[1] "population" "total"      "rate" 
#>
#>more helper functions: starts_with, ends_with, contains, matches, and num_range
#>starts_with() example
new_dataframe <- select(murders, starts_with('r'))
names(new_dataframe)
#>[1] "region" "rate"

#>4.2.4 Transforming variables
#>mutate() transforms variables
mutate(murders, population = log10(population))
#>used with across() can transform several variables:
mutate(murders, across(c(population, total), log10))
#>helper function applying transformation to all numeric variables
mutate(murders, across(where(is.numeric), log10))
#>helper function applying transformation to all character variables
mutate(murders, across(where(is.character), tolower))

#>4.3 The Pipe
#>combines helper functions for more concise processing
murders |> select(state, region, rate) |> filter(rate <= 0.71) 
#>           state        region              rate
#>1          Iowa         North Central       0.6893484
#>2  North Dakota         North Central       0.5947151
#>3        Hawaii         West 0.5145920
#>4 New Hampshire         Northeast 0.3798036
#>5       Vermont         Northeast 0.3196211


#>4.4 Summarizing data
#>height dataframe example
#>summarize()
library(dplyr)
library(dslabs)
s <- heights |> filter(sex =='Female') |> summarize(average = mean(height), standard_deviation
                                                    = sd(height))
#>new dataframe s created
s
#>   average              standard_deviation
#>   1 64.93942           3.760656
#>   
#>accessor $
s$average
#>1 64.93942           
s$standard_deviation
#>3.760656


#>murders dataframe example
#>find the murder rate for the entire US using summarize() accounting for small & large states
us_murder_rate <- murders |> summarize(rate = sum(total)/sum(population)*100000)
#>      rate
#>      1 3.034555

#>4.4.2 Multiple summaries
#>retrieving 3 summaries from the same variable example
heights |> summarize(median = median(height), min = min(height), max = max(height))
#>    median  min     max
#>1   68.5    50      82.67717
#>
#>achieving same result above using helper functions quantile() and reframe()
#>quantile() returns median, min, and max
#>summarize() expects one value per row so instead we use reframe() to satisfy quantile()
heights |> reframe(quantiles = quantile(height, c(0.5, 0, 1))) 
#>  quantiles
#>1  68.50000
#>2  50.00000
#>3  82.67717
#>
#>a defined function will return a data frame with columns per summary (2 step process below)
median_min_max <- function(x){
  qs <- quantile(x, c(0.5, 0, 1))
  data.frame(median = qs[1], min = qs[2], max = qs[3])
}

heights |> summarize(median_min_max(height))
#>    median  min     max
#>1   68.5    50      82.67717


#>4.4.3 Group then summarize with group_by()
#> using group_by() followed by summarize() to compare data categorically
heights |> group_by(sex) |> summarize(average = mean(height), standard_deviation = sd(height))
#> A tibble: 2 × 3
#>sex       average          standard_deviation
#><fct>     <dbl>              <dbl>
#>1 Female    64.9               3.76
#>2 Male      69.3               3.61
#>
#>another example using the murders database
murders |> group_by(region) |> summarize(median_min_max(rate))
#> A tibble: 4 × 4
#>region        median   min   max
#><fct>          <dbl> <dbl> <dbl>
#>1 Northeast       1.80 0.320  3.60
#>2 South           3.40 1.46  16.5 
#>3 North Central   1.97 0.595  5.36
#>4 West            1.29 0.515  3.63
#> 

#>4.4.4 Extracting variables with pull
#>use pull() to return numeric data type instead of data.frame that dplyr functions return
#>dply example returning data.frame class type
class(us_murder_rate)
#>[1] "data.frame"
#>
#>alternatively using pull() for numeric data class that can be used in function calculation(s)
us_murder_rate <- murders |> summarize(rate = sum(total)/sum(population)*1000000) |> pull(rate)
us_murder_rate
#>[1] 30.34555
class(us_murder_rate)
#>[1] "numeric"


#>4.5 Sorting
#>arrange()
murders |> arrange(desc(rate))
#>
#>4.5.1 Nested sorting
#>Using additional columns to break ties
#>ordered by region then ordered by rate
murders |> arrange(region, rate) |> head()
#>
#>4.5.2 top n
#>behavior same like head() that takes the first 6 results
#>slice_max(x, n=# of rows) returns specified top n
murders |> slice_max(rate, n=5)
#>          state abb        region population total      rate rank
#>1 District of Columbia  DC         South     601723    99 16.452753    1
#>2            Louisiana  LA         South    4533372   351  7.742581    2
#>3             Missouri  MO North Central    5988927   321  5.359892    3
#>4             Maryland  MD         South    5773552   293  5.074866    4
#>5       South Carolina  SC         South    4625364   207  4.475323    5
#>
#>slice_min() returns the least rates or whichever variable is specified
murders |> slice_min(rate, n=5)
#>          state abb        region population total      rate rank
#>1       Vermont  VT     Northeast     625741     2 0.3196211   51
#>2 New Hampshire  NH     Northeast    1316470     5 0.3798036   50
#>3        Hawaii  HI          West    1360301     7 0.5145920   49
#>4  North Dakota  ND North Central     672591     4 0.5947151   48
#>5          Iowa  IA North Central    3046355    21 0.6893484   47