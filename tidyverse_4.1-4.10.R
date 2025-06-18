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





















