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