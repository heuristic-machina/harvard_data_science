library(dplyr)
library(dslabs)
library(data.table)
#data.table approach to dplyr functions: mutate, filter, select, group_by 
#and summarize.  First step is converting data frame to data.table using
#as.data.table
murders_dt <- as.data.table(murders)

#5.1.1 column-wise subsetting
#similar to subsetting matrices like dplyr's select()
murders_dt[, c('state', 'region')]
#or use .() signifying column names instead of string format
murders_dt[, .(state, region)]

#5.1.2 adding/transforming variables
#similar to dplyr's mutate() but does not reassign so is optimized for
#computing resources
murders_dt[, rate := total / population * 100000]
#adding multiple columns with quotes
murders_dt[, ":="(rate = total / population * 100000, rank = rank(population))]

#5.1.3 reference vs copy
#example1 := changes by reference
#example2 copy() keeps original
#example3 data.frame() creates copy or use setDT() to convert large datasets

#5.1.4 row-wise subsetting
#similar to dplyr's filter()
murders_dt[rate <= 0.7]
#[] selects column name & db does not need to be first arg as in filter() syntax
#select() and filter() becomes more succinct in the following:
murders_dt[rate <= 0.7, .(state, rate)]

#5.2 summarizing data
#quicker way to subset and summarize data
s <- heights_dt[sex == "Female", .(avg = mean(height), sd = sd(height))]

#5.2.1 multiple summaries
median_min_max <- function(x){
  qs <- quantile(x, c(0.5, 0, 1))
  data.frame(median = qs[1], minimum = qs[2], maximum = qs[3])
}
#place function call in the .() data.table
heights_dt[, .(median_min_max(height))]

#5.2.2 group then summarize
#dplyr equivalent to data.table is 'by'
heights_dt[, .(avg = mean(height), sd = sd(height), by = sex)]