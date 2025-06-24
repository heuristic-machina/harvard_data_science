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