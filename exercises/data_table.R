#5.4 exercises
#1. Load the data.table package and the murders dataset and convert it 
#to data.table object:
library(data.table)
library(dslabs)
data("murders")
murders<-setDT(murders)
#add population_in_millions column
murders[, rate := total/population*100000]

#2.add a column rank containing the rank, from highest to lowest murder rate.
murders[, rank := rank(rate)]
murders[order(-rank)]