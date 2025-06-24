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

#3. show state names and abbreviation in murders
murders[, .(state, abb)]

#4.Show the top 5 states with the highest murder rates. From here on, do not 
#change the murders dataset, just show the result. Remember that you can filter 
#based on the rank column
murders[rank >= 47]

#5Create a new data frame called no_south that removes states from the South 
#region. How many states are in this category? You can use the function nrow for this.
no_south <- murders[region != 'South']
nrow(no_south)
#show how many rows present in table
#[1] 34

#6Create a new data frame called murders_nw with only the states from the Northeast
#and the West. How many states are in this category?
murders_nw <- murders[region %in% c('Northeast', 'West')]
murders_nw
nrow(murders_nw)
#shows how many states by row count
#[1] 22

#7 Create a table called my_states that contains rows for states satisfying both 
#the conditions: they are in the Northeast or West and the murder rate is less 
#than 1. Show only the state name, the rate, and the rank
my_states <- murders_nw[rate < 1]
my_states[, .(state, rate, rank)]