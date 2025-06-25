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

#5 Create a new data frame called no_south that removes states from the South 
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

#8. We will provide some basic facts about blood pressure. First let’s select a
#group to set the standard. We will use 20-to-29-year-old females. AgeDecade is 
#a categorical variable with these ages. Note that the category is coded like 
# " 20-29", with a space in front! Use the data.table package to compute the 
#average and standard deviation of systolic blood pressure as saved 
#in the BPSysAve variable. Save it to a variable called ref.
library(dslabs)
library(NHANES)
library(data.table)
data(NHANES)
nhdat <- setDT(NHANES)
#convert to data.table
ref <- nhdat[AgeDecade %in% " 20-29" & Gender %in% "female", .(average = mean(BPSysAve, na.rm=TRUE), standard_deviation = sd(BPSysAve, na.rm=TRUE))]

#9 report the min and max values for the same group
refmm <- nhdat[AgeDecade %in% " 20-29" & Gender %in% "female", .(minBP = min(BPSysAve, na.rm=TRUE), maxBP = max(BPSysAve, na.rm=TRUE))]

#10 Compute the average and standard deviation for females, but for each age group 
#separately rather than a selected decade as in exercise 8. Note that the age groups are defined by AgeDecade.
reff <- nhdat[Gender %in% "female", .(average = mean(BPSysAve, na.rm=TRUE), standard_deviation = sd(BPSysAve, na.rm=TRUE)), by=AgeDecade]

#11 repeat #10 grouping by male
refm <- nhdat[Gender %in% "male", .(average = mean(BPSysAve, na.rm=TRUE), standard_deviation = sd(BPSysAve, na.rm=TRUE)), by=AgeDecade]

#12 For males between the ages of 40-49, compare systolic blood pressure across 
#race as reported in the Race1 variable. Order the resulting table from lowest to 
#highest average systolic blood pressure.
refrace <- nhdat[Gender %in% "male" & AgeDecade %in% ' 40-49', .(average = mean(BPSysAve, na.rm=TRUE), standard_deviation = sd(BPSysAve, na.rm=TRUE)), by=Race1]
