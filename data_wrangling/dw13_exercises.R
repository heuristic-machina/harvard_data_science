#Data Wrangling Parsing dates & times Exercises 13.4

#For these exercises we will use the following dataset:

library(dslabs)
head(pr_death_counts)

#1. We want to make a plot of death counts versus date. Confirm that the date 
#variable are in fact dates and not strings.
class(pr_death_counts$date)
#[1] "Date"

#2. Plot deaths versus date.
#using point
ggplot(pr_death_counts, aes(x=deaths, y=date)) + geom_point() + labs(x = 'Deaths', y = 'Date')

#using line
ggplot(pr_death_counts, aes(x=deaths, y=date)) + geom_line() + labs(x = 'Deaths', y = 'Date')