#Exercises 7.7 data visualization

#1. Define variables containing the heights of males and females like this:
#library(dslabs)
# male <- heights$height[heights$sex == "Male"]
#female <- heights$height[heights$sex == "Female"]
#How many measurements for each?
male <- heights$height[heights$sex == 'Male']
male
#num[1:812]
female <- heights$height[heights$sex == "Female"]
female
#num[1:238]

#2. Create a five row table showing female_percentiles and male_percentiles with 
#the 10th, 30th, 50th, 70th, & 90th percentiles for each sex. Then create a data
#frame with these two as columns.
male_percentiles <- quantile(male, probs=c(.1, .3, .5, .7, .9))
female_percentiles <- quantile(female, probs=c(.1, .3, .5, .7, .9))
df_perc <- data.frame(male_percentiles, female_percentiles)
df_perc
#male_percentiles female_percentiles
#10%         65.00000           61.00000
#30%         68.00000           63.00000
#50%         69.00000           64.98031
#70%         71.00000           66.46417
#90%         73.22751           69.00000

#3-7 references box-plot.  solutions:https://rpubs.com/dimpz429/ph125ch91

#8  Load the height data set and create a vector x with just the male heights. 
#What proportion of the data is between 69 and 72 inches (taller than 69, but 
#shorter or equal to 72)? Hint: use a logical operator and mean.
library(dslabs)
x <- heights$height[heights$sex=="Male"]
meanx<-mean(x)
sdx<-sd(x)
pnorm(72, meanx, sdx)-pnorm(69, meanx, sdx)
# [1] 0.3061779