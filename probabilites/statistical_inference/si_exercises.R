#Probabilities: Statistical Inference
#Exercises 5.9

#1. In American Roulette, you can also bet on green. 
#There are 18 reds, 18 blacks and 2 greens (0 and 00). 
#What are the chances the green comes out?
green <- 2
red <- 18
black <- 18
p_green = green/(green+black+red)
p_green
#[1] 0.05263158