#Machine Learning 27:
#Conditional probabilities and expectations

#1 Compute conditional probabilities for being Male for the 
#heights dataset. Round the heights to the closest inch. Plot 
#the estimated conditional probability P(x) = Pr(Male|height = x)
#for each x.

data("heights")
heights %>%  
  mutate(height = round(height)) %>%  
  group_by(height) %>%  
  summarize(p = mean(sex == "Male")) %>%
  qplot(height, p, data =.)

#2. In the plot we just made, we see high variability for low 
#values of height. This is because we have few data points in 
#these strata. This time use the quantile function for quantiles 
#0.1, 0.2, ..., 0.9 and the cut function to assure each group 
#has the same number of points. Hint: For any numeric vector x, 
#you can create groups based on quantiles as we demonstrate below.
cut(x, quantile(x, seq(0, 1, 0.1)), include.lowest = TRUE)

ps<-seq(0, 1, 0.1)
heights %>%
  mutate(g=cut(height,
               quantile(height, ps),
               include.lowest=TRUE)) %>%
  group_by(g) %>%
  summarize(p=mean(sex=='Male'), height=mean(height)) %>%
  qplot(height, p, data=.)
