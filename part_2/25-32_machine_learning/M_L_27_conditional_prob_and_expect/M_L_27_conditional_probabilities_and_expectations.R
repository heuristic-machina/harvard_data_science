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