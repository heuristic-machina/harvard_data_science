#Statistical Inference 13: Hierchichal Models

#13.3 mathematical representations of the hierarchical model

#Using the election polls of 2016 Clinton vs Trump

#using variability as Xij
#collecting data from a pollster with 6 polls as index J, 
#the number of each pollster index I,
#assuming the spread is 2.1, sd is 0.025, and sample N is 2000.
set.seed(3)
I <- 5
J <- 6
N <- 2000
mu <- .021
p <- (mu + 1)/2
h <- rnorm(I, 0, 0.025)
X <- sapply(1:I, function(i){
  mu + h[i] + rnorm(J, 0, 2*sqrt(p*(1 - p)/N))
  })

#graphing: change X vector to graphable data frame
#red line shows true spread mu
df<- as.data.frame(X)
df$Poll <- 1:J
library(tidyr)
df_long <- pivot_longer(df, cols = -Poll, names_to='Pollster', values_to='Spread')
ggplot(df_long, aes(x=Spread, y=Pollster)) + 
  geom_point(color='steelblue', size=3) +
  geom_vline(xintercept = mu, linetype='dashed', color='red') +
  labs(title='Simulated Data',
       x='Spread',
       y='Pollster') +
  theme_minimal()
