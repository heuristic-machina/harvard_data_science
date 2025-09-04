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

#Exercises 13.7

#1. Create this table. Now for each poll use the CLT to create a 95% 
#confidence interval for the spread reported by each poll. Call the 
#resulting object cis with columns lower and upper for the limits of 
#the confidence intervals. Use the select function to keep the columns 
#state, startdate, end date, pollster, grade, spread, lower, upper.
library(dslabs)
data("polls_us_election_2016")
polls<-polls_us_election_2016 |> 
  filter(state != "U.S." & enddate>="2016-10-31") |> 
  mutate(spread= rawpoll_clinton/100-rawpoll_trump/100)
cis <- polls %>% 
  mutate(X_hat=(spread+1)/2, 
         se=2*sqrt(X_hat*(1-X_hat)/samplesize), 
         lower=spread-qnorm(0.975)*se, 
         upper=spread+qnorm(0.975)*se) %>%
  select(state, startdate, enddate, pollster, grade, spread, lower, upper)

#2. You can add the final result to the cis table you just created using 
#the right_join function like this.  #Now determine how often the 95% #
add<-results_us_election_2016 |> 
  mutate(actual_spread=clinton/100-trump/100) |> 
  select(state, actual_spread)
cis_data<-cis %>% 
  mutate(state=as.character(state)) %>% 
  left_join(add, by="state")
p_hits<-cis_data %>% 
  mutate(hit=lower<=actual_spread & upper>=actual_spread) %>% 
  summarize(proportion_hits=mean(hit))
