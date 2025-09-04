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
#the right_join function like this.  #Now determine how often the 95% 
#Now determine how often the 95% confidence interval includes the actual result.
  mutate(actual_spread=clinton/100-trump/100) |> 
  select(state, actual_spread)
cis_data<-cis %>% 
  mutate(state=as.character(state)) %>% 
  left_join(add, by="state")
p_hits<-cis_data %>% 
  mutate(hit=lower<=actual_spread & upper>=actual_spread) %>% 
  summarize(proportion_hits=mean(hit))

#3. Repeat this, but show the proportion of hits for each pollster. Show only
#pollsters with more than 5 polls and order them from best to worst. Show the
#number of polls conducted by each pollster and the FiveThirtyEight grade of 
#each pollster. Hint: use n=n(), grade=grade[1] in the call to summarize.

add<-results_us_election_2016 %>%
  mutate(actual_spread=clinton/100-trump/100) %>%
  select(state, actual_spread)
ci_data<-cis %>% mutate(state=as.character(state)) %>%
  left_join(add, by="state")
p_hits<-ci_data %>%
  mutate(hit=lower<=actual_spread & upper>=actual_spread) %>%
  group_by(pollster) %>% filter(n()>=5) %>%
  summarize(proportion_hits=mean(hit), n=n(), grade=grade[1])
p_hits
# A tibble: 13 × 4
#pollster                                 proportion_hits     n grade
#<fct>                                              <dbl> <int> <fct>
#1 Emerson College                                    0.909    11 B    
#2 Google Consumer Surveys                            0.588   102 B    
#3 Gravis Marketing                                   0.783    23 B-   
#4 Ipsos                                              0.807   119 A-   
#5 Mitchell Research & Communications                 0.8       5 D    
#6 Public Policy Polling                              0.889     9 B+   
#7 Quinnipiac University                              1         6 A-   
#8 Rasmussen Reports/Pulse Opinion Research           0.774    31 C+   
#9 Remington                                          0.667     9 NA   
#10 SurveyMonkey                                       0.577   357 C-   
#11 Trafalgar Group                                    0.778     9 C    
#12 University of New Hampshire                        0.857     7 B+   
#13 YouGov                                             0.544    57 B    


#4. Repeat exercise 3, but instead of pollster, stratify by state. Note that 
#here we can’t show grades.
add<-results_us_election_2016 %>% 
  mutate(actual_spread=clinton/100-trump/100) %>% 
  select(state, actual_spread)
ci_data<-cis %>% 
  mutate(state=as.character(state)) %>% 
  left_join(add, by="state")
p_hits_by_state<-ci_data %>% 
  mutate(hit=lower<=actual_spread & upper>=actual_spread) %>% 
  group_by(state) %>% 
  filter(n()>=5) %>% 
  summarize(proportion_hits=mean(hit), n=n()) %>% 
  arrange(desc(proportion_hits))
p_hits_by_state
# A tibble: 51 × 3
#state        proportion_hits     n
#<chr>                  <dbl> <int>
#1 Connecticut            1        13
#2 Delaware               1        12
#3 Rhode Island           1        10
#4 New Mexico             0.941    17
#5 Washington             0.933    15
#6 Oregon                 0.929    14
#7 Illinois               0.923    13
#8 Nevada                 0.923    26
#9 Maine                  0.917    12
#10Montana                0.917    12

#5. Make a barplot based on the result of exercise 4. Use coord_flip.
p_hits_state_bar <- p_hits_by_state %>%
  mutate(state=reorder(state, proportion_hits)) %>%
  ggplot(aes(state, proportion_hits)) + 
  geom_bar(stat='identity') +
  coord_flip()
p_hits_state_bar