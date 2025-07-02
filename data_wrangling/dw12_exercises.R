#Data Wrangling Exercises 12.5: Joining Tables

#1. Use the left_join function to create a table of the top home run hitters. 
#The table should have playerID, first name, last name, and number of 
#home runs (HR). Rewrite the object top with this new table.

library(Lahman)

top <- Batting |> 
  filter(yearID == 2016) |>
  arrange(desc(HR)) |>
  slice(1:10)

top |> as_tibble()

People |> as_tibble()

ex1 <- left_join(top, People, by='playerID') |>
  select(HR, playerID, nameFirst, nameLast)
#   HR  playerID nameFirst    nameLast
#1  47 trumbma01      Mark      Trumbo
#2  43  cruzne02    Nelson        Cruz
#3  42 daviskh01     Khris       Davis
#4  42 doziebr01     Brian      Dozier
#5  42 encared01     Edwin Encarnacion

#3. In a previous exercise, we created a tidy version of the co2 dataset:

co2_wide <- data.frame(matrix(co2, ncol = 12, byrow = TRUE)) |> 
  setNames(1:12) |>
  mutate(year = 1959:1997) |>
  pivot_longer(-year, names_to = "month", values_to = "co2") |>
  mutate(month = as.numeric(month))

#We want to see if the monthly trend is changing, so we are going to remove 
#the year effects and then plot the results. We will first compute the year 
#averages. Use the group_by and summarize to compute the average co2 for each 
#year. Save in an object called yearly_avg.

yearly_avg <- co2_wide |> group_by(year) |> summarize(yearly_avg = mean(co2))
# A tibble: 6 × 2
#year yearly_avg
#<int>      <dbl>
#1  1959       316.
#2  1960       317.
#3  1961       317.
#4  1962       318.
#5  1963       319.
#6  1964       319.