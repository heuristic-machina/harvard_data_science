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


#2.2. Now use the Salaries data frame to add each player’s salary to the 
#table you created in exercise 1. Note that salaries are different every year 
#so make sure to filter for the year 2016, then use right_join. This time show 
#first name, last name, team, HR, and salary.

#updated data frame ex1 for the yearID column to complete the above prompt:
ex1 <- left_join(top, People, by='playerID') |>
  select(HR, playerID, yearID, nameFirst, nameLast)

#the final query
tws_ex2 <- ex1 |>
  filter(yearID == 2016) |>
  right_join(select(Salaries, playerID, yearID, teamID, salary), by = c("playerID", "yearID")) |>
  select(nameFirst, nameLast, teamID, HR, salary)

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

#44. Now use the left_join function to add the yearly average to the co2_wide 
#dataset. Then compute the residuals: observed co2 measure - yearly average.

co2_residuals <- co2_wide |> left_join(yearly_avg, by='year') |>
  mutate(residuals=co2-yearly_avg)
# A tibble: 6 × 5
#   year  month   co2 yearly_avg residuals
#   <int> <dbl> <dbl>      <dbl>     <dbl>
#1  1959     1  315.       316.    -0.406
#2  1959     2  316.       316.     0.484