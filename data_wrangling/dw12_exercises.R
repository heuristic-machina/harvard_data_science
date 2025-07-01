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