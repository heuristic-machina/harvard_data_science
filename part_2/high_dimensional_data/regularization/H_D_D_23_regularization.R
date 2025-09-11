#High Dimensional Data 23
#Regularization

#1. For the movielens data, compute the number of ratings for 
#each movie and then plot it against the year the movie was 
#released. Use the square root transformation on the counts.

library(dslabs)
library(tidyverse)

data("movielens")

movielens %>%
  group_by(movieId) %>%
  summarize(
    n = n_distinct(userId),
    year = as.character(first(year))
  ) %>%
  ggplot(aes(x = year, y = n)) +
  geom_boxplot() +
  scale_y_sqrt() +  # same as coord_trans(y = "sqrt")
  scale_x_discrete(
    breaks = function(x) x[seq(1, length(x), by = 5)]  # show every 5th year
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8)
  ) +
  labs(
    title = "Number of Distinct Users per Movie by Year",
    x = "Year",
    y = "Number of Users (sqrt scale)"
  )
