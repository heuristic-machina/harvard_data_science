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

#2. We see that, on average, movies that were released after
# 1993 get more ratings. We also see that with newer movies, 
#starting in 1993, the number of ratings decreases with year:
# the more recent a movie is, the less time users have had to
# rate it.  Among movies that came out in 1993 or later, what
# are the 25 movies with the most ratings per year? Also, 
#report their average rating.

movies_1993_onward <- movielens %>%
  filter(year >= 1993) %>%
  group_by(movieId) %>%
  summarize(avg_rating = mean(rating), n = n(), title=title[1], year=year) %>%
  mutate(n_year = n / years) %>%
  top_n(25, n_year) %>%
  arrange(desc(n_year))