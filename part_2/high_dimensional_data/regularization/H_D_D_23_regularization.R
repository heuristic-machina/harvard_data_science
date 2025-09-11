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

library(dslabs)
library(dplyr)

data("movielens")

current_year <- as.numeric(format(Sys.Date(), "%Y"))

movies_1993_onward <- movielens %>%
  filter(year >= 1993) %>%
  group_by(movieId) %>%
  summarize(
    avg_rating = mean(rating),
    n = n(),
    title = first(title),
    year = first(year),
    .groups = "drop"   # drop grouping after summarise
  ) %>%
  mutate(
    years = current_year - year + 1,
    n_year = n / years
  ) %>%
  slice_max(n_year, n = 25) %>%
  arrange(desc(n_year))

movies_1993_onward
# A tibble: 25 × 7
#movieId    avg_rating   n   title                year years n_year
#     <int>     <dbl> <int> <chr>               <int> <dbl>  <dbl>
#1     356       4.05   341 Forrest Gump         1994    32  10.7 
#2     296       4.26   324 Pulp Fiction         1994    32  10.1 
#3     318       4.49   311 Shawshank Redempti…  1994    32   9.72


#3. From the table constructed in the previous example, we 
#see that the most rated movies tend to have above average 
#ratings. This is not surprising: more people watch popular 
#movies. To confirm this, stratify the post 1993 movies by 
#ratings per year and compute their average ratings. Make a 
#plot of average rating versus ratings per year and show an 
#estimate of the trend.

data("movielens")

current_year <- as.numeric(format(Sys.Date(), "%Y"))

ratings_per_year<- movielens %>% filter(year >= 1993) %>%
  group_by(movieId) %>%
  summarize(
    avg_rating = mean(rating),
    n = n(),
    title = first(title),
    year = first(year),
    .groups = "drop") %>%
  mutate(
      years = current_year - year + 1,
      rate = n / years) %>%
  ggplot(aes(rate, avg_rating)) +
  geom_point(alpha=0.5) +
  geom_smooth() +
  scale_x_log10()
      
#Copilot: A log scale compresses the large values and expands 
#the small ones, spreading the points more evenly across the
# axis.  This makes patterns visible in the dense region that
# were previously invisible.

#4. In the previous exercise, we see that the more a movie 
#is rated, the higher the rating. Suppose you are doing a 
#predictive analysis in which you need to fill in the 
#missing ratings with some value. Which of the following 
#strategies would you use?

#Fill in the value with a lower value than the average since
# lack of rating is associated with lower ratings. Try out
# different values and evaluate prediction in a test set.

#5. The movielens dataset also includes a time stamp. This 
#variable represents the time and data in which the rating 
#was provided. The units are seconds since January 1, 1970. 
#Create a new column date with the date. Hint: Use the 
#as_datetime function in the lubridate package.
library(lubridate)
movielens <- mutate(movielens, date = as_datetime(timestamp))
movielens


#6. Compute the average rating for each week and plot this 
#average against day. Hint: Use the round_date function 
#before you group_by.

movielens %>% mutate(date=round_date(date, unit='week')) %>%
  group_by(date) %>%
  summarize(rating=mean(rating)) %>%
  ggplot(aes(date, rating)) +
  geom_point() +
  geom_smooth()

#7. The plot shows some evidence of a time effect. If we 
#define d(u,i) as the day for user’s u rating of movie i,
# which of the following models is most appropriate:

#8. The movielens data also has a genres column. This column 
#includes every genre that applies to the movie. Some movies
# fall under several genres. Define a category as whatever 
#combination appears in this column. Keep only categories 
#with more than 1,000 ratings. Then compute the average and 
#standard error for each category. Plot these as error bar 
#plots.

movielens %>% group_by(genres) %>%
  summarize(n=n(), 
            avg=mean(rating), se=sd(rating)/sqrt(n())) %>%
  filter(n>=1000) %>%
  mutate(genres=reorder(genres, avg)) %>%
  ggplot(aes(x=genres, y=avg,
             ymin=avg-2*se, ymax=avg+2*se)) +
  geom_point() +
  geom_errorbar() +
  theme(axis.text.x = element_text(angle=90, hjust=1))