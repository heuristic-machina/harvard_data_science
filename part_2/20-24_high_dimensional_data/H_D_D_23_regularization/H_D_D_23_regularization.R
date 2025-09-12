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

#Next 7 exercises for education system
#testing if smaller sizes test better

#simulate small schools dataset for 100 schools
#simulate number of students in each school n

set.seed(1986)
n<-round(2^rnorm(1000, 8, 1))

#rt(n, df=degrees of freedom) generates random variates

mu<-round(80+2*rt(1000,5))
range(mu)
#[1] 67 90

schools<-data.frame(id=paste('PS',1:100),
                    size=n,
                    quality=mu,
                    rank=rank(-mu))

#have students take test with average measured by quality
#and standard deviation of 30 percentage points

scores<-sapply(1:nrow(schools), function(i) {
  scores<-rnorm(schools$size[i], schools$quality[i], 30)
  scores
})
schools<-schools |> mutate(score=sapply(scores,mean))

#10. What are the top schools based on the average score? 
#Show just the ID, size, and the average score.
schools |> top_n(10, score) |> arrange(desc(score)) |>
  select(id, size, score)

#11. Compare the median school size to the median school
# size of the top 10 schools based on the score.

top_10<- schools |> top_n(10, score) |> arrange(desc(score)) |>
  select(id, size, score)
median(size)
#1        173.5

schools %>% summarize(median(size))
median(size)
#1          261

#12. According to this test, it appears small schools are 
#better than large schools. Five out of the top 10 schools 
#have 100 or fewer students. But how can this be? We 
#constructed the simulation so that quality and size are 
#independent. Repeat the exercise for the worst 10 schools.
bottom_10<- schools |> top_n(10, -score) |> arrange((score)) |>
  select(id, size, score)
bottom_10

#13. The same is true for the worst schools! They are small 
#as well. Plot the average score versus school size to see 
#what’s going on. Highlight the top 10 schools based on the
# true quality. Use the log scale transform for the size.

schools %>% ggplot(aes(size,score)) +
  geom_point(alpha=.5) +
  geom_point(data=filter(schools, rank<=10), col=2) +
  scale_x_log10()

#14. We can see that the standard error of the score has 
#larger variability when the school is smaller. This is a 
#basic statistical reality we learned in the probability and 
#inference sections. In fact, note that 4 of the top 10 
#schools are in the top 10 schools based on the exam score.

#Let’s use regularization to pick the best schools. Remember
# regularization shrinks deviations from the average towards
# 0. So to apply regularization here, we first need to define
# the overall average for all schools:
overall <- mean(sapply(scores, mean))

#and then define, for each school, how it deviates from 
#that average. Write code that estimates the score above 
#average for each school, but dividing by n + lambda instead
# of n, with n the school size and lambda a regularization
# parameter. Try lambda=3.

lambda=3
reg_school<-schools %>% 
  mutate(reg_score=overall +
           (score-overall)*size/(size+lambda)) %>%
  arrange(desc(reg_score))

reg_school %>% top_n(10, reg_score)

#15. Notice that this improves things a bit. The number of 
#small schools that are not highly ranked is now 4. Is there
# a better lambda? Find the lambda that minimizes the 
#RMSE = 1/100 summation(quality-estimate)^2.

lambdas<-seq(10,250)
rmse<-sapply(lambdas, function(lambda){
  schools %>%
    mutate(reg_score=overall +
             (score-overall)*size/(size+lambda)) %>%
    summarize(rmse=sqrt(1/100*sum((reg_score-quality)^2))) %>%
    pull(rmse)
})
lambdas[which.min(rmse)]
#[1] 125

#16. Rank the schools based on the average obtained with 
#the best lambda. Note that no small school is incorrectly 
#included.

lambda_best<-125
school_lambda<- schools %>%
  mutate(reg_score=overall + (score-overall)*size/(size+lambda_best)) %>%
  arrange(desc(reg_score)) %>%
  top_n(10, reg_score)
school_lambda

#17. A common mistake to make when using regularization is 
#shrinking values towards 0 that are not centered around 0.
# For example, if we don’t subtract the overall average before
# shrinking, we actually obtain a very similar result. 
#Confirm this by re-running the code from exercise 6, but 
#without removing the overall mean.

lambdas<-seq(10,250)
rmse_not_cent<-sapply(lambdas, function(lambdas){
  score_not_cent<-sapply(scores,
                             function(x) sum(x)/length(x)+lambda)
  sqrt(mean((score_not_cent-schools$quality)^2))
  })
lambdas[which.min(rmse_not_cent)]
#[1] 10