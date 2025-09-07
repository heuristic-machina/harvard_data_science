#Linear Models 15: Multivariate Regression

#15.1.3 team level statistics
library(tidyverse)
library(Lahman)
dat <- Teams |> filter(yearID %in% 1962:2002) |>
  mutate(team = teamID, year = yearID, r = R/G, 
         singles = (H - X2B - X3B - HR)/G, 
         doubles = X2B/G, triples = X3B/G, hr = HR/G,
         sb = SB/G, bb = BB/G) |>
  select(team, year, r, singles, doubles, triples, hr, sb, bb)

#association of homeruns and runs
p <- dat |> ggplot(aes(hr, r)) + geom_point(alpha = 0.5)
p 
#association of bases on balls and runs
dat |> ggplot(aes(bb, r)) + geom_point(alpha = 0.5)
#confounding association of homeruns and base on balls
dat |> ggplot(aes(hr, bb)) + geom_point(alpha = 0.5)


#scale(x, center = TRUE, scale = TRUE)
#centering is done by subtracting the column means of x from their 
#corresponding columns.  Scaling divides each column of x by their standard
#deviations
#Homerun Run strata visualization
dat |> mutate(z_hr = round(scale(hr))) |>
  filter(z_hr %in% -2:3) |>
  ggplot() +  
  stat_qq(aes(sample = r)) +
  facet_wrap(~z_hr) 

#facet_wrap(~z_hr) creates separate Q-Q plots based on how far their home 
#run totals deviate from the average.  This allows the visualization
#of the r distribution (runs) in relation to homeruns z_hr
#filter(z_hr %in% -2:3) displays home run totals within 2 standard deviations
# below to 3 above the mean

#linear regression in predicting the number of runs scored
#runs r is the response variable and homeruns hr is the prediction variable
hr_fit  <- lm(r ~ hr, data = dat)$coef
p + geom_abline(intercept = hr_fit[[1]], slope = hr_fit[[2]])

hr_fit
#(Intercept)          hr 
#2.770363    1.851745 

#the y-intercept of the regression line represents the expected number
#of runs (2.77) when the number of homeruns is zero of the average team

#the teams that hit 1 more homerun per game than the average team score
#1.85 (the slope) more runs per game than the average team.

#relationship of runs to bases from balls
bb_slope <- lm(r ~ bb, data = dat)$coef[2]
bb_slope
bb 
#0.7388725
#adding 2 players with high bases due to balls yeild 1.5 more runs?
#association is not causation:
#the assumption of having players with high rates of bases due to balls
#is confounded with pitchers not wanting to throw strikes to players 
#with high homerun rates
dat |> summarise(cor(bb, hr), cor(singles, hr), cor(bb, singles))
#     cor(bb, hr) cor(singles, hr) cor(bb, singles)
#1   0.4064585       -0.1862848      -0.05126617


#Exercises

#In these exercises, we will compare the stability of singles and BBs.
#1. Before we begin, we want to generate two tables. One for 2002 and
#another for the average of 1999-2001 seasons. We want to define per 
#plate appearance statistics. Here is how we create the 2017 table, 
#keeping only players with more than 100 plate appearances:

library(Lahman)
dat <- Batting |> filter(yearID == 2002) |>
  mutate(pa = AB + BB, 
         singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) |>
  filter(pa >= 100) |>
  select(playerID, singles, bb)

#Now, compute a similar table, but with rates computed over 1999-2001.

#AB, at bat is the number times of either a hit H or an out
#Batting average is H/AB
dat2 <- Batting |> filter(yearID %in% 1999:2001) %>%
  mutate(pa=AB+BB, singles=(H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa>=100) %>%
  group_by(playerID) %>%
  summarise(mean_singles=mean(singles), mean_bb=mean(bb))

#2. You can use the inner_join function to combine the 2001 data and 
#averages in the same table. Compute the correlation between 2002 and 
#the previous seasons for singles and BB:
dat3 <- inner_join(dat, dat2, by = "playerID")
cors<-cor(dat3$singles, dat3$mean_singles)
corb<-cor(dat3$bb, dat3$mean_bb)

head(cors)
#[1] 0.5509222
head(corb)
#[1] 0.7174787

#3. Note that the correlation is higher for BB. To quickly get an 
#idea of the uncertainty associated with this correlation estimate, 
#we will fit a linear model and compute confidence intervals for the
# slope coefficient. However, first make scatterplots to confirm that
# fitting a linear model is appropriate.

dat3 %>% ggplot(aes(singles, mean_singles)) + geom_point()
dat3 %>% ggplot(aes(bb, mean_bb)) + geom_point()

#4. Now fit a linear model for each metric and use the confint function 
#to compare the estimates.
singles<-lm(singles~mean_singles, dat3)
singles
#Call:
#lm(formula = singles ~ mean_singles, data = dat3)

#Coefficients:
#(Intercept)  mean_singles  
#0.06206       0.58813 

confint(singles)
#               2.5 %     97.5 %
#(Intercept)  0.04747792 0.07664646
#mean_singles 0.49943734 0.67683074

bb<-lm(bb~mean_bb, dat3)
bb
#Call:
#lm(formula = bb ~ mean_bb, data = dat3)

#Coefficients:
#(Intercept)   mean_bb  
#0.01548      0.82905  
confint(bb)
#               2.5 %     97.5 %
#(Intercept) 0.007789552 0.02317953
#mean_bb     0.748916885 0.90918171

#5. In a previous section, we computed the correlation between mothers
# and daughters, mothers and sons, fathers and daughters, and fathers
# and sons. We noticed that the highest correlation is between fathers
# and sons and the lowest is between mothers and sons. We can compute
# these correlations using:
library(HistData)
set.seed(1)
galton_heights <- GaltonFamilies |>
  group_by(family, gender) |>
  sample_n(1) |>
  ungroup()

cors <- galton_heights |> 
  pivot_longer(father:mother, 
               names_to = "parent", values_to = "parentHeight") |>
  mutate(child = ifelse(gender == "female", "daughter", "son")) |>
  unite(pair, c("parent", "child")) |> 
  group_by(pair) |>
  summarize(cor = cor(parentHeight, childHeight))
#Are these differences statistically significant? To answer this, we 
#will compute the slopes of the regression line along with their 
#standard errors. Start by using lm and the broom package to compute 
#the slopes LSE and the standard errors.
fs<-filter(galton_heights, gender=="male") |>
  select(father, childHeight) |> rename(son=childHeight)
fd<-filter(galton_heights, gender=="female") |>
  select(father, childHeight) |> rename(daughter=childHeight)
md<-filter(galton_heights, gender=="female") |>
  select(mother, childHeight) |> rename(daughter=childHeight)
ms<-filter(galton_heights, gender=="male") |>
  select(mother, childHeight) |> rename(son=childHeight)
fitfs<-lm(father~son, data=fs)
fitms<-lm(mother~son, data=ms)
fitmd<-lm(mother~daughter, data=md)
fitfd<-lm(father~daughter, data=fd)
tidy(fitfs)
# A tibble: 2 × 5
#term        estimate std.error statistic   p.value
#<chr>          <dbl>     <dbl>     <dbl>    <dbl>
# 1 (Intercept)   37.0      4.87    7.61    1.54e-12
#2 son            0.463    0.0702   6.59    4.74e-10
tidy(fitms)
# A tibble: 2 × 5
#term        estimate std.error statistic  p.value
#<chr>          <dbl>     <dbl>     <dbl>    <dbl>
# 1 (Intercept)   43.7     4.81       9.08   2.07e-16
#2 son            0.293    0.0694    4.22   3.84e- 5
tidy(fitfd)
# A tibble: 2 × 5
#term        estimate std.error statistic  p.value
#<chr>          <dbl>     <dbl>     <dbl>    <dbl>
# 1 (Intercept)   41.7     5.02     8.30    2.71e-14
#2 daughter       0.432    0.0783   5.52    1.21e- 7
tidy(fitmd)
# A tibble: 2 × 5
#term        estimate std.error statistic   p.value
#<chr>          <dbl>     <dbl>     <dbl>    <dbl>
#1 (Intercept)    40.8    4.21      9.70   4.62e-18
#2 daughter       0.364   0.0656    5.55   1.07e- 7

#2 Repeat the exercise above, but compute a confidence interval as well.
tidyfitmd<-tidy(fitmd, conf.int=TRUE)
#default conf.int is 95%
#conf int = spread + or - qnorm(.95)*se or qnorm(.05)*se
# A tibble: 2 × 7
#term        estimate std.error statistic  p.value conf.low conf.high
#<chr>          <dbl>     <dbl>     <dbl>    <dbl>    <dbl>     <dbl>
#1 (Intercept)   40.8      4.21        9.70 4.62e-18   32.5      49.1  
#2 daughter       0.364    0.0656      5.55 1.07e- 7    0.234     0.493
tidyfit<-tidy(fitms, conf.int=TRUE)
tidyfitfd<-tidy(fitfd, conf.int=TRUE)
tidyfitfs<-tidy(fitfs, conf.int=TRUE)

#3 Plot the confidence intervals and notice that they overlap, 
#which implies that the data is consistent with the inheritance of 
#height being independent of sex.

library(gridExtra)
library(ggplot2)

SSS<-galton_heights|>
  pivot_longer(father:mother, names_to="parent",
               values_to="parentHeight") |>
  mutate(child=ifelse(gender=="female", "daughter", "son"))|>
  unite(pair, c("parent", "child")) |>
  group_by(pair) 
SSS |>
  group_by(pair) |>
  reframe(tidy(lm(parentHeight ~ childHeight), conf.int=TRUE)) |>
  filter(term=="childHeight") |>
  select(pair, estimate, conf.low, conf.high) |>
  ggplot(aes(pair, y=estimate, ymin=conf.low, ymax=conf.high)) +
  geom_errorbar() + geom_point()