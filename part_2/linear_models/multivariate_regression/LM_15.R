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

#6 Repeat the exercise above, but compute a confidence interval as well.
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

#7 Plot the confidence intervals and notice that they overlap, 
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

#8 Because we are selecting children at random, we can actually do 
#something like a permutation test here. Repeat the computation of 
#correlations 100 times taking a different sample each time. Hint: 
#use similar code to what we used with simulations.

B<-100
N<-25
#father son correlation
rfs<-replicate(B, {
  sample_n(fs, N, replace=TRUE) |>
    summarise(r=cor(father, son)) |> pull(r)})
#mother son correlation
rms<-replicate(B, {
  sample_n(ms, N, replace=TRUE) |>
    summarise(r=cor(mother, son)) |> pull(r)})
#father daughter correlation
rfd<-replicate(B, {
  sample_n(fd, N, replace=TRUE) |>
    summarise(r=cor(father, daughter)) |> pull(r)})
#mother daughter correlation
rmd<-replicate(B, {
  sample_n(md, N, replace=TRUE) |>
    summarise(r=cor(mother, daughter)) |> pull(r)})

#9. Fit a linear regression model to obtain the effects of BB and HR
# on Runs (at the team level) in 1971. Use the tidy function in the
# broom package to obtain the results in a data frame.
library(Lahman)
fit<-Teams %>% filter(yearID %in% 1971) %>% lm(R~BB+HR, data=.)
tidy(fit, conf.int = TRUE)
# A tibble: 3 × 7
#term        estimate std.error statistic p.value conf.low conf.high
#<chr>          <dbl>     <dbl>     <dbl>   <dbl>    <dbl>     <dbl>
#1 (Intercept)  257.      112.         2.31 0.0314   25.3      489.   
#2 BB             0.414     0.210      1.97 0.0625   -0.0237     0.852
#3 HR             1.30      0.431      3.01 0.00673   0.399      2.19 

#10. Now let’s repeat the above for each year since 1962 and make a 
#plot. Use summarize and the broom package to fit this model for every
# year since 1962.

res<-Teams %>% filter(yearID %in% 1962:2018) %>%
  group_by(yearID) %>% do(tidy(lm(R~BB+HR, data=.))) %>%
  ungroup()
res%>%filter(term=="BB") %>% ggplot(aes(yearID, estimate)) +
  geom_point() + geom_smooth(method="lm")

#11. Use the results of the previous exercise to plot the estimated 
#effects of BB on runs.

res<-Teams %>% filter(yearID %in% 1962:2018) %>%
  group_by(yearID) %>% do(tidy(lm(R~BB, data=.))) %>%
  ungroup()
res%>%filter(term=="BB") %>% ggplot(aes(yearID, estimate)) + 
  geom_point() + geom_smooth(method="lm")

#12. Advanced. Write a function that takes R, HR, and BB as arguments
# and fits two linear models: R ~ BB and R~BB+HR. Then use the summary 
#function to obtain the BB for both models for each year since 1962. 
#Then plot these against each other as a function of time.

#13 Since the 1980s, sabermetricians have used a summary statistic 
#different from batting average to evaluate players. They realized 
#walks were important and that doubles, triples, and HRs, should be 
#weighed more than singles. As a result, they proposed the following
# metric: BBPA+Singles+2Doubles+3Triples+4HRAB
#They called this on-base-percentage plus slugging percentage (OPS). 
#Although the sabermetricians probably did not use regression, here 
#we show how this metric is close to what one gets with regression. 
#1. Compute the OPS for each team in the 2001 season. Then plot Runs 
#per game versus OPS.
library(dplyr)
library(tidyverse)
library(broom)
library(magrittr)
library(Lahman)
library(ggplot2)
j<-Teams %>% filter(yearID %in% 2001) %>%
  mutate(singles=(H-HR-X2B-X3B)/G, 
         BB=BB/G, 
         HR=HR/G, 
         R_per_game=R/G,
         doubles=X2B/G,
         triples=X3B/G,
         PA=BB+AB)
OPStab<-j %>% mutate(OPS=BB/PA+(singles+2*doubles+3*triples+4*HR)/AB)
ggplot(data=OPStab, mapping=aes(x=R_per_game, y=OPS)) + geom_point() +
  labs(title='2001 Baseball On-Base-Percentages Vs. Runs Per Game',
       x='On-Base-Percentages', y='Runs per Game')

#14. For every year since 1962, compute the correlation between runs 
#per game and OPS. Then plot these correlations as a function of year.

corrOPS<-Teams %>% filter(yearID %in% 1962:2001) %>%
  mutate(singles=(H-HR-X2B-X3B)/G,
         BB=BB/G, HR=HR/G, R_per_game=R/G,
         doubles=X2B/G, triples=X3B/G, PA=BB+AB) %>%
  mutate(OPS=(BB/PA+(singles+2*doubles+3*triples+4*HR)/AB)) %>%
  group_by(yearID) %>% summarise(corrR=cor(R_per_game, OPS)) 
ggplot(data=corrOPS, mapping=aes(x=yearID, y=corrR))+
  geom_point()+
  labs(
    title="Yearly Correlation On-Base-Percentages Vs. Runs Per",
    x="Year", y="Correlation")

#15. Keep in mind that we can rewrite OPS as a weighted average of 
#BBs, singles, doubles, triples, and HRs. We know that the weights for 
#doubles, triples, and HRs are 2, 3, and 4 times that of singles. But 
#what about BB? What is the weight for BB relative to singles? Hint: The 
#weight for BB relative to singles will be a function of AB and PA.

#AB/PA is the weight for BB to singles

#16. Consider that the weight for BB, AB/PA, will change from team to 
#team. To assess its variability, compute and plot this quantity for 
#each team for each year since 1962. Then plot it again, but instead 
#of computing it for every team, compute and plot the ratio for the 
#entire year. Then, once you are convinced that there is not much of a 
#time or team trend, report the overall average.

#39 year range for weighted BB
BBweight<-Teams%>%filter(yearID %in% 1962:2001)%>%
  mutate(singles=(H-HR-X2B-X3B)/G,
         BB=BB/G, HR=HR/G, R_per_game=R/G,
         doubles=X2B/G, triples=X3B/G, PA=BB+AB,BBw=AB/PA)
ggplot(data=BBweight, mapping=aes(x=yearID, y=BBw)) + geom_point()

#7 year range for weighted BB
BBweightT<-Teams%>%filter(yearID %in% 1962:1969)%>%
  mutate(singles=(H-HR-X2B-X3B)/G, BB=BB/G, HR=HR/G,
         R_per_game=R/G, doubles=X2B/G, 
         triples=X3B/G, PA=BB+AB,BBw=AB/PA)
ggplot(data=BBweightT, mapping=aes(x=yearID, y=BBw)) + 
  geom_point()+
  labs(title="Baseball Weight of 'A Base on Balls' for all MLB Teams",
       x="Year", y="Weight of Base on Balls")