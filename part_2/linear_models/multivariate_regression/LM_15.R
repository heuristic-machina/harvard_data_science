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