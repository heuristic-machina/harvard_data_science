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