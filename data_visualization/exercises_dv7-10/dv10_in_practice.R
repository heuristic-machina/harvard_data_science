#10.1 Data visualization in practice

#10.3 faceting
#facet_grid()
#comparing two variables year~continent
filter(gapminder, year %in% c (1970, 2010)) |> 
  ggplot(aes(infant_mortality, life_expectancy, col= continent)) + 
  geom_point() + facet_grid(year~continent)

#analysis with one variable
#facet_grid(.~x)
filter(gapminder, year %in% c(1962, 2012)) |>
  ggplot(aes(fertility, life_expectancy, col = continent)) +
  geom_point() +
  facet_grid(. ~ year)

#10.3.1 facet_wrap() for viewing time transformation
gapminder |> filter(year %in% years) |>
  ggplot(aes(life_expectancy, infant_mortality, col = continent)) +
  geom_point() +
  facet_wrap(~year)

#10.4 time series plot
#continent fertility trends
#americas
amerfert <- gapminder |> filter(continent == 'Americas') |> 
  ggplot(aes(year, fertility)) +
  geom_point() +
  ggtitle('Americas Fertility 50 years')
#europe
eurofert <- gapminder |> filter(continent == 'Europe') |>
  ggplot(aes(year, fertility)) +
  geom_point() +
  ggtitle('Europe Fertility 50 years')
#africa
afrifert <- gapminder |> filter(continent == 'Africa') |>
  ggplot(aes(year, fertility)) +
  geom_point() +
  ggtitle('Africa Fertility 50 years')
#asia
asiafert <- gapminder |> filter(continent == 'Asia') |>
  ggplot(aes(year, fertility)) +
  geom_point() +
  ggtitle('Asia Fertility 50 years')
#oceania
oceanfert <- gapminder |> filter(continent == 'Oceania') |>
  ggplot(aes(year, fertility)) +
  geom_point() +
  ggtitle('Oceania Fertility 50 years')