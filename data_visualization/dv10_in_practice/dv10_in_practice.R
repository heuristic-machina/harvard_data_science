#Data visualization 10 In Practice

#10.3 Faceting
#visualize variables by continent and specified years
filter(gapminder, year %in% c (1970, 2010)) |> 
  ggplot(aes(infant_mortality, life_expectancy, col= continent)) +
  geom_point() +
  facet_grid(year~continent)
#facet syntax for 1 argument
filter(gapminder, year %in% c(1962, 2012)) |>
  +     ggplot(aes(fertility, life_expectancy, col = continent)) +
  +     geom_point() +
  +     facet_grid(. ~ year)
#trend transformation using facet_wrap()
years <- c(1960, 1970, 1980, 1990, 2000, 2010)
gapminder |> filter(year %in% years) |> 
  ggplot(aes(life_expectancy, infant_mortality, col = continent)) +
  geom_point() + 
  facet_wrap(~year)