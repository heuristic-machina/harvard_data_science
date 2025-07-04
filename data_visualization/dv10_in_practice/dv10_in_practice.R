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

#10.4 time series plot
#geom_point for each continent
#americas
amerfert <- gapminder |> filter(continent == 'Americas') |>
  ggplot(aes(year, fertility)) +
  geom_point() + ggtitle('Americas Fertility 50 years')
#europe
eurofert <- gapminder |> filter(continent == 'Europe') |>
  ggplot(aes(year, fertility)) +
  geom_point() + ggtitle('Europe Fertility 50 years')
#africa
afrifert <- gapminder |> filter(continent == 'Africa') |>
  ggplot(aes(year, fertility)) +
  geom_point() + ggtitle('Africa Fertility 50 years')
#asia
asiafert <- gapminder |> filter(continent == 'Asia') |>
  ggplot(aes(year, fertility)) +
  geom_point() + ggtitle('Asia Fertility 50 years')
#oceania
oceanfert <- gapminder |> filter(continent == 'Oceania') |>
  ggplot(aes(year, fertility)) +
  geom_point() + ggtitle('Oceania Fertility 50 years')

#geom(line() for tracking single series (1 country below))
us <- gapminder |> 
  +     filter(country == "United States") |> 
  +     ggplot(aes(year, fertility)) +
  +     geom_line() + ggtitle('US Fertility Trend')

#geom_line comparing 2 countries using col argument to differentiate lines
countries <- c("South Korea","Germany")
skor_germ <- gapminder |> filter(country %in% countries & !is.na(fertility)) |> 
  ggplot(aes(year,fertility, col = country)) +
  geom_line() + ggtitle('Germany & S Korean Fertility Trends')

#label lines with geom_textpath()
install.packages('geomtextpath')
library(geomtextpath)
labline <- gapminder |> 
  +     filter(country %in% countries) |> 
  +     ggplot(aes(year, life_expectancy, col = country, label = country)) +
  +     geom_textpath() +
  +     theme(legend.position = "none")

#10.5 data transformations