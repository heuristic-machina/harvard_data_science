#Data wrangling 11 reshaping data 

#access the data frame
library(tidyverse) 
library(dslabs)
path <- system.file("extdata", package = "dslabs")
filename <- file.path(path, "fertility-two-countries-example.csv")
wide_data <- read_csv(filename)

# 11.1 pivot_longer() converts wide data into tidy data
#the original csv does not have a column labeled as fertility, instead value 
#is assigned to observed year
#original format
# country   1960  1961  1962
# Germany   2.41  2.44  2.47
# S Korea   6.16  5.99  5.79

#pivot data using names_to() and values_to() args
#specify target columns as code with backticks
new_tidy_data <- wide_data |>
  pivot_longer(`1960`:`2015`, names_to = 'year', values_to = 'fertility')

head(new_tidy_data)
#country year  fertility
#<chr>   <chr>     <dbl>
#1 Germany 1960       2.41
#2 Germany 1961       2.44
#3 Germany 1962       2.47
#4 Germany 1963       2.49
#5 Germany 1964       2.49
#6 Germany 1965       2.48

#South Korea data starts on row57
#year is returned as character and needs to be an integer for graphing
new_tidy_data <- wide_data |>
  pivot_longer(-country, names_to = "year", values_to = "fertility") |>
  mutate(year = as.integer(year))

#plot
new_tidy_data |> 
  ggplot(aes(year, fertility, color = country)) + 
  geom_point()

#11.2 pivot_wider()
#inverse of pivot_longer()
#takes a tidy data frame and widens it usually as an intermediary step
#uses names_from() and values_from for the column args
pwide_data <- new_tidy_data |> 
  pivot_wider(names_from = year, values_from = fertility)

select(pwide_data, country, `1960`:`1967`)
## A tibble: 2 × 9
#country        `1960` `1961` `1962` `1963` `1964` `1965` `1966` `1967`
#<chr>           <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>
#1 Germany       2.41   2.44   2.47   2.49   2.49   2.48   2.44   2.37
#2 South Korea   6.16   5.99   5.79   5.57   5.36   5.16   4.99   4.85
