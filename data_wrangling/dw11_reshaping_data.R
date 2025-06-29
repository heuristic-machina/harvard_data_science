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


#11.3 separating variables
path <- system.file("extdata", package = "dslabs")

filename <- "life-expectancy-and-fertility-two-countries-example.csv"
filename <-  file.path(path, filename)

raw_dat <- read_csv(filename)
select(raw_dat, 1:5)
#> # A tibble: 2 × 5
#>   country     `1960_fertility` `1960_life_expectancy` `1961_fertility`
#>   <chr>                  <dbl>                  <dbl>            <dbl>
#> 1 Germany                 2.41                   69.3             2.44
#> 2 South Korea             6.16                   53.0             5.99
#> # ℹ 1 more variable: `1961_life_expectancy` <dbl>

#lengthen the data
dat <- raw_dat |> pivot_longer(-country)
head(dat)
#> # A tibble: 6 × 3
#>   country name                 value
#>   <chr>   <chr>                <dbl>
#> 1 Germany 1960_fertility        2.41
#> 2 Germany 1960_life_expectancy 69.3 
#> 3 Germany 1961_fertility        2.44
#> 4 Germany 1961_life_expectancy 69.8 
#> 5 Germany 1962_fertility        2.47
#> # ℹ 1 more row

dat$name[1:5]
#> [1] "1960_fertility"       "1960_life_expectancy" "1961_fertility"      
#> [4] "1961_life_expectancy" "1962_fertility"

#> problem: each observation is split into 2 rows instead of 1 row 
#> solution: separate variable names into each column (widen data)
#> start with separating out the year into it's own column
#> too_many = 'merge' for names differing '_' underscores
dat |> separate_wider_delim(name, delim = "_", 
                            names = c("year", "name"), 
                            too_many = "merge")
#> # A tibble: 224 × 4
#>   country year  name            value
#>   <chr>   <chr> <chr>           <dbl>
#> 1 Germany 1960  fertility        2.41
#> 2 Germany 1960  life_expectancy 69.3 
#> 3 Germany 1961  fertility        2.44
#> 4 Germany 1961  life_expectancy 69.8 
#> 5 Germany 1962  fertility        2.47
#> # ℹ 219 more rows
#> 
#> pivot_wider() used for variable name columns and mutate to get year as 
#> integer, must run together as below:
dat <- dat |> 
  separate_wider_delim(name, delim = "_", 
                       names = c("year", "name"), 
                       too_many = "merge") |>
  pivot_wider() |>
  mutate(year = as.integer(year))

dat
#> # A tibble: 112 × 4
#>   country  year fertility life_expectancy
#>   <chr>   <int>     <dbl>           <dbl>
#> 1 Germany  1960      2.41            69.3
#> 2 Germany  1961      2.44            69.8
#> 3 Germany  1962      2.47            70.0
#> 4 Germany  1963      2.49            70.1
#> 5 Germany  1964      2.49            70.7
#> # ℹ 107 more rows 

#11.4 reshaping with data.table
path <- system.file("extdata", package = "dslabs")
filename <- file.path(path, "fertility-two-countries-example.csv")

#11.4.1 pivot_longer() comparison with melt()
#data.table melt behaves similarly to tidyverse pivot_longer()
library(data.table)
dt_wide_dat <- fread(filename)

dt_tidy_dat <- melt(dt_wide_dat, measure.vars = 2:ncol(dt_wide_dat),
                    variable.name = 'year', value.name = 'fertility')
dt_tidy_dat
#V1   year fertility
#<char> <fctr>     <num>
#1:     country     V2   1960.00
#2:     Germany     V2      2.41
#3: South Korea     V2      6.16
#4:     country     V3   1961.00
#5:     Germany     V3      2.44

#11.4.2 pivot_wider comparison with dcast()
#following code reverts 11.4.1 code to the original data table
dt_dcast <- dcast(dt_tidy_dat, formula = ... ~ year, value.var = 'fertility')