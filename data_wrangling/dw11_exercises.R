#11.6 Exercises

#1Run the following command to define the co2_wide object:
library(tidyverse)
library(dslabs)
co2
co2_wide <- data.frame(matrix(co2, ncol = 12, byrow = TRUE)) |> 
  setNames(1:12) |>
  mutate(year = as.character(1959:1997))

#       1      2      3      4      5      6      7      8      9     10     11         
#1 315.42 316.31 316.50 317.56 318.13 318.00 316.39 314.65 313.68 313.18 314.66

#12     year
#315.43 1959

#Use the pivot_longer function to wrangle this into a tidy dataset. Call 
#the column with the CO2 measurements co2 and call the month column month. 
#Call the resulting object co2_tidy.

co2_tidy <- co2_wide |> pivot_longer(-year, names_to='month', values_to='co2')
# co2_tidy
#year month    co2
#1   1959     1 315.42
#2   1960     1 316.27
#3   1961     1 316.73
#4   1962     1 317.78
#5   1963     1 318.58

#2. Plot CO2 versus month with a different curve for each year using this code:
co2_tidy |> ggplot(aes(month, co2, color = year)) + geom_line()
#Rewrite your code to make sure the month column is numeric. Then make the plot.
co2_tidy %>% ggplot(aes(as.numeric(month), co2, color = year)) + geom_line()

#3 CO2 measures increase monotonically from 1959 to 1997

#4  Now load the admissions data set, which contains admission information for 
#men and women across six majors and keep only the admitted percentage column:
load(admissions)
dat <- admissions |> select(-applicants)

p_wide_dat <- dat |> pivot_wider(names_from = gender, values_from = admitted)
# A tibble: 6 × 3
#major   men women
#<chr> <dbl> <dbl>
#1 A        62    82
#2 B        63    68
#3 C        37    34
#4 D        33    35
#5 E        28    24
#6 F         6     7

#5 Wrangle Admissions data majors into four observations: admitted_men, 
#admitted_women, applicants_men, and  applicants_women.  This is a 2-step process:
#pivot_longer() followed by pivot_wider()

#Use the pivot_longer function to create a test data frame with a column 
#containing the type of observation: admitted or applicants. Call the new 
#columns name and value.
test_dat <- admissions |>
  pivot_longer(admitted:applicants, names_to='name', values_to='value')
# A tibble: 6 × 4
#major gender name       value
#<chr> <chr>  <chr>      <dbl>
#1 A     men    admitted      62
#2 A     men    applicants   825
#3 B     men    admitted      63
#4 B     men    applicants   560
#5 C     men    admitted      37
#6 C     men    applicants   325

#6 Use unite() function to create new column called column_name
td2 <- unite(test_dat, col_name, c(name, gender))
# A tibble: 6 × 3
#major col_name       value
#1 A     admitted_men      62
#2 A     applicants_men   825
#3 B     admitted_men      63
#4 B     applicants_men   560
#5 C     admitted_men      37
#6 C     applicants_men   325