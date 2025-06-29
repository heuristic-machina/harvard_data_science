#11.6 Exercises

#1Run the following command to define the co2_wide object:

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

co2_tidy <- gather(co2_wide, month, co2, -year)
# co2_tidy
#year month    co2
#1   1959     1 315.42
#2   1960     1 316.27
#3   1961     1 316.73
#4   1962     1 317.78
#5   1963     1 318.58