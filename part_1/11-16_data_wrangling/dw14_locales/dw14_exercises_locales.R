#Data wrangling exercises 14.4 Locales

#1. Load the lubridate package and set the locale to French for this exercise.
library(lubridate)
Sys.setlocale("LC_TIME", "fr_FR.UTF-8")

#2Create a numeric vector containing the following numbers: 
#12345.67, 9876.54, 3456.78, and 5432.10.
mes_nombres <- c(12345.67, 9876.54, 3456.78, 5432.10)

#3. Use the format() function to format the numeric vector as currency, 
#displaying the values in Euros. Ensure that the decimal point is represented 
#correctly according to the French locale. Print the formatted currency values.
formatted_euros <- format(mes_nombres,
                          big.mark = " ",
                          decimal.mark = ",",
                          nsmall = 2)

# Add the Euro symbol
formatted_euros <- paste(formatted_euros, "€")

print(formatted_euros)

#4Create a date vector with three dates: July 14, 1789, January 1, 1803, and 
#July 5, 1962. Use the format() function to format the date vector in the
#“dd Month yyyy” format, where “Month” should be displayed in the French 
#language. Ensure that the month names are correctly translated according to 
#the French locale. Print the formatted date values.

#create vector
dates <- as.Date(c("1789-07-14", "1803-01-01", "1962-07-05"))
formatted_dates <- format(dates, "%d %B %Y")
print(formatted_dates)
#[1] "12 345,67 €" " 9 876,54 €" " 3 456,78 €" " 5 432,10 €"

#5. Reset the locale to the default setting (e.g., “C” or “en_US.UTF-8”) to 
#revert to the standard formatting.
Sys.setlocale("LC_TIME", "en_US.UTF-8")

#6. Repeat steps 2-4 for the numeric vector, and steps 5-7 for the date 
#vector to observe the standard formatting.
num_vector = c(12345.67, 9876.54, 3456.78,5432.10)
formatted_dollars <- format(num_vector, big.mark = ',', decimal.mark = '.', nsmall =2)
formatted_dollars <- paste('$', formatted_dollars)
formatted_dollars
#[1] "$ 12,345.67" "$  9,876.54" "$  3,456.78" "$  5,432.10"