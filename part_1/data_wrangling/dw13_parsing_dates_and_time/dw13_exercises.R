#Data Wrangling Parsing dates & times Exercises 13.4

#For these exercises we will use the following dataset:

library(dslabs)
head(pr_death_counts)

#1. We want to make a plot of death counts versus date. Confirm that the date 
#variable are in fact dates and not strings.
class(pr_death_counts$date)
#[1] "Date"

#2. Plot deaths versus date.
#using point
ggplot(pr_death_counts, aes(x=deaths, y=date)) + geom_point() + labs(x = 'Deaths', y = 'Date')

#using line
ggplot(pr_death_counts, aes(x=deaths, y=date)) + geom_line() + labs(x = 'Deaths', y = 'Date')

#5. Repeat the plot but use the day of the year on the x-axis instead of date.
day_of_year <- yday(pr_death_counts$date)
ggplot(pr_death_counts, aes(x = deaths, y = day_of_year)) + geom_line() +
  labs(x= "Day of Year", y = "Deaths")

#6. Compute the deaths per day by month.
#copilot ai generated solution
# Load required libraries
library(dslabs)
library(dplyr)
library(lubridate)

# Read your data
df <- pr_death_counts

# Convert the date column to proper Date format
df$date <- as.Date(df$date)

# Create a new column for year-month
df <- df |>
  mutate(year_month = floor_date(date, "month"))

# Group by month, sum deaths, calculate days in month, and compute daily average
monthly_deaths <- df |>
  group_by(year_month) |>
  summarise(total_deaths = sum(deaths)) |>
  mutate(days_in_month = days_in_month(year_month),deaths_per_day = total_deaths / days_in_month)

#7. Show the deaths per days for July and for September.

# Filter for July and September
july_data <- df[format(df$date, "%m") == "07", ]
sept_data <- df[format(df$date, "%m") == "09", ]
# Group and summarize
july_daily <- july_data %>%
  mutate(day = format(date, "%d")) %>%
  group_by(day) %>%
  summarise(deaths = sum(deaths))

sept_daily <- sept_data %>%
  mutate(day = format(date, "%d")) %>%
  group_by(day) %>%
  summarise(deaths = sum(deaths))

#8  Compute deaths per week and make a plot.
weekly_deaths <- pr_death_counts %>%
  group_by(year, week) %>%
  summarise(deaths = sum(deaths), .groups = 'drop')

ggplot(weekly_deaths, aes(x = week, y = deaths, group = year, color = factor(year))) +
  geom_line(linewidth = 1) +
  labs(title = "Weekly Deaths Over Time",
       x = "ISO Week Number",
       y = "Number of Deaths",
       color = "Year") +
  theme_minimal()