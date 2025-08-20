#6.5 Importing Exercises

#1. Use the read_csv function to read each of the files that the following code saves in the files object:
library(readr)
files <- list.files(path)
files
#[1] "2010_bigfive_regents.xls"                                "calificaciones.csv"                                     
#[3] "carbon_emissions.csv"                                    "fertility-two-countries-example.csv"                    
#[5] "HRlist2.txt"                                             "life-expectancy-and-fertility-two-countries-example.csv"
#[7] "murders.csv"                                             "olive.csv"                                              
#[9] "RD-Mortality-Report_2015-18-180531.pdf"                  "ssa-death-probability.csv"  
filename <- c('carbon_emissions.csv', 'fertility-two-countries-example.csv', 'life-expectancy-and-fertility-two-countries-example.csv', 'olive.csv', 'murders.csv', 'ssa-death-probability.csv')
dir <- system.file('extdata', package='dslabs')
fullpath <- file.path(dir, filename)
file.copy(fullpath, filename)
#[1]  TRUE  TRUE  TRUE  TRUE FALSE  TRUE
read_csv('carbon_emissions.csv', n_max=5)
read_csv('fertility-two-countries-example.csv', n_max=5)
read_csv('ssa-death-probability.csv', n_max=5)
read_csv('olive.csv')

#2. Note that the the olive file, gives us a warning. This is because the first 
#line of the file is missing the header for the first column.  Read the help file for read_csv to figure out how to read in the file without reading this
#header. If you skip the header, you should not get this warning. Save the result 
#to an object called dat.
?read_csv
dat <- read_csv('olive.csv', skip=1)

#Use the readLines function to read in just the first line (we later learn how 
#to extract values from the output).
read_lines("olive.csv", skip=0, n_max=1)