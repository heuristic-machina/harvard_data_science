#High dimensional data 20: Matrices

library(tidyverse)
library(dslabs)
table(mnist$train$labels)
#> 
#>    0    1    2    3    4    5    6    7    8    9 
#> 5923 6742 5958 6131 5842 5421 5918 6265 5851 5949
#> 
library(ggplot2)

label_counts <- as.data.frame(table(mnist$train$labels))
colnames(label_counts) <- c("digit", "count")

ggplot(label_counts, aes(x = digit, y = count)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = count), vjust = -0.3, size = 3.5) +
  labs(
    title = "MNIST Training Set Label Distribution",
    x = "Digit",
    y = "Number of Images"
  ) +
  theme_minimal()

#Exercises 20.11

#1. Create a 100 by 10 matrix of randomly generated normal 
#numbers. Put the result in x.
set.seed(123)
x<-matrix(rnorm(100*10, 100, 2))

#2. Apply the three R functions that give you the dimension 
#of x, the number of rows of x, and the number of columns of
# x, respectively.
x <- matrix(rnorm(100 * 10, mean = 100, sd = 2), 
            nrow = 100, ncol = 10)

nrow(x)
#[1] 100
ncol(x)
#[1] 10
dim(x)
#[1] 100  10
