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

#3. Add the scalar 1 to row 1, the scalar 2 to row 2, and 
#so on, to the matrix x.

add_scalar_row<- sweep(x, 1, 1:nrow(x), FUN='+')

#4. Add the scalar 1 to column 1, the scalar 2 to column 
#2, and so on, to the matrix x. Hint: Use sweep with FUN = "+".
add_scalar_col<- sweep(x, 1, 2:ncol(x), FUN='+')

#5. Compute the average of each row of x.
avg_row<-rowMeans(x)

#6. Compute the average of each column of x.
avg_col<-colMeans(x)

#7. For each digit in the MNIST training data, compute the
# proportion of pixels that are in a grey area, defined as
# values between 50 and 205. Make a boxplot by digit class. 
#Hint: Use logical operators and rowMeans.
library(dslabs)
library(ggplot2)

# Extract training images and labels
x <- mnist$train$images   # 60000 x 784 matrix
y <- mnist$train$labels   # vector of length 60000

# 1. Logical mask for grey pixels
grey_mask <- x >= 50 & x <= 205

# 2. Proportion of grey pixels per image
grey_prop <- rowMeans(grey_mask)

# 3. Combine with labels
df <- data.frame(
  digit = factor(y, levels = 0:9),
  grey_prop = grey_prop
)

# 4. Boxplot by digit
ggplot(df, aes(x = digit, y = grey_prop)) +
  geom_boxplot(fill = "lightblue") +
  labs(
    title = "Proportion of Grey Pixels (50–205) by MNIST Digit",
    x = "Digit",
    y = "Proportion of Grey Pixels"
  ) +
  theme_minimal()
