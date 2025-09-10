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
