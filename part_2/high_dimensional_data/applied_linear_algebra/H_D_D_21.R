#High Dimensional Data 21
#Applied Linear Algebra

#Exercises 21.5
#1. Generate two matrix, A and B, containing randomly 
#generated and normally distributed numbers. The dimensions 
#of these two matrices should be 4x3 and 3x6, respectively. 
#Confirm that C <- A %*% B produces the same results as:
set.seed(2)
A<-matrix(rnorm(4*3, 100, 2), 
          nrow = 4, ncol = 3)
B<-matrix(rnorm(3*6, 100, 2), 
          nrow = 3, ncol = 6)

m <- nrow(A)
p <- ncol(B)
C <- matrix(0, m, p)
for(i in 1:m){
  for(j in 1:p){
    C[i,j] <- sum(A[i,] * B[,j])
  }
}

all.equal(C, A %*% B)
#[1] TRUE

#2. Solve the following system of equations using R:
#x + y + z + w = 10
#2x + 3y - z - w =5
#3x - y + 4z - 2w = 15
#2x + 2y - 2z - 2w = 20

D<-matrix(c(1, 1, 1, 1, 2, 3,
            -1, -1, 3, -1, 4,
            -2, 2, 2, -2, -2), 4, 4, byrow=TRUE)
e<-matrix(c(10, 5, 15, 20))
solve(D, e)
#      [,1]
#[1,]  25.0
#[2,] -15.0
#[3,] -12.5
#[4,]  12.5

#3. Define x:
mnist <- read_mnist()
x <- mnist$train$images[1:300,] 
y <- mnist$train$labels[1:300]
#and compute the distance matrix:
d <- dist(x)
class(d)
#Generate a boxplot showing the distances for the second 
#row of d stratified by digits. Do not include the distance 
#to itself, which we know is 0. Can you predict what digit 
#is represented by the second row of x?

#2nd row of x predicted digit using y
y[2]
#[1] 0

#Copilot explanation of stratify by digits:
#'for image 2, how far is it in euclidean pixel space from every other image,
#other image, and how do those distances look when grouped by
#the true digit label of the other images?
#Row 2 of d contains the distances from image 2 to all other images in 
#subset (1-300)'
# Convert dist object to full matrix
dmat <- as.matrix(d)

# Distances from image 2 to all others
dist_row2 <- dmat[2, ]

# Combine with labels
df <- data.frame(
  digit = factor(y),
  distance = dist_row2
)

# Remove self-distance (0)
df <- df[df$distance != 0, ]

# Boxplot of distances by digit
library(ggplot2)
ggplot(df, aes(x = digit, y = distance)) +
  geom_boxplot(fill = "lightblue") +
  labs(
    title = paste("Distances from Image 2 (Digit", y[2], ") by Digit Class"),
    x = "Digit Class",
    y = "Euclidean Distance"
  ) +
  theme_minimal()

#4. Use the apply function and matrix algebra to compute the 
#distance between the second digit mnist$train$images[4,] and 
#all other digits represented in mnist$train$images. Then 
#generate a boxplot as in exercise 2 and predict what digit is 
#the fourth row.

# Convert dist object to full matrix
dmat <- as.matrix(d)

# Distances from image 4 to all others
dist_row4 <- dmat[4, ]

# Combine with labels
df_4 <- data.frame(
  digit = factor(y),
  distance = dist_row4
)

# Remove self-distance (0)
df_4 <- df[df$distance != 0, ]

# Boxplot of distances by digit
library(ggplot2)
ggplot(df_4, aes(x = digit, y = distance)) +
  geom_boxplot(fill = "lightblue") +
  labs(
    title = paste("Distances from Image 4 (Digit", y[4], ") by Digit Class"),
    x = "Digit Class",
    y = "Euclidean Distance"
  ) +
  theme_minimal()

#using apply() for digit
# Row 2 as a vector
target <- x[4, ]

# Euclidean distance to each row using apply()
dists <- apply(x, 1, function(row) sqrt(sum((row - target)^2)))

# Ignore self-distance
dists[4] <- Inf

# Index of closest image
closest_idx <- which.min(dists)

# Predicted digit
predicted_digit <- y[closest_idx]
predicted_digit
