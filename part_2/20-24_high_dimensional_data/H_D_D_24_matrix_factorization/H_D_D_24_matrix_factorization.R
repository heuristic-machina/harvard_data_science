#High Dimensional Data 24
#Matrix Factorization

#Exercises 24.5

#SVD to estimate factors
#Dataset of grade scores for 100 students in 24 subjects
#Percentage points received: 0=C, 25=A+, -25=F

#Analysis:Are all students just about as good? Does being 
#good in one subject imply one will be good in another? How 
#does the SVD help with all this?

set.seed(1987)
n <- 100
k <- 8
Sigma <- 64  * matrix(c(1, .75, .5, .75, 1, .5, .5, .5, 1),
                      3, 3) 
m <- MASS::mvrnorm(n, rep(0, 3), Sigma)
m <- m[order(rowMeans(m), decreasing = TRUE),]
y <- m %x% matrix(rep(1, k), nrow = 1) +
  matrix(rnorm(matrix(n * k * 3)), n, k * 3)
colnames(y) <- c(paste(rep("Math",k), 1:k, sep="_"),
                 paste(rep("Science",k), 1:k, sep="_"),
                 paste(rep("Arts",k), 1:k, sep="_"))

#plot shows x by subject
par(mar = c(8, 5, 4, 2))  # More space at bottom
plot(ss_y, type = "b", pch = 19, col = "steelblue",
     xaxt = "n", xlab = "", ylab = "Sum of Squares",
     main = "Sum of Squares per Subject Column")
axis(1, at = 1:length(ss_y), labels = colnames(y),
     las = 2, cex.axis = 0.7)
my_image <- function(x, zlim = range(x), ...){
  par(mar = c(8, 5, 4, 2))
  colors <- rev(RColorBrewer::brewer.pal(9, "RdBu"))
  cols <- 1:ncol(x)
  rows <- 1:nrow(x)
  image(cols, rows, t(x[rev(rows), , drop = FALSE]),
        xaxt = "n", yaxt = "n",
        xlab = "", ylab = "", col = colors, zlim = zlim, ...)
  abline(h = rows + 0.5, v = cols + 0.5)
  axis(side = 1, at = cols, labels = colnames(x), las = 2, cex.axis = 0.7)
}

#1. How would you describe the data based on this figure?

#The students that test well are at the top of the image and
# there seems to be three groupings by subject.


#2 Examine the correlation between the test scores directly:
my_image(cor(y), zlim = c(-1,1))
range(cor(y))
#[1] 0.4855371 1.0000000
axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2)

#Which of the following best describes what you see?

#There is a correlation among all tests, but higher if the tests
# are in science and math and even higher within each subject.


#3 Compute the sum of squares of the columns of Y and store them
# in ss_y. Then compute the sum of squares of columns of the 
#transformed YV and store them in ss_yv. Confirm that sum(ss_y)
# is equal to sum(ss_yv).
s <- svd(y)
names(s)
#[1] "d" "u" "v"

#y_svd <- sweep(s$u, d) %*% t(s$v)
#given code object 'd' not found error

#Copilot fix
y_svd <- s$u %*% diag(s$d) %*% t(s$v)
max(abs(y - y_svd))
#[1] 7.105427e-14

ss_y<-apply((y)^2, 2, sum)
sum(ss_y)
ss_yv<-apply((y%*%s$v)^2,2,sum)
sum(ss_yv)
#[1] 175434.6
ss_y
#[1] 175434.6

isTRUE(all.equal(sum(ss_y), sum(ss_yv)))
#[1] TRUE

#4. We see that the total sum of squares is preserved. This
# is because V is orthogonal. Now to start understanding how YV
#is useful, plot ss_y against the column number and then do the
# same for ss_yv. What do you observe?

#resetting margins after heatmap plot
#plot shows x as index
par(mar = c(5, 5, 4, 2))

plot(ss_y, type = "b", pch = 19, col = "steelblue",
      xlab = "Column Index", ylab = "Sum of Squares",
      main = "Sum of Squares per Subject Column")

range(ss_y)
#[1] 6936.943 7751.430

#improved graph
par(mar = c(8, 5, 4, 2))  # More space at bottom
plot(ss_y, type = "b", pch = 19, col = "steelblue",
      xaxt = "n", xlab = "", ylab = "Sum of Squares",
      main = "Sum of Squares per Subject Column")
axis(1, at = 1:length(ss_y),
     labels = colnames(y), las = 2, cex.axis = 0.7)

plot(ss_yv, type = "b", pch = 19, col = "steelblue",
     xaxt = "n", xlab = "", ylab = "Sum of Squares",
     main = "Sum of Squares per Right Singular Vector V")
axis(1, at = 1:length(ss_yv), labels = paste0("V", 1:length(ss_yv)), las = 2, cex.axis = 0.7)

 


