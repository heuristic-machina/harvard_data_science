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

all.equal(ss_y, ss_yv, check.attributes = FALSE)
# "Mean relative difference: 1.73222"
#[1] TRUE
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
axis(1, at = 1:length(ss_yv),
     labels = paste0("V", 1:length(ss_yv)),
     las = 2, cex.axis = 0.7)

#Alternate plot
plot(s$d^2, type = "b", pch = 19, col = "firebrick",
      xlab = "Component Index", ylab = "Variance Explained",
      main = "Singular Values Squared")

#Copilot:So when you see the first few singular vectors dominate
# in ss_yv, it’s telling you:
#Your data lives mostly in a low-dimensional subspace
#The remaining directions (like V5–V24) contribute very little
# — similar to how lower rows in echelon form may be all zeros
#In that sense, the first few singular vectors are analogous 
#to the pivot columns in echelon form — they’re the ones that 
#carry the signal.

#5. We see that the variability of the columns of YV is 
#decreasing. Furthermore, we see that, relative to the first 
#three, the variability of the columns beyond the third is 
#almost 0. Now notice that we didn’t have to compute ss_yv 
#because we already have the answer. How? Remember that YV=UD
#and because U is orthogonal, we know that the sum of squares 
#of the columns of UD are the diagonal entries of D squared. 
#Confirm this by plotting the square root of ss_yv versus the 
#diagonal entries of D.
ss_ud <- apply((s$u %*% diag(s$d))^2, 2, sum)

all.equal(ss_ud, ss_yv, check.attributes = FALSE)
#[1] TRUE

#6. From the above we know that the sum of squares of the
#columns of Y (the total sum of squares) add up to the sum of 
#s$d^2, and that the transformation YV gives us columns with sums 
#of squares equal to s$d^2. Now compute what percent of the 
#total variability is explained by just the first three columns
# of YV.

sum(s$d[1:3]^2) / sum(s$d^2)
#[1] 0.9877922

#7 Use the sweep function to compute UD without constructing
# diag(s$d) and without using matrix multiplication.
identical(s$u %*% diag(s$d), sweep(s$u, 2, s$d, FUN = "*"))
#[1] TRUE

#8 Compute the average score for each student and plot it
# against U1D1,1 , and describe what you find.
rowMeans(y)
UD <- sweep(s$u, 2, s$d, FUN = "*")

par(mar = c(5, 5, 4, 2))
plot(UD[,1], rowMeans(y), type = "b",
     pch = 19, col = "firebrick",
     xlab = "UD[,1]", ylab = "rowMeans(y)")

#9 we see that the first column of UD is almost identical to 
#the average score for each student except for the sign. This 
#implies that multiplying Y by the first column of V must be 
#performing a similar operation to taking the average. Make 
#an image plot of V and describe the first column relative to 
#others and how this relates to taking an average.

my_image(s$v)
#The first column is very close to being a constant, which 
#implies that the first column of YV is the sum of the rows 
#of Y multiplied by some constant, and is thus proportional 
#to an average.

#10 We already saw that we can rewrite UD as:
#   u1d1,1 + u2d2,2 + ... + updp,p
#with uj the j-th column of U.  This implies that we can 
#rewrite the entire SVD as:
#   Y = u1d1,1v1T + u2d2,2v2T + ... + updp,pvpT
#with Vj the jth column of V.  Plot u1, then plot v1T using the
#same range for the y-axis limits.  Then make an image of 
#u1d1,1v1T and compare it to the image of Y.  Hint:  Use the
#my_image function defined above and use the drop=FALSE argument
#to assure the subsets of matrices are matrices.
plot(s$u[,1], ylim = c(-0.25, 0.25),type = "b",
     pch = 19, col = "firebrick",
     xlab = "Index", ylab = "s$u[,1]", main= "U1")
plot(s$v[,1], ylim = c(-0.25, 0.25),type = "b",
     pch = 19, col = "firebrick",
     xlab = "Index", ylab = "s$v[,1]", main= "V1")

#heatmap comparison
with(s, my_image((u[, 1, drop=FALSE]*d[1]) %*% t(v[, 1, drop=FALSE])))

# 11. We see that with just a vector of length 100, a scalar, 
#and a vector of length 24, we actually come close to 
#reconstructing the original 100x24 matrix. This is our first
# matrix factorization:

#   Y = d1,1u1v1T

#We know it explains s$d[1]^2/sum(s$d^2) * 100 percent of the 
#total variability. Our approximation only explains the 
#observation that good students tend to be good in all subjects.
# But another aspect of the original data that our approximation
# does not explain was the higher similarity we observed within
# subjects. We can see this by computing the difference between
# our approximation and original data and then computing the 
#correlations. You can see this by running this code:

resid <- y - with(s,(u[,1, drop=FALSE]*d[1]) %*%
                    t(v[,1, drop=FALSE]))
my_image(cor(resid), zlim = c(-1,1))
axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2)

#Now that we have removed the overall student effect, the 
#correlation plot reveals that we have not yet explained the
# within subject correlation nor the fact that math and science
# are closer to each other than to the arts. So let’s explore 
#the second column of the SVD. Repeat the previous exercise 
#but for the second column: Plot U2, then plot V2T using the 
#same range for the y-axis limits, then make an image of 
#U2D2,2V2T and compare it to the image of resid.

plot(s$u[,2], ylim = c(-0.5, 0.5),type = "b",
     pch = 19, col = "firebrick",
     xlab = "Index", ylab = "s$u[,2]", main= "U2")

plot(s$v[,2], ylim = c(-0.5, 0.5),type = "b",
     pch = 19, col = "firebrick",
     xlab = "Index", ylab = "s$v[,2]", main= "V2T")

with(s, my_image((u[, 2, drop=FALSE]*d[2]) %*% t(v[, 2, drop=FALSE])))

my_image(resid)

#12 The second column clearly relates to a student’s difference
# in ability in math/science versus the arts. We can see this
# most clearly from the plot of s$v[,2]. Adding the matrix we
# obtain with these two columns will help with our approximation:

#   Y = D1,1U1V1T = D2,2U2V2T

#We know it will explain 
#sum(s$d[1:2]^2)/sum(s$d^2) * 100 percent 
#of the total variability. We can compute new residuals like
# this:

resid <- y - with(s,sweep(u[, 1:2], 2, d[1:2], FUN="*") %*%
                    t(v[, 1:2]))
my_image(cor(resid), zlim = c(-1,1))
axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2)

#and see that the structure that is left is driven by the
# differences between math and science. Confirm this by 
#plotting U3, then plot V3T using the same range for the y-axis
# limits, then make an image of U3D3,3V3T and compare it to 
#the image of resid.


plot(s$u[,3], ylim = c(-0.5, 0.5),type = "b",
     pch = 19, col = "firebrick",
     xlab = "Index", ylab = "s$u[,3]", main= "U3")

plot(s$v[,3], ylim = c(-0.5, 0.5),type = "b",
     pch = 19, col = "firebrick",
     xlab = "Index", ylab = "s$u[,3]", main= "V3")

with(s, my_image((u[, 3, drop=FALSE]*d[3]) %*% t(v[, 3, drop=FALSE])))

my_image(resid)


#13 The third column clearly relates to a student’s difference
# in ability in math and science. We can see this most clearly
# from the plot of s$v[,3]. Adding the matrix we obtain with
# these two columns will help with our approximation:

#   Y = D1,1U1V1T + D2,2U2V2T + D3,3U3V3T

#We know it will explain: 
#sum(s$d[1:3]^2)/sum(s$d^2) * 100 percent of the total 
#variability. We can compute new residuals like this:

resid <- y - with(s,sweep(u[, 1:3], 2, d[1:3], FUN="*") %*% t(v[, 1:3]))
my_image(cor(resid), zlim = c(-1,1))
axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2)

#We no longer see structure in the residuals: they seem to be
# independent of each other. This implies that we can describe
# the data with the following model:
#   Y = D1,1U1V1T + D2,2U2V2T + D3,3U3V3T + E

#with E a matrix of independent identically distributed errors.
# This model is useful because we summarize 100x24 observations
# with 3 x (100+24+1) numbers.

#Furthermore, the three components of the model have useful 
#interpretations: 1) the overall ability of a student, 2) the 
#difference in ability between the math/sciences and arts, 
#and 3) the remaining differences between the three subjects.

#The sizes D1,1, D2,2, D3,3 tell us the variability explained
# by each component. Finally, note that the components
# DJ,JUJVJ are equivalent to the jth principal component.

#Finish the exercise by plotting an image of Y, an image of
# D1,1U1V1 + D2,2U2V2 + D3,3U3V3 and an image of the residuals,
#all with the the same zlim.

y_hat <- with(s,sweep(u[, 1:3], 2,
                      d[1:3], FUN="*") %*% t(v[, 1:3]))
my_image(y, zlim = range(y))

my_image(y_hat, zlim = range(y))

my_image(y - y_hat, zlim = range(y))