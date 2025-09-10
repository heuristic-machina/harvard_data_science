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