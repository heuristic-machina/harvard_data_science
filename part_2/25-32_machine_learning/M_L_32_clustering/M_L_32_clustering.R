#Machine Learning 32: Clustering


library(dslabs)
data('tissue_gene_expression')
dim(tissue_gene_expression$x)
#[1] 189 500
#The matrix has the gene expression levels of 500 genes from 189
#biological samples representing seven different tissues. The tissue
#type is stored in y
table(tissue_gene_expression$y)
# cerebellum       colon endometrium hippocampus      kidney       liver 
#38                 34          15          31          39          26 
#placenta 
#6 

#1. Load the tissue_gene_expression dataset. Remove the row means 
#and compute the distance between each observation. Store the 
#result in d.

x_centered <- sweep(tissue_gene_expression$x,
                    1,
                    rowMeans(tissue_gene_expression$x))
d <- dist(x_centered)

#2. Make a hierarchical clustering plot and add the tissue types 
#as labels.
h <- hclust(d)
plot(h, cex = 0.65)
abline(h = 100, col = "red", lty = 2)


#3. Run a k-means clustering on the data with K=7. Make 
#a table comparing the identified clusters to the actual 
#tissue types. Run the algorithm several times to see how
#the answer changes.

set.seed(42)
k <- kmeans(x_centered, centers = 7)
table(tissue_gene_expression$y, k$cluster)

#              1  2  3  4  5  6  7
# cerebellum   2  0 36  0  0  0  0
# colon        0 34  0  0  0  0  0
# endometrium  0  0  0 15  0  0  0
# hippocampus  0  0 31  0  0  0  0
# kidney       3  0  0  0 36  0  0
# liver        2  0  0  0  0 24  0
# placenta     0  0  0  0  0  0  6

#PCA clustering visual
par(mar = c(5, 5, 2, 2))
pc <- prcomp(x_centered)
plot(pc$x[,1], pc$x[,2], col = k$cluster, pch = 19,
     xlab = "PC1", ylab = "PC2",
     main = "K-means Clustering (K=7)")

#visual tissue by color type
plot(pc$x[,1], pc$x[,2], col = as.numeric(tissue_gene_expression$y), pch = 19,
     xlab = "PC1", ylab = "PC2", main = "True Tissue Types")

#Comparing the 2 plots against eachother show the black dots
#move around the tissue types.  These black points are where
#clustering disagrees with the know tissue type.


#4. Make a heatmap of the 50 most variable genes. Make sure the 
#observations show up in the columns, that the predictors are 
#centered, and add a color bar to show the different tissue types. 
#Hint: use the ColSideColors argument to assign colors. Also, 
#use col = RColorBrewer::brewer.pal(11, "RdBu") for a better 
#use of colors.

library(matrixStats)
library(RColorBrewer)
sds <- colSds(tissue_gene_expression$x, na.rm = TRUE)
o <- order(sds, decreasing = TRUE)[1:50]
heatmap(tissue_gene_expression$x[,o],
        col = RColorBrewer::brewer.pal(11,"Spectral"))