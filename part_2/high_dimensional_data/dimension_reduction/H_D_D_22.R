#High dimensional data 22
#Dimenstion reduction

#Exercises 22.7

#1. We want to explore the tissue_gene_expression predictors by 
#plotting them.
dim(tissue_gene_expression$x)
#[1] 189 500 
names(tissue_gene_expression) 
#[1] "x" "y"
table(tissue_gene_expression$y)
#or levels(y)

# cerebellum       colon endometrium hippocampus      kidney       liver 
#         38          34          15          31          39          26 
#   placenta 
#          6 

#We hope to get an idea of which observations are close to each 
#other, but the predictors are 500-dimensional so plotting is 
#difficult. Plot the first two principal components with color 
#representing tissue type.

#Copilot:  
#189(rows) observations, 500(columns) predictors(gene expression levels),
#y for tissue type
library(dslabs)
library(ggplot2)

# Extract predictors and tissue labels
x <- tissue_gene_expression$x
y <- tissue_gene_expression$y

# Perform PCA
pca <- prcomp(x, center = TRUE, scale. = TRUE)

# Create a data frame with the first two PCs and tissue type
df <- data.frame(
  PC1 = pca$x[, 1],
  PC2 = pca$x[, 2],
  tissue = y
)

# Plot
ggplot(df, aes(x = PC1, y = PC2, color = tissue)) +
  geom_point(size = 3, alpha = 0.8) +
  labs(
    title = "Tissue Gene Expression: First Two Principal Components",
    x = "PC1",
    y = "PC2"
  ) +
  theme_minimal() +
  theme(legend.position = "right")
