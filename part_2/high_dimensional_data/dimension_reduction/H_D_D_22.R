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

#2. The predictors for each observation are measured on the same 
#measurement device (a gene expression microarray) after an experimental
# procedure. A different device and procedure is used for each observation.
# This may introduce biases that affect all predictors for each observation
# in the same way. To explore the effect of this potential bias, for each 
#observation, compute the average across all predictors and then plot this
# against the first PC with color representing tissue. Report the correlation.
library(dslabs)
library(ggplot2)

# Extract predictors and tissue labels
x <- tissue_gene_expression$x
y <- tissue_gene_expression$y

# Perform PCA
pca <- prcomp(x, center = TRUE, scale. = TRUE)

# Bias of predictors (mean per sample)
avg_per_sample <- rowMeans(x)

# Create a data frame
df <- data.frame(
  PC1 = pca$x[, 1],
  bias = avg_per_sample,
  tissue = y
)

# Compute Pearson correlation
cor_val <- cor(df$PC1, df$bias)

# Plot with correlation annotation
ggplot(df, aes(x = PC1, y = bias, color = tissue)) +
  geom_point(size = 3, alpha = 0.8) +
  annotate("text",
           x = min(df$PC1), 
           y = max(df$bias),
           label = paste0("Pearson r = ", round(cor_val, 2)),
           hjust = 0, vjust = 1, size = 5,
           fontface = "bold",
           color = "black") +
  labs(
    title = "Tissue Gene Expression: First Principal Component and Bias",
    x = "PC1",
    y = "Bias"
  ) +
  theme_minimal() +
  theme(legend.position = "right")


#3. We see an association with the first PC and the observation
# averages. Redo the PCA, but only after removing the center.

#side by side with and without centering
library(dslabs)
library(ggplot2)
data(tissue_gene_expression)

x <- tissue_gene_expression$x
y <- tissue_gene_expression$y

# PCA with centering
pca_centered <- prcomp(x, center = TRUE, scale. = TRUE)

# PCA without centering
pca_uncentered <- prcomp(x, center = FALSE, scale. = TRUE)

# Combine into one data frame for plotting
df_centered <- data.frame(
  PC1 = pca_centered$x[, 1],
  PC2 = pca_centered$x[, 2],
  tissue = y,
  type = "Centered"
)

df_uncentered <- data.frame(
  PC1 = pca_uncentered$x[, 1],
  PC2 = pca_uncentered$x[, 2],
  tissue = y,
  type = "Uncentered"
)

df_all <- rbind(df_centered, df_uncentered)

# Plot side-by-side
ggplot(df_all, aes(x = PC1, y = PC2, color = tissue)) +
  geom_point(size = 2, alpha = 0.8) +
  facet_wrap(~ type, scales = "free") +
  labs(
    title = "PCA with vs. without Centering",
    x = "PC1",
    y = "PC2"
  ) +
  theme_minimal()

