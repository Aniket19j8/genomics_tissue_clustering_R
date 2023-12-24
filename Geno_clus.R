# Install and load necessary packages
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}

library(ggplot2)

# Generate synthetic genomics dataset
set.seed(42)
genomics_data <- matrix(rnorm(600), ncol = 2)
colnames(genomics_data) <- c("Gene_1", "Gene_2")

# Apply PCA for dimensionality reduction
pca_result <- prcomp(genomics_data, center = TRUE, scale. = TRUE)
pca_data <- as.data.frame(pca_result$x[, 1:2])

# Apply hierarchical clustering
hc_labels <- cutree(hclust(dist(pca_data)), k = 3)

# Apply k-means clustering
kmeans_labels <- kmeans(pca_data, centers = 3)$cluster

# Visualize the results
par(mfrow = c(1, 2))

plot(pca_data, col = hc_labels, pch = 16, main = "Hierarchical Clustering", xlab = "PC1", ylab = "PC2")
points(pca_result$center[, 1:2], col = 1:3, pch = 8, cex = 2)

plot(pca_data, col = kmeans_labels, pch = 16, main = "K-Means Clustering", xlab = "PC1", ylab = "PC2")
points(pca_result$center[, 1:2], col = 1:3, pch = 8, cex = 2)

# Generate ground truth labels
set.seed(123)
true_labels <- sample(1:3, 200, replace = TRUE)

# Calculate accuracy for hierarchical clustering
accuracy_hc <- sum(hc_labels == true_labels) / length(true_labels)

# Calculate accuracy for k-means clustering
accuracy_kmeans <- sum(kmeans_labels == true_labels) / length(true_labels)

cat("Accuracy for Hierarchical Clustering:", round(accuracy_hc, 2), "\n")
cat("Accuracy for K-Means Clustering:", round(accuracy_kmeans, 2), "\n")
