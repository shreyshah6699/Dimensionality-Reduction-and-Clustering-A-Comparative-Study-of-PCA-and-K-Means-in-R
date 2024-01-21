rm(list = ls())


#a) Generate a simulated data set with 20 observations in each of three classes (i.e. 60
#observations total), and 50 variables

set.seed(123)
class1_data <- matrix(rnorm(20*50, mean = 0), ncol = 50)
class2_data <- matrix(rnorm(20*50, mean = 1), ncol = 50)
class3_data <- matrix(rnorm(20*50, mean = 2), ncol = 50)

combined_data <- rbind(class1_data, class2_data, class3_data)
data_labels <- factor(c(rep(1, 20), rep(2, 20), rep(3, 20)))

#b) Perform PCA on the 60 observations and plot the first two principal component score
#vectors. Use a different color to indicate the observations in each of the three classes.

pca_result <- prcomp(combined_data, center = TRUE, scale. = TRUE)

#c) Perform K-means clustering of the observations with K = 3. How well do the clusters that
#you obtained in K-means clustering compare to the true class labels?

plot(pca_result$x[,1:2], col = data_labels, pch = 19, xlab = "PC1", ylab = "PC2")
legend("topright", legend = c("Class 1", "Class 2", "Class 3"), col = 1:3, pch = 19)

km3_data <- kmeans(combined_data, centers = 3, nstart = 20)
table(data_labels, km3_data$cluster)

#d) Perform K-means clustering with K = 2. Describe your results

km2_data <- kmeans(combined_data, centers = 2, nstart = 20)
table(data_labels, km2_data$cluster)

#e) Now perform K-means clustering with K = 4, and describe your results.

km4_data <- kmeans(combined_data, centers = 4, nstart = 20)
table(data_labels, km4_data$cluster)

#f) Now perform K-means clustering with K = 3 on the first two principal component score
#vectors, rather than on the raw data. That is, perform K-means clustering on the 60 × 2
#matrix of which the first column is the first principal component score vector, and the
#second column is the second principal component score vector. Comment on the
#results.

km_pca_data <- kmeans(pca_result$x[,1:2], centers = 3, nstart = 20)
table(data_labels, km_pca_data$cluster)

#g) Using the scale() function, perform K-means clustering with K = 3 on the data after
#scaling each variable to have standard deviation one. How do these results compare to
#those obtained in (b)? Explain

scaled_data <- scale(combined_data)
km_scaled_data <- kmeans(scaled_data, centers = 3, nstart = 20)
table(data_labels, km_scaled_data$cluster)
