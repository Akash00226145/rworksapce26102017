library("foreign")
data(iris)
iris2 <- iris
iris2$Species <- NULL
kmeans.result <- kmeans(iris2, 3)
kmeans.result
kmeans.result$cluster
table(iris$Species, kmeans.result$cluster)
plot(iris2[c("Sepal.Length", "Sepal.Width")], col = kmeans.result$cluster)
# plot cluster centers
points(kmeans.result$centers[,c("Sepal.Length", "Sepal.Width")], col = 1:3, pch = 8, cex=2)


#Q2 
iris2 <- iris
iris2$Species <- NULL
set.seed(4)
kmeans.result <- kmeans(iris2, 3)
kmeans.result
kmeans.result$cluster
kmeans.result$tot.withinss
table(iris$Species, kmeans.result$cluster)
plot(iris2[c("Sepal.Length", "Sepal.Width")], col = kmeans.result$cluster)
# plot cluster centers
points(kmeans.result$centers[,c("Sepal.Length", "Sepal.Width")], col = 1:3, pch = 8, cex=2)
