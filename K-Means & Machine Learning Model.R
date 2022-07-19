## load required libraries
library(dplyr)
library(ggplot2)

##Unsupervised leanring: k-means clustering

iris_numerics <- select(iris, -Species)%>%
  scale() #so scale basically sets everything in relation to the mean
iris_numerics
summary(iris_numerics)

iris_clusters <- kmeans(iris_numerics, centers = 3)
iris_clusters #our results


iris_clusters$cluster #vector designating a cluster for each row
iris$cluster <- iris_clusters$cluster #add cluster column to original dataset


ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width))+
  geom_point(aes(color = as.factor(cluster)))
