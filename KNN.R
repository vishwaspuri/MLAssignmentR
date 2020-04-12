#Following packages are required:
library(class)
library(ggplot2)
library(GGally)

#Getting a summary of the stats for the dataset:
summary(iris)

apply(iris[,1:4], 2, sd)

#Plotting the data using histogram:
par(mfrow=c(2,2))
hist(iris$Sepal.Length, col="blue", breaks=20)
hist(iris$Sepal.Width, col="blue", breaks=20)
hist(iris$Petal.Length, col="blue", breaks=20)
hist(iris$Petal.Width, col="blue", breaks=20)

#PLotting the data using scatter plots:
ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width, col = Species)) +
  geom_point()

ggplot(data = iris, aes(x = Petal.Length, y = Petal.Width, col = Species)) +
  geom_point()

#Correlation Matrix:
ggpairs(iris)

#Splitting the dataset:
set.seed(12420352)
iris[,1:4] <- scale(iris[,1:4])
setosa<- rbind(iris[iris$Species=="setosa",])
versicolor<- rbind(iris[iris$Species=="versicolor",])
virginica<- rbind(iris[iris$Species=="virginica",])

ind <- sample(1:nrow(setosa), nrow(setosa)*0.75)
iris.train<- rbind(setosa[ind,], versicolor[ind,], virginica[ind,])
iris.test<- rbind(setosa[-ind,], versicolor[-ind,], virginica[-ind,])
iris[,1:4] <- scale(iris[,1:4])

#Finding optimum value of K:
error <- c()
for (i in 1:15)
{
  knn.fit <- knn(train = iris.train[,1:4], test = iris.test[,1:4], cl = iris.train$Species, k = i)
  error[i] = 1- mean(knn.fit == iris.test$Species)
}

#Plotting error vs k-value:
ggplot(data = data.frame(error), aes(x = 1:15, y = error)) +
  geom_line(color = "Blue")

#The plot shows that minimum error is when value of k is equal to 12.
#We also get a local minima at k=5.
#Given our dataset, We chose a little less complex model and go with k=5.


#Getting the confusion matrix & accuracy of the model
iris_pred <- knn(train = iris.train[,1:4], test = iris.test[,1:4], cl = iris.train$Species, k=5)
table(iris.test$Species,iris_pred)
print("Accuracy after KNN classification")
print(accuracy(iris.test$Species,iris_pred))
