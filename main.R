#Retreiving data
library(datasets)
library(kernlab)
library(MASS)
flowerData=iris
data(iris)
set.seed(42)
#----------------------------------------------------------
#Exploring data
#----------------------------------------------------------
print("SUMMARY OF DATASET")
#summary(flowerData)
#print(head(flowerData))
#print(tail(flowerData))
#----------------------------------------------------------
#Data Visualization and data is stored in 'plot.pdf' file in the working directory
#----------------------------------------------------------
plot(flowerData$Sepal.Length, col=c("red","blue","green"))
plot(flowerData$Sepal.Width, col=c("red","blue","green"))
plot(flowerData$Petal.Length, col=c("red","blue","green"))
plot(flowerData$Petal.Width, col=c("red","blue","green"))
#----------------------------------------------------------
#Accuracy function
#----------------------------------------------------------
accuracy<-function(predicted_seq, actual_seq){
  count<-0
  size<-0
  if(length(predicted_seq)==length(actual_seq)){
    size<-length(predicted_seq)
  }
  else{
    print("Sequence size does not match!")
  }
  for(i in 1:size){
    if(predicted_seq[i]==actual_seq[i]){
      count<-count+1
    }
  }
  return (count*100/size)
}
#----------------------------------------------------------
#Shuffle dataset row-wise
#----------------------------------------------------------
shuffledFlowerData<-flowerData[sample(nrow(flowerData)),]
#----------------------------------------------------------
#Seperate dataset into training and testing
#The data is seperated in 75/25 training/testing ratio
#----------------------------------------------------------
datasetSize<-nrow(shuffledFlowerData)
#traininng data
trainingDatasetSize<-as.integer(0.75*datasetSize)
trainingDataset=shuffledFlowerData[1:trainingDatasetSize,]
#testing data
testingDatasetSize<-datasetSize-trainingDatasetSize
testingDataset<-shuffledFlowerData[113:150,]
#----------------------------------------------------------
#Classification without dimensionality reduction
#----------------------------------------------------------
fit<-ksvm(Species~., data=trainingDataset)
#summary(fit)
prediction<-predict(fit, testingDataset, type='response')
print("The accuracy of the SVM is as shown below:")
print(accuracy(prediction,testingDataset[,5]))
#----------------------------------------------------------
#print('Principal Component Analysis and classification')
#----------------------------------------------------------
shuffledFlowerData.pca.eigenvalues<-princomp(x=shuffledFlowerData[,1:4], scores = TRUE)
shuffledFlowerData.pca.eigenvectors<-shuffledFlowerData.pca.eigenvalues$scores
plot(shuffledFlowerData.pca.eigenvectors[,1], col=c("red","blue","green"), ylab = "PCA component 1")
mostUsefulData<-shuffledFlowerData.pca.eigenvectors[,1]
postPCADataset<-cbind(mostUsefulData, shuffledFlowerData['Species'])
#print(postPCADataset)
#----------------------------------------------------------
#Dividing PCA data into test and training
#----------------------------------------------------------
#traininng data
trainingDataset.pca<-postPCADataset[1:trainingDatasetSize,]
#print(trainingDataset.pca)
#testing data
testingDataset.pca<-postPCADataset[113:150,]
#----------------------------------------------------------
#Classification of post-PCA data
#----------------------------------------------------------
fit.pca<-ksvm(Species~., data=trainingDataset.pca)
#summary(fit.pca)
prediction.pca<-predict(fit.pca, testingDataset.pca, type='response')
print("The accuracy of the SVM after PCA is as shown below:")
print(accuracy(prediction.pca,testingDataset.pca[,2]))
#----------------------------------------------------------
#Fischers Linear Discriminant and classification
#----------------------------------------------------------
x <- cbind(iris$Petal.Length,iris$Petal.Width) 
Y <- ifelse(iris$Species == "virginica", +1, -1) 
u <- apply(x,2,mean) 
up <- apply(subset(x,Y==+1),2,mean) 
un <- apply(subset(x,Y==-1),2,mean) 
np <- sum(Y==+1)
nn <- sum(Y==-1) 
SB <- nn * (un - u) %*% t(up - u)
distance.from.plane = function(x,w,b) {
  b - sum(x*w)
}
classify.fisher = function(x,w,b){
  distances =
    apply(x, 1, distance.from.plane, w, b)
  return(ifelse(distances < 0, -1, +1))
}
scatter <- function(v){ 
( (v - un) %*% t(v - un) ) + ( (v - up) %*% t(v - up) ) 
} 
SW <- matrix( apply( apply(x,1,scatter), 1, sum ), nrow=2 ) 
t <- seq(- pi , pi, 0.05)
uv <- cbind(cos(t), sin(t)) 
action <- function(uv, m){
abs( uv %*% m %*% matrix(uv) )
} 
ratios <- apply(uv, 1, action, SB) / apply(uv, 1, action, SW)  
mr <- which.max(ratios) 
muv <- uv[mr,] 
mv <- 40*ratios[mr]*muv 
xp <- as.vector(x %*% muv) 
rxp <- round(range(xp),0)+c(-1,1) 
xpn <- subset(xp,Y==-1) 
xpp <- subset(xp,Y==+1)
b = (mean(xpp) * sd(xpn) + mean(xpn) * sd(xpp)) / + (sd(xpp) + sd(xpn)) 
plot(x,col=Y+3,asp=1) > par(lwd=2)
abline(b/muv[2],-muv[1]/muv[2]) 
sum(abs(Y - classify.fisher(x,muv,b) ))
#Applying LDA For Separating Class 'Virginica' separately from other classes(Partial Dataset Consideration)
X <- data.frame(length=iris$Petal.Length,width=iris$Petal.Width)
X$class <- ifelse(iris$Species == "virginica", +1, -1) 
LDA <- lda(class ~ length + width, data = X) 
X$pred <- predict(LDA)$class
X$col <- 2+ifelse(X$pred == X$class, as.numeric(X$class), 0) 
color <- c("red","green", "blue","red","blue")
plot(X$length,X$width,col=color[X$col],pch=2,cex=1)
pts <- 3000 
rpts <- function(v,pts){
runif(pts,min=v[1],max=v[2]) 
} 
grid <- data.frame(apply(apply(X[,1:2],2,range),2,rpts,pts)) 
grid$pred <- 3+as.numeric(predict(LDA,grid)$class) 
points(grid$length,grid$width,col=color[grid$pred],cex=0.1)
sum(X$class != X$pred)
#Applying LDA For all three classes Simultaenously(Full Dataset Consideration)
LDA <- lda(Species ~ ., data = iris) 
X <- data.frame(predict(LDA)$x) 
X$class <- factor(ifelse(predict(LDA)$class == iris$Species, + as.numeric(iris$Species), 0)) 
library(ggplot2, warn.conflicts = FALSE) 
color <- c("red", "green", "blue","purple") 
pts <- 50000
cube <- data.frame(apply(apply(iris[,1:4],2,range),2,rpts,pts)) 
cube.pred <- predict(LDA,cube) 
X.grid <- data.frame(cube.pred$x) 
X.grid$class <- factor(as.numeric(cube.pred$class)) 
p <- ggplot(X, aes(x = LD1, y = LD2))+
geom_point(aes(colour=class),size=3) 
p + geom_point(data=X.grid,aes(colour=class),size=0.5) 
print("Accuracy after FLD classification")
print(accuracy(iris$Species,predict(LDA)$class))
#----------------------------------------------------------
#k-Nearest Neighbour algorithm and classification
#----------------------------------------------------------
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
