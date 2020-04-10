#Retreiving data
library(datasets)
library(kernlab)
flowerData=iris
set.seed(42)
#----------------------------------------------------------
#Exploring data
#----------------------------------------------------------
#print("SUMMARY OF DATASET")
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
summary(fit)
prediction<-predict(fit, testingDataset, type='response')
print("The accuracy of the SVM is as shown below:")
print(accuracy(prediction,testingDataset[,5]))
#----------------------------------------------------------
print('Principal Component Analysis and classification')
#----------------------------------------------------------
shuffledFlowerData.pca.eigenvalues<-princomp(x=shuffledFlowerData[,1:4], cors=TRUE, scores = TRUE)
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
print(trainingDataset.pca)
#testing data
testingDataset.pca<-postPCADataset[113:150,]
#----------------------------------------------------------
#Classification of post-PCA data
#----------------------------------------------------------
fit.pca<-ksvm(Species~., data=trainingDataset.pca)
#summary(fit.pca)
prediction.pca<-predict(fit.pca, testingDataset.pca, type='response')
print("The accuracy of the SVM is as shown below:")
print(accuracy(prediction.pca,testingDataset.pca[,2]))
#----------------------------------------------------------
#Fischers Linear Discriminant and classification
#----------------------------------------------------------









#----------------------------------------------------------
#Metric Learning and classification
#----------------------------------------------------------


















#--------------------------------------------------------
#Accuracy comparison on test set
#--------------------------------------------------------





