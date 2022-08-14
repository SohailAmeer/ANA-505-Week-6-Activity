# ANA-505-Week-6-Activity
IRIS Dataset Classification using SVM, K Means and C50 models in R


#Support Vector Classification

install.packages("e1071")
install.packages("GGally")
install.packages("ggplot2")

library(e1071)
library(GGally)
library(ggplot2)

data(iris)

str(iris)
head(iris,3)

svm_model <- svm(Species ~ ., data=iris,
                 kernel="radial")
ggpairs(iris, ggplot2::aes(colour = Species, alpha = 0.4))

plot(svm_model, data=iris,
     Petal.Width~Petal.Length,
     slice = list(Sepal.Width=3, Sepal.Length=4) 
)

pred = predict(svm_model,iris)
tab = table(Predicted=pred, Actual = iris$Species)
tab

1-sum(diag(tab)/sum(tab))

 

print("The accuracy of SVM is 97.33% which is a good classification outcome")


#K-Means Clustering

require("datasets")
data("iris")
str(iris)

summary(iris)

head(iris, 3)

iris.new<- iris[,c(1,2,3,4)]
iris.class<- iris[,"Species"]
head(iris.new, 3)

head(iris.class, 3)

normalize <- function(x){
  return ((x-min(x))/(max(x)-min(x)))
}

iris.new$Sepal.Length<- normalize(iris.new$Sepal.Length)
iris.new$Sepal.Width<- normalize(iris.new$Sepal.Width)
iris.new$Petal.Length<- normalize(iris.new$Petal.Length)
iris.new$Petal.Width<- normalize(iris.new$Petal.Width)
head(iris.new)

result<- kmeans(iris.new,3)
result$size

result$centers

result$cluster

par(mfrow=c(2,2), mar=c(5,4,2,2))
plot(iris.new[c(1,2)], col=result$cluster)
plot(iris.new[c(1,2)], col=iris.class)
plot(iris.new[c(3,4)], col=result$cluster)
plot(iris.new[c(3,4)], col=iris.class)

table(result$cluster,iris.class)

print("The accuracy of K-Means algorithm is 88.67% which is a satisfactory performance")

#C50 algorithm

install.packages("C50")
install.packages("dplyr")
library(dplyr)

iris

head(iris,4)
dim(iris)

library(C50)

iris_setosa <- iris[iris$Species == "setosa", ]
iris_versicolor <- iris[iris$Species == "versicolor",]
iris_virginica <- iris[iris$Species == "virginica",]

iris_train <- rbind(iris_setosa[1:35,], iris_versicolor[1:35,], iris_virginica[1:35,])
iris_test <- rbind(iris_setosa[36:50,], iris_versicolor[36:50,], iris_virginica[36:50,])

install.packages("caret")
library(caret)

install.packages("lattice")
install.packages("ggplot2")

library(lattice)
library(ggplot2)

attach(iris)
inTrainingData <- createDataPartition(y= Species, p=0.70, list = FALSE)
trainData <- iris[inTrainingData,]
testData <- iris[-inTrainingData,]

dtModel <- C5.0(trainData[,-5], trainData$Species)
plot(dtModel)

predict(dtModel, testData)

summary(testData)

(testData$Species == predict(dtModel, testData))

mean(testData$Species == predict(dtModel, testData))

install.packages("gmodels")
library(gmodels)
CrossTable(testData$Species,predict(dtModel, testData))

CrossTable(testData$Species == predict(dtModel, testData))


print("The accuracy of C50 algorithm is 88.89% that makes a remarkable outcome for the classification ")



#Accuracy from the models:

SVM accuracy: 97.33%

K Means accuracy: 88.67%

C50 accuracy: 88.89%
