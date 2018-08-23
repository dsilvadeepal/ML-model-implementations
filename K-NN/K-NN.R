setwd("C:/Users/dsilv/Desktop/Learning/Data Science/ML-model-implementations/K-NN")

library(class)
library(ggplot2)

iris <- read.csv("Iris.csv", -1)

iris <- iris[, -1]

iris %>% ggplot(aes(PetalLengthCm, PetalWidthCm, color = Species)) + geom_jitter() + 
  scale_color_manual(values=c("mediumpurple3", "violetred3", "darkturquoise"))


table(iris$Species)

prop.table(table(iris$Species)) * 100


#Normalize iris data
irisNorm <- iris
irisNorm[,-5] <- scale(iris[,-5])

summary(irisNorm)

#Create training and test data
set.seed(1234)
ind <- sample(2, nrow(irisNorm), replace=TRUE, prob=c(0.7,0.3))
trainData <- irisNorm[ind==1,]
testData <- irisNorm[ind==2,]

#Create the K-NN model

KnnPred_7 <- knn(trainData[,-5], testData[,-5],
                 trainData$Species, k=7, prob=TRUE)


#Confusion matrix
table(testData$Species, KnnPred_7)
sum(KnnPred_7==testData$Species)/length(testData$Species)*100

#OR

accuracy<-mean(testData$Species==KnnPred_7)
accuracy




