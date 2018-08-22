#Load Libraries
library(ggplot2)
library(caTools)
library(e1071)  # for svm

#Import Data
iris<-read.csv("Iris.csv")[,-1]   #drop the column with ids

#Review the data
str(iris)
summary(iris)

#Standardize column names
names(iris)<-tolower(names(iris))


#Split the dataset into Training & Test data
set.seed(123)
n <- nrow(iris)
shuffled <- iris[sample(n),]
dim(shuffled)
train <- shuffled[1:round(0.7 * n),]
test <- shuffled[(round(0.7 * n) + 1):n,]


#Tune the model
a<-tune("svm",species~.,data=iris,
        kernel="radial",                           #set the kernel as radial
        tunecontrol = tune.control(cross = 10),    #10-fold cross-validation
        ranges=list(cost=c(2^c(-5:5)),             #set cost and gamma
                    gamma=c(10^c(-2:2))))
summary(a)


#SVM Model
model <- svm(species ~ ., 
             train, kernel = "radial", cost = 2, gamma = .1)

predictions<-table(predict(model,test),test$species)
sum(diag(predictions))/sum(predictions)


plot(model, iris, petalwidthcm  ~ petallengthcm,
     slice = list(sepalwidthcm = 3, sepallengthcm = 4))