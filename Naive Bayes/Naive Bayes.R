library(caTools)  #split the data set into training and test data
library(e1071)  #to train a Naive Bayes classifier
library(caret)  #for the confusion matrix


iris <- read.csv("Iris.csv")

str(iris)

#Remove Id Column
iris <- iris[, -1]

#Split the data into training and test data on the Species feature
set.seed(100)
split <- sample.split(iris$Species, SplitRatio = 0.7)
train = subset(iris, split == TRUE)
test = subset(iris, split == FALSE)


model <- naiveBayes(Species ~ ., data = train)

table(predict(model, test[,1:4]), test$Species)

pred <- predict(model, test[ , 1:4])

confusionMatrix(pred, test$Species)

