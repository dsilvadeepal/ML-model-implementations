#Import libraries
library(caret)
library(randomForest)

#Input the data
voice <- read.csv("voice.csv")

#View & summarize data
str(voice)
summary(voice)

#We see that label(gender) is our output variable(the one we want to determine)
#Split the data into training and testing sets using label as the split.
#We use createDataPartition to create balanced splits of the data.
set.seed(100)
s <-createDataPartition(voice$label, p=0.8, list=F)
train<-voice[s,]
test<-voice[-s,]


#Constructing a random forest model
forest<-randomForest(label~., data=train, ntree=25, do.trace=T, importance=T)

#Predicting the outcome
pred<-predict(forest, test, type="class")
confusionMatrix(pred, test$label)



