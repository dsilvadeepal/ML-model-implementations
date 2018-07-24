#Import libraries
library(caret) #short for _C_lassification _A_nd _RE_gression _T_raining
library(rpart)
library(rpart.plot)


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

#Modeling with Decision Trees and Plotting
tree<-rpart(label~., data=train)
rpart.plot(tree, type=1, space=3, extra = 1, tweak = 0.7, main="Number of cases per class")

#Predicting the outcome
pred<-predict(tree, test, type="class")
confusionMatrix(pred, test$label)
