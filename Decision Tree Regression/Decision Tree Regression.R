
library(rpart)

cars <- read.csv("cars.csv")

#Create training and test data
train <- cars[1:950, ] # training data
test <- cars[951:963, ] # test data


#Generating the decision tree model
regtree <- rpart(CarSales ~ ., method = "anova", data = train)

summary(regtree)

#Plotting the regression tree
plot(regtree, uniform=TRUE, main="Regression Tree for Sales")
text(regtree, use.n=TRUE, all=TRUE, cex=.8)

#View the regression tree parameters
printcp(regtree)

#View the cross-validation error for each split
par(mfrow=c(1,2)) 
rsq.rpart(regtree) 
#Left chart chows R2 improving as splits increase & right chart shows xerror(cross validation error)
#decreases with each split

#Predicting the outcome
predict(regtree, test, method="anova")