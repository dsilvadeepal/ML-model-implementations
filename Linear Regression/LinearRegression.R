setwd("C:/Users/dsilv/Desktop/Learning/Data Science/ML-model-implementations/Linear Regression")

#Load libraries
library(tidyverse)
library(ggplot2)
library(gridExtra)

#Load data
insurance <- read.csv("insurance.csv")

# Preview & summarize the data
head(insurance)
str(insurance)
summary(insurance)

#Plotting some of the variables 
#Scatter plots for numeric variables
plot.age <- ggplot(insurance, aes(x = age, y = charges)) +
  geom_point()

plot.bmi <- ggplot(insurance, aes(x = bmi, y = charges)) +
  geom_point()

grid.arrange(plot.age, plot.bmi, ncol=2)

#Box plots for categorical variables
plot.sex <- ggplot(insurance, aes(x = sex, y = charges)) +
  geom_boxplot()

plot.region <- ggplot(insurance, aes(x = region, y = charges)) +
  geom_boxplot()

plot.child <- ggplot(insurance, aes(x = as.factor(children), y = charges)) +
  geom_boxplot()

plot.smoker <- ggplot(insurance, aes(x = smoker, y = charges)) +
  geom_boxplot()

grid.arrange(plot.sex, plot.region, plot.child, plot.smoker, ncol=2, nrow=2)

# We see that means of sex and region are pretty similar so we'll ignore it from our model.
# And we see that age and charges are not linearly dependent.
# We also see that charges are highly dependent on smoking and BMI variables.
# How about if we have a smoker who is highly obese. We see there could be an interaction between these 2 variables
# so lets handle that further.

# Creating our model

#Simple linear regression (with smoking as an input)
mod1 = lm(charges ~ smoker, data = insurance)
summary(mod1)

#Looking at R^2 we see that 61.9% os the total variation in the charges is explained by the predictor smoker.

#Multiple linear regression using all variables
mod2 = lm(charges ~ ., data = insurance)
summary(mod2)

# We can corroborate with our analysis before that sex and region do not contribute to our model. We can hence remove it.

#Let's transform the BMI variable into obese or not.
insurance$obese <- as.factor(ifelse(insurance$bmi >=30, "yes", "no"))


# Multiple linear regression with interaction parameters & quadratic function for age
mod3 <- lm(charges ~ obese * smoker + age + age^2 + children, data=insurance)
summary(mod3)



