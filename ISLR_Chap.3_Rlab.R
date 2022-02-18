### ISLR chapter 3 R-lab ###
library(MASS)
library(ISLR2)
data(Boston)
nrow(Boston)
ncol(Boston)
dim(Boston)
str(Boston)
lm.fit<-lm(medv~lstat,data=Boston) # linear model medv is response(y) and lstat
# is predictor(x)
summary(lm.fit)
names(lm.fit) # Extract information
coef(lm.fit) # access the coefficient
confint(lm.fit) # Obtain confidence interval
predict(lm.fit , data.frame(lstat=c(5,10,15)) , interval="confidence")
predict(lm.fit , data.frame(lstat=c(5,10,15)) , interval="prediction")
plot(lstat,medv,col=6)
abline(lm.fit,col=1)
abline(lm.fit,lw=4,col="red")
plot(lstat,medv,col="blue")
plot(lstat,medv,pch=20) # pch creates different plotting symbol
plot(1:20,1:20,pch=1:20) # Showcase of different symbol 
par(mfrow=c(2,2))
plot(lm.fit)
plot(predict(lm.fit),residuals(lm.fit))
plot(predict(lm.fit),rstudent(lm.fit))
plot(hatvalues(lm.fit)) # Leverage statistics can be computed for any number of
# predictors using the hatvalues() function
which.max(hatvalues(lm.fit)) # Identifies the index of the largest element of 
# vector. In this case, it tells us which obs has the largest levrage statistic.
### Multiple linear regression lm(y~x1+x2+x3,data)
lm.fit<-lm(medv~lstat+age,data=Boston)
summary(lm.fit)
# ALL predictors
lm.fit<-lm(medv~.,data=Boston)
summary(lm.fit)
summary(lm.fit)$r.sq # Gives us R^2
summary(lm.fit)$sigma # Give us RSE

install.packages("VIF")
library(car)
vif(lm.fit) # Variance inflation function for each predictors

lm.fit1<-lm(medv~.-age,Boston) # Take out age
summary(lm.fit1)

lm.fit1<-update(lm.fit,~.-age) # Alternative method
summary(lm.fit1)

# Interaction terms
summary(lm(medv~lstat*age,Boston))

# Non-linear transformations of the predictors
?I() # Change the class of an object
lm.fit2<-lm(medv~lstat+I(lstat^2))
summary(lm.fit2)        

# Use anova() to further quantify the extent to which the quadractic fit is
# superior to the linear fit
lm.fit<-lm(medv~lstat,data=Boston)
anova(lm.fit,lm.fit2) 
?anova()
# By the F-stat of 135 and the associated p-value virtually at 0
# We see strong evidence the quadratic fit is far superior
par(mfrow=c(2,2))
plot(lm.fit2)

lm.fit5<-lm(medv~poly(lstat,5)) # poly() function create a polynomial of degree
# 5 in this scenario
summary(lm.fit5)
summary(lm(medv~log(rm),data=Boston))

# Qualitative predictors
head(Carseats)
str(Carseats)
lm.fit<-lm(Sales~.+Income:Advertising+Price:Age,data=Carseats)
summary(lm.fit)

# contrast() function returns the coding that r uses for dummy variables
attach(Carseats)
contrasts(ShelveLoc)

# Writing functions
LoadLibraries<-function()
{
  library(ISLR2)
  library(MASS)
  print("The libraries ISLR2 and MASS have been loaded")
}
# Call the function
LoadLibraries()









