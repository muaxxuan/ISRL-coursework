##### ISLR chapter 5 R-lab #####
### The Validation Set Approach ###
library(ISLR2)
set.seed(1) # Set initial value to 1
train <- sample(392,196) # Using sample() function to split the set of obs into
# 2 halves by selecting 196 obs (Refer as the Training set) out of the original 
# 392 obs. # horsepower is predictor (x)
lm.fit <- lm(mpg~horsepower,data=Auto,subset=train) # mpg is reponse (y)
# Using subset option in lm() to fit a linear regression using only training set
attach(Auto)
mean((mpg-predict(lm.fit,Auto))[-train]^2)
# Use mean() to calculate the MSE of 196 obs in the validation set
# Note -train index selects only obs that are not in the training set
# The estimated test MSE for the linear regression is 23.26601
lm.fit2 <- lm(mpg~poly(horsepower,2),data=Auto,subset=train)
mean((mpg-predict(lm.fit2,Auto))[-train]^2)
# For degree 3
lm.fit3 <- lm(mpg~poly(horsepower,3),data=Auto,subset=train)
mean((mpg-predict(lm.fit3,Auto))[-train]^2)
# We notice the quradatic has a better fit since its mse is lowest of all fits
set.seed(2)
train <- sample(392,196)
lm.fit <- lm(mpg~horsepower,data=Auto,subset=train)
mean((mpg-predict(lm.fit,Auto))[-train]^2)

lm.fit2 <- lm(mpg~poly(horsepower,2),data=Auto,subset=train)
mean((mpg-predict(lm.fit2,Auto))[-train]^2)

lm.fit3 <- lm(mpg~poly(horsepower,3),data=Auto,subet=train)
mean((mpg-predict(lm.fit3,Auto))[-train]^2)

###### LOOCV ######
glm.fit <- glm(mpg~horsepower,data=Auto)
coef(glm.fit)
lm.fit <- lm(mpg~horsepower,data=Auto)
coef(lm.fit)
# Notice if we use glm() without passing in the family argument then its 
# performs like the lm()
library(boot)
glm.fit <- glm(mpg ~ horsepower, data = Auto)
cv.err <- cv.glm(Auto, glm.fit)
cv.err$delta

# Using for loop to iterate 10 times the degree of polynomials
cv.error <- rep(0, 10)
for(i in 1:10)
{
  glm.fit <- glm(mpg ~ poly(horsepower, i), data = Auto)
  cv.error[i] <- cv.glm(Auto, glm.fit)$delta[1]
  print(cv.error[i])
}

# We see a shrap drop in the estimated test MSE between the linear and quadrafit
# fits, but no clear improvement for the rest... Why NA?

### K-fold Cross-Validation ###

set.seed(17)
cv.error.10 <- rep(0, 10)
 
for(i in 1:10)
{
  glm.fit <- glm(mpg ~ poly(horsepower, i), data = Auto)
  cv.error.10[i] <- cv.glm(Auto, glm.fit, K = 10)$delta[1]
  print(cv.error.10[i])
}

### The Bootstrap ###

## Estimating the accuracy of a Statistics of interest ##

# This formula minimizes the risk of say a portfolio in finance
# Estimate the value of alpha from the Portfolio dataset
alpha.fn <- function(data, index)
{
  X <- data$X[index]
  Y <- data$Y[index]
  (var(Y) - cov(X, Y)) / (var(X) + var(Y) - 2*cov(X, Y))
}
alpha.fn(Portfolio, 1:100)

set.seed(7)
alpha.fn(Portfolio, sample(100, 100, replace = T))
# sample() randomly selects 100 obs from the range 1-100 with replacement

# the boot() automates the above approach
# Let us reproduce for R = 1000 boostrap estimates for alpha
boot(Portfolio, alpha.fn, R = 1000)
# The final output shows that using the original data alpha_hat = .5758
# and the bootstrap estimate for SE(alpha_hat) = .08969

## Estimating the accuracy of a linear reg. model  ##

boot.fn <- function(data, index)
{
  coef(lm(mpg ~ horsepower, data = data, subset = index))
}
boot.fn(Auto, 1:392)
# boot.fn() can also be used in order to create bootstrap estimate for the inter
# cept and the slope by randomly sampling among the obs with replacement.
set.seed(1)
boot.fn(Auto, sample(392, 392, replace = T))
boot.fn(data=Auto,sample(392,392,replace=T))
# Use boot() to compute the SE pf 1000 bootstraps estimates for the intercept &
# the slope
boot(Auto, boot.fn, R = 1000)
# summary() below does the same as boot() just above
summary(lm(mpg ~ horsepower, data = Auto))$coef

boot.fn <- function(data, index)
{
  coef(
        lm(mpg ~ horsepower + I(horsepower^2),
        data = data, subset = index)
      )
}
set.seed(1)
boot(Auto, boot.fn, 1000)

summary(
  lm(mpg ~ horsepower + I(horsepower^2), data = Auto)
)$coef










