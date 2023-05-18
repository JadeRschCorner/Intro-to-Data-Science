
# Lab: Cross-Validation and the Bootstrap


## The Validation Set Approach
###
library(ISLR2)
set.seed(1)
train <- sample(392, 196)
# the sample() function takes 196 random samples from 1:392. 
# Note 392 is the number of rows of the data set "Auto"
###
lm.fit <- lm(mpg ~ horsepower, data = Auto, subset = train)
###
attach(Auto)
mean((mpg - predict(lm.fit, Auto))[-train]^2)
# test MSE of lm.fit
###
lm.fit2 <- lm(mpg ~ poly(horsepower, 2), data = Auto, 
    subset = train)
mean((mpg - predict(lm.fit2, Auto))[-train]^2)
lm.fit3 <- lm(mpg ~ poly(horsepower, 3), data = Auto, 
    subset = train)
mean((mpg - predict(lm.fit3, Auto))[-train]^2)
###
set.seed(2)
# Set a different seed value for random numbers so that 
# a different set of train data may be selected.
train <- sample(392, 196)
lm.fit <- lm(mpg ~ horsepower, subset = train)
mean((mpg - predict(lm.fit, Auto))[-train]^2)
lm.fit2 <- lm(mpg ~ poly(horsepower, 2), data = Auto, 
    subset = train)
mean((mpg - predict(lm.fit2, Auto))[-train]^2)
lm.fit3 <- lm(mpg ~ poly(horsepower, 3), data = Auto, 
    subset = train)
mean((mpg - predict(lm.fit3, Auto))[-train]^2)





## Leave-One-Out Cross-Validation

###
glm.fit <- glm(mpg ~ horsepower, data = Auto)
# without 'family="binomial"' argument, glm() performs linear regression.
coef(glm.fit)
###
lm.fit <- lm(mpg ~ horsepower, data = Auto)
coef(lm.fit)
###
library(boot)
# the "cv.glm()" function is part of the "boot" package
glm.fit <- glm(mpg ~ horsepower, data = Auto)
cv.err <- cv.glm(Auto, glm.fit)
# cv.glm() calculates the estimated K-fold cross-validation prediction 
# error for generalized linear models
# cv.glm(data, glmfit, cost, K). by default, K equals the number of 
# observations in data
cv.err$delta
# delta contains raw and adjusted (bias-corrected) cross-validation estimates 
# of prediction error.
###
cv.error <- rep(0, 10)
for (i in 1:10) {
  glm.fit <- glm(mpg ~ poly(horsepower, i), data = Auto)
  cv.error[i] <- cv.glm(Auto, glm.fit)$delta[1]
}
cv.error





## $k$-Fold Cross-Validation
###
set.seed(17)
cv.error.10 <- rep(0, 10)
for (i in 1:10) {
  glm.fit <- glm(mpg ~ poly(horsepower, i), data = Auto)
  cv.error.10[i] <- cv.glm(Auto, glm.fit, K = 10)$delta[1]
}
cv.error.10






## The Bootstrap

### Estimating the Accuracy of a Statistic of Interest

###
alpha.fn <- function(data, index) {
  X <- data$X[index]
  Y <- data$Y[index]
  (var(Y) - cov(X, Y)) / (var(X) + var(Y) - 2 * cov(X, Y))
}
###
alpha.fn(Portfolio, 1:100)
# The "Portfolio" data set is from the ISLR2 package
###
set.seed(7)
alpha.fn(Portfolio, sample(100, 100, replace = T))
# sample(100,100,replace=T) randomly select 100 numbers from 1:100 with replacement.
###
boot(Portfolio, alpha.fn, R = 1000)
# The boot() function is part of the "boot" library. It performs the bootstrap.





### Estimating the Accuracy of a Linear Regression Model

###
boot.fn <- function(data, index)
  coef(lm(mpg ~ horsepower, data = data, subset = index))
boot.fn(Auto, 1:392)
###
set.seed(1)
boot.fn(Auto, sample(392, 392, replace = T))
boot.fn(Auto, sample(392, 392, replace = T))
###
boot(Auto, boot.fn, 1000)
###
summary(lm(mpg ~ horsepower, data = Auto))$coef
###
boot.fn <- function(data, index)
  coef(
      lm(mpg ~ horsepower + I(horsepower^2), 
        data = data, subset = index)
    )
set.seed(1)
boot(Auto, boot.fn, 1000)
summary(
    lm(mpg ~ horsepower + I(horsepower^2), data = Auto)
  )$coef
###
