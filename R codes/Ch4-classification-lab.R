
# Lab: Classification Methods


## The Stock Market Data

###
library(ISLR2)
names(Smarket)
dim(Smarket)
summary(Smarket)
pairs(Smarket)
###
#outputs pairwise correlation coefficient
cor(Smarket)
cor(Smarket[, -9])
###
attach(Smarket)
plot(Volume)

## Logistic Regression

###
glm.fits <- glm(
    Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
    data = Smarket, family = binomial
  )
summary(glm.fits)
# For interpretation of glm outputs, see https://www.datascienceblog.net/post/machine-learning/interpreting_generalized_linear_models/
###
coef(glm.fits)
summary(glm.fits)$coef
summary(glm.fits)$coef[, 4]
###
glm.probs <- predict(glm.fits, type = "response")  #output probability of Y=1. For more explanation, run "?predict.glm"
glm.probs[1:10]
contrasts(Direction)
###
glm.pred <- rep("Down", 1250)
glm.pred[glm.probs > .5] = "Up"
###
table(glm.pred, Direction) # creates a contingency table, i.e., a confusion matrix
(507 + 145) / 1250
mean(glm.pred == Direction)
###
train <- (Year < 2005)
# excluding observations before 2005. Note: the observations are up to 2005.

Smarket.2005 <- Smarket[!train, ]
dim(Smarket.2005)
Direction.2005 <- Direction[!train]
###
# fit the logistic regression model using the observations defined by subset.
glm.fits <- glm(
    Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
    data = Smarket, family = binomial, subset = train
  )

#2005 observations are used to test the model fitted with observations before 2005
glm.probs <- predict(glm.fits, Smarket.2005,
    type = "response")
###
glm.pred <- rep("Down", 252)
glm.pred[glm.probs > .5] <- "Up"
table(glm.pred, Direction.2005)
mean(glm.pred == Direction.2005)
mean(glm.pred != Direction.2005)
###

# fit the logistic regression model with less predictors
glm.fits <- glm(Direction ~ Lag1 + Lag2, data = Smarket,
    family = binomial, subset = train)
glm.probs <- predict(glm.fits, Smarket.2005,
    type = "response")
glm.pred <- rep("Down", 252)
glm.pred[glm.probs > .5] <- "Up"
table(glm.pred, Direction.2005)
mean(glm.pred == Direction.2005)
106 / (106 + 76)
###
predict(glm.fits,
    newdata =
      data.frame(Lag1 = c(1.2, 1.5),  Lag2 = c(1.1, -0.8)),
    type = "response"
  )

## Linear Discriminant Analysis

###
library(MASS)
lda.fit <- lda(Direction ~ Lag1 + Lag2, data = Smarket,
    subset = train)
lda.fit
#Coefficients of linear discriminants give the direction of decision boundary hyperplane
plot(lda.fit)  # plots of histograms of linear discriminants (i.e., inner product of predictor vector and coefficient vector)
###
lda.pred <- predict(lda.fit, Smarket.2005)
names(lda.pred)
# predict() returns three elements: class - predicted category of y; 
#                                  posterior - posterior prob of observation belonging to certain category.
#                                  x - linear discriminant

###
lda.class <- lda.pred$class
table(lda.class, Direction.2005)
mean(lda.class == Direction.2005)
###
sum(lda.pred$posterior[, 1] >= .5)
sum(lda.pred$posterior[, 1] < .5)
###
lda.pred$posterior[1:20, 1]
lda.class[1:20]
###
# Use a posterior probability threshold of 0.9 
#  to make predictions of downward market movements
sum(lda.pred$posterior[, 1] > .9)

## Quadratic Discriminant Analysis

###
qda.fit <- qda(Direction ~ Lag1 + Lag2, data = Smarket,
    subset = train)
qda.fit
###
qda.class <- predict(qda.fit, Smarket.2005)$class
table(qda.class, Direction.2005)
mean(qda.class == Direction.2005)

## Naive Bayes

###
library(e1071)
nb.fit <- naiveBayes(Direction ~ Lag1 + Lag2, data = Smarket,
    subset = train)
# by default, each quantitative feature is modeled using a Gaussian distribution
nb.fit
# The conditional probabilities contain the estimated 
#  mean([,1]) and standard deviation ([,2]) of each variable in each class
###
# Note below the use of two logical indices on a vector
mean(Lag1[train][Direction[train] == "Down"])
sd(Lag1[train][Direction[train] == "Down"])
###
nb.class <- predict(nb.fit, Smarket.2005)
table(nb.class, Direction.2005)
mean(nb.class == Direction.2005)
###
# Below the predict function generates estimates of the probability
#   that each observation belongs to a particular class.
nb.preds <- predict(nb.fit, Smarket.2005, type = "raw")
nb.preds[1:5, ]


## K-Nearest Neighbors
###
library(class)
#knn() is part of the "class" package. It requires four inputs.
train.X <- cbind(Lag1, Lag2)[train, ]
test.X <- cbind(Lag1, Lag2)[!train, ]
train.Direction <- Direction[train]
###
set.seed(1)
knn.pred <- knn(train.X, test.X, train.Direction, k = 1)
table(knn.pred, Direction.2005)
(83 + 43) / 252
###
knn.pred <- knn(train.X, test.X, train.Direction, k = 3)
table(knn.pred, Direction.2005)
mean(knn.pred == Direction.2005)



###
# Application of knn on the Caravan data set, which is included in ISLR2.
# use "?Caravan" to find details of Caravan data.
dim(Caravan)
attach(Caravan)
summary(Purchase)
348 / 5822
###

# The scale() function standardizes the data so that each variable 
# has a zero mean and a standard deviation of 1.
standardized.X <- scale(Caravan[, -86])
var(Caravan[, 1])
var(Caravan[, 2])
var(standardized.X[, 1])
var(standardized.X[, 2])
###
test <- 1:1000
train.X <- standardized.X[-test, ]
test.X <- standardized.X[test, ]
train.Y <- Purchase[-test]
test.Y <- Purchase[test]
set.seed(1)
knn.pred <- knn(train.X, test.X, train.Y, k = 1)
mean(test.Y != knn.pred)
mean(test.Y != "No")
###
table(knn.pred, test.Y)
9 / (68 + 9)
###
knn.pred <- knn(train.X, test.X, train.Y, k = 3)
table(knn.pred, test.Y)
5 / 26
knn.pred <- knn(train.X, test.X, train.Y, k = 5)
table(knn.pred, test.Y)
4 / 15
###
# Below a logistic regression model is fit on the Caravan data.
glm.fits <- glm(Purchase ~ ., data = Caravan,
    family = binomial, subset = -test)
glm.probs <- predict(glm.fits, Caravan[test, ],
    type = "response")
glm.pred <- rep("No", 1000)
glm.pred[glm.probs > .5] <- "Yes"
table(glm.pred, test.Y)

glm.pred <- rep("No", 1000)
glm.pred[glm.probs > .25] <- "Yes"
table(glm.pred, test.Y)
11 / (22 + 11)

## Poisson Regression

###
# The Bikeshare data is included in ISLR2.
attach(Bikeshare)
dim(Bikeshare)
names(Bikeshare)
###
# The response variable is the number of bike rentals (bikers)
#  per hour in Washington, DC.
mod.lm <- lm(
    bikers ~ mnth + hr + workingday + temp + weathersit,
    data = Bikeshare
  )
summary(mod.lm)
# Note that in the output, the first level of hr (0) 
#      and mnth (Jan) are treated as the baseline values.
###
# Below a different coding method is used for hr and mnth, in such a way
# that the sum of coefficient estimates of hr or mnth is 0. Therefore, the 
# coefficient can be interpreted as the difference from the mean level.

contrasts(Bikeshare$hr) = contr.sum(24)
contrasts(Bikeshare$mnth) = contr.sum(12)
# Note: a contrast of a factor is a linear combination of the indicator functions
#       of different levels of a factor. 
mod.lm2 <- lm(
    bikers ~ mnth + hr + workingday + temp + weathersit,
    data = Bikeshare
  )
summary(mod.lm2)
###
sum((predict(mod.lm) - predict(mod.lm2))^2)
###
all.equal(predict(mod.lm), predict(mod.lm2))
###
coef.months <- c(coef(mod.lm2)[2:12],
    -sum(coef(mod.lm2)[2:12]))
###
#  xaxt="n" and yaxt="n" suppress the x and y axis respectively
plot(coef.months, xlab = "Month", ylab = "Coefficient",
    xaxt = "n", col = "blue", pch = 19, type = "o")
axis(side = 1, at = 1:12, labels = c("J", "F", "M", "A",
    "M", "J", "J", "A", "S", "O", "N", "D"))
###
coef.hours <- c(coef(mod.lm2)[13:35],
    -sum(coef(mod.lm2)[13:35]))
plot(coef.hours, xlab = "Hour", ylab = "Coefficient",
    col = "blue", pch = 19, type = "o")
###
mod.pois <- glm(
    bikers ~ mnth + hr + workingday + temp + weathersit,
    data = Bikeshare, family = poisson
  )
summary(mod.pois)
###
coef.mnth <- c(coef(mod.pois)[2:12],
    -sum(coef(mod.pois)[2:12]))
plot(coef.mnth, xlab = "Month", ylab = "Coefficient",
     xaxt = "n", col = "blue", pch = 19, type = "o")
axis(side = 1, at = 1:12, labels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D"))
coef.hours <- c(coef(mod.pois)[13:35],
     -sum(coef(mod.pois)[13:35]))
plot(coef.hours, xlab = "Hour", ylab = "Coefficient",
    col = "blue", pch = 19, type = "o")
###
plot(predict(mod.lm2), predict(mod.pois, type = "response"))
# The argument type = "response" specifies that the output is 
#  exp(beta*x) instead of beta*x.
abline(0, 1, col = 2, lwd = 3)
###
