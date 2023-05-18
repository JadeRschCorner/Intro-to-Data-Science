
# Lab: Linear Models and Regularization Methods

## Subset Selection Methods

### Best Subset Selection

###
library(ISLR2)
View(Hitters)
names(Hitters)
dim(Hitters)
sum(is.na(Hitters$Salary))
###
Hitters <- na.omit(Hitters)
dim(Hitters)
sum(is.na(Hitters))
###
# install.packages("leaps")  # leaps: Regression Subset Selection package
library(leaps)
regfit.full <- regsubsets(Salary ~ ., Hitters)
# regsubsets() function is part of the "leaps" package. It performs best subset selection
# by exhaustive search, forward or backward stepwise, or sequential replacement

summary(regfit.full)
###
regfit.full <- regsubsets(Salary ~ ., data = Hitters,
    nvmax = 19) # The default value of nvmax is 8.
reg.summary <- summary(regfit.full)
###
names(reg.summary)
###
reg.summary$rsq
###
par(mfrow = c(2, 2))
plot(reg.summary$rss, xlab = "Number of Variables",
    ylab = "RSS", type = "l")
plot(reg.summary$adjr2, xlab = "Number of Variables",
    ylab = "Adjusted RSq", type = "l")
###
which.max(reg.summary$adjr2)
points(11, reg.summary$adjr2[11], col = "red", cex = 2, 
    pch = 20)
###
plot(reg.summary$cp, xlab = "Number of Variables",
    ylab = "Cp", type = "l")
which.min(reg.summary$cp)
points(10, reg.summary$cp[10], col = "red", cex = 2,
    pch = 20)
which.min(reg.summary$bic)
plot(reg.summary$bic, xlab = "Number of Variables",
    ylab = "BIC", type = "l")
points(6, reg.summary$bic[6], col = "red", cex = 2,
    pch = 20)
###
plot(regfit.full, scale = "r2")
# In the plot, the black square in the top row indicates the variable 
#  is selected in the optimal model.

plot(regfit.full, scale = "adjr2")
plot(regfit.full, scale = "Cp")
plot(regfit.full, scale = "bic")
###
coef(regfit.full, 6)

### Forward and Backward Stepwise Selection

###
regfit.fwd <- regsubsets(Salary ~ ., data = Hitters,
    nvmax = 19, method = "forward")
summary(regfit.fwd)
regfit.bwd <- regsubsets(Salary ~ ., data = Hitters,
    nvmax = 19, method = "backward")
summary(regfit.bwd)
###
coef(regfit.full, 7)
coef(regfit.fwd, 7)
coef(regfit.bwd, 7)




### Choosing Among Models Using the Validation-Set Approach and Cross-Validation

###
set.seed(1)
train <- sample(c(TRUE, FALSE), nrow(Hitters), replace = TRUE)
# The sample() function selects a set of data for training purpose
test <- (!train)
###
regfit.best <- regsubsets(Salary ~ .,
    data = Hitters[train, ], nvmax = 19)
# the regsubsets performs best subset selection using training set.
###

test.mat <- model.matrix(Salary ~ ., data = Hitters[test, ])
# The model.matrix() function builds an "X" matrix from test data (i.e., test data matrix)
###

val.errors <- rep(NA, 19)
for (i in 1:19) {
 coefi <- coef(regfit.best, id = i) # estimated coefficients for model with i predictors
 pred <- test.mat[, names(coefi)] %*% coefi #test data matrix times estimated coefficient vector gives a vector of predictions
 val.errors[i] <- mean((Hitters$Salary[test] - pred)^2) # test MSE
}
###
val.errors
which.min(val.errors)
coef(regfit.best, 7)
###



# the following block defines a function to make predictions
 predict.regsubsets <- function(object, newdata, id, ...) {  #The .operator (ellipsis) allows a function to take arguments that are not predefined in its definition. 
  form <- as.formula(object$call[[2]]) # extract formula from object
  mat <- model.matrix(form, newdata)  # create model matrix from formula and newdata
  coefi <- coef(object, id = id)       # extract estimated coefficient vector from object
  xvars <- names(coefi)               # extract names of predictors corresponding to the coefficient vector
  mat[, xvars] %*% coefi              # model matrix multiplied by coefficient vector gives predicted response values
 }
###
regfit.best <- regsubsets(Salary ~ ., data = Hitters,
    nvmax = 19)           # there are 19 predictors in Hitters data set.
coef(regfit.best, 7)
###
k <- 10
n <- nrow(Hitters)
set.seed(1)
folds <- sample(rep(1:k, length = n)) # allocate each observation to one of k folds; Here the sample function performs a random permutation.
cv.errors <- matrix(NA, k, 19,
    dimnames = list(NULL, paste(1:19)))   # paste() converts a vector of numbers to a vector of characters; row names are NULL.
###
for (j in 1:k) {
  best.fit <- regsubsets(Salary ~ .,
       data = Hitters[folds != j, ],
       nvmax = 19)
  for (i in 1:19) {
    pred <- predict(best.fit, Hitters[folds == j, ], id = i)
    cv.errors[j, i] <-
         mean((Hitters$Salary[folds == j] - pred)^2)
   }
 }
###
mean.cv.errors <- apply(cv.errors, 2, mean)  # calculate column means
mean.cv.errors
par(mfrow = c(1, 1))
plot(mean.cv.errors, type = "b")
###
reg.best <- regsubsets(Salary ~ ., data = Hitters,
    nvmax = 19)
coef(reg.best, 10)





## Ridge Regression and the Lasso

###
x <- model.matrix(Salary ~ ., Hitters)[, -1]
# matrix x contains 19 predictors from the Hitters data set
# The model.matrix() function is used to create the X matrix. It also converts 
# qualitative variables into dummy variables.
# The first column of the model.matrix output is an Intercept term (a vector of
# 1), which is excluded from the X matrix.
y <- Hitters$Salary



### Ridge Regression

###
library(glmnet)
# The glmnet() function in the "glmnet" package can fit ridge, lasso regression 
# models, and more.
# This glmnet() function takes X matrix and y vector as inputs, so we must 
# create these first.
# by default, glmnet() standardizes the variables to be on the same scale.

grid <- 10^seq(10, -2, length = 100)
ridge.mod <- glmnet(x, y, alpha = 0, lambda = grid)
# alpha = 0 -> ridge; alpha =1 -> lasso; alpha between 0 and 1 -> elastic net.
# lambda is a tuning parameter for ridge regression
###
dim(coef(ridge.mod))
# each column of the coef(ridge.mod) matrix is a vector of estimated coefficients for one lambda value
###
ridge.mod$lambda[50]
coef(ridge.mod)[, 50]
sqrt(sum(coef(ridge.mod)[-1, 50]^2))  # "-1" excludes the intercept
# length (l_2 norm) of the estimated coefficient vector 

ridge.mod$lambda[60]
coef(ridge.mod)[, 60]
sqrt(sum(coef(ridge.mod)[-1, 60]^2))
# a smaller lambda values leads to a higher l_2 norm

###
predict(ridge.mod, s = 50, type = "coefficients")[1:20, ]
# This gives the ridge regression coefficients for a new value (50) of lambda.
# see https://www.rdocumentation.org/packages/glmnet/versions/1.1-1/topics/predict.glmnet 
# for details of the predict() function for glmnet class
## Usage for class 'glmnet':
# predict(object, newx, s = object$lambda,
#        type=c("link","response","coefficients","class","nonzero"), exact =
#          FALSE, ...)
## S3 method for class 'glmnet':
# coef(object,s=object$lambda, exact=FALSE, ...)

###
# The code below randomly divides the observations into two parts, 
# one for training, the other for testing.
set.seed(1)
train <- sample(1:nrow(x), nrow(x) / 2)
test <- (-train)
y.test <- y[test]
###
ridge.mod <- glmnet(x[train, ], y[train], alpha = 0,
    lambda = grid, thresh = 1e-12)
# thresh is convergence threshold for coordinate descent.Default value is 1e-7.
ridge.pred <- predict(ridge.mod, s = 4, newx = x[test, ])
mean((ridge.pred - y.test)^2) # test MSE
###
mean((mean(y[train]) - y.test)^2) # test MSE for a model with only an intercept.
###
ridge.pred <- predict(ridge.mod, s = 1e10, newx = x[test, ])
mean((ridge.pred - y.test)^2)
###
ridge.pred <- predict(ridge.mod, s = 0, newx = x[test, ],
    exact = T, x = x[train, ], y = y[train])
mean((ridge.pred - y.test)^2)
lm(y ~ x, subset = train)
predict(ridge.mod, s = 0, exact = T, type = "coefficients",
    x = x[train, ], y = y[train])[1:20, ]
# when lambda=0, ridge regression become regular OLS linear regression

###
set.seed(1)
cv.out <- cv.glmnet(x[train, ], y[train], alpha = 0)
# cv.glmnet() performs cross validation for different lambda values.

plot(cv.out)
bestlam <- cv.out$lambda.min
# lambda value that results in the smallest cross validation error
bestlam

###
ridge.pred <- predict(ridge.mod, s = bestlam,
    newx = x[test, ])
mean((ridge.pred - y.test)^2)
# test MSE associated with lambda.min

# fit the ridge regression on the full data set. Lambda sequence is computed automatically.
###
out <- glmnet(x, y, alpha = 0)
predict(out, type = "coefficients", s = bestlam)[1:20, ]



### The Lasso

###
lasso.mod <- glmnet(x[train, ], y[train], alpha = 1,
    lambda = grid)
plot(lasso.mod)
###
set.seed(1)
cv.out <- cv.glmnet(x[train, ], y[train], alpha = 1)
plot(cv.out)
bestlam <- cv.out$lambda.min
lasso.pred <- predict(lasso.mod, s = bestlam,
    newx = x[test, ])
mean((lasso.pred - y.test)^2)
###
out <- glmnet(x, y, alpha = 1, lambda = grid)
lasso.coef <- predict(out, type = "coefficients",
    s = bestlam)[1:20, ]
lasso.coef # note that some coefficients are zero.
lasso.coef[lasso.coef != 0]





# Chapter 12 Lab 1: Principal Components Analysis

View(USArrests)
dim(USArrests)
names(USArrests)
# the data set "USArrests" is part of R base package.

states=row.names(USArrests)
states

apply(USArrests, 2, mean)  # calculate the mean of each column of USArrests
apply(USArrests, 2, var)   # calculate the variance of each column
pr.out=prcomp(USArrests, scale=TRUE)
# prcomp() performs PCA. By default, it centers the variables to have mean zero.
# by using the argument "scale=TRUE", the variances of variables are all scaled to 1.
#Note: It is important to scale the variables to 0 mean and 1 variance before PCA.

names(pr.out)
pr.out$center  # the means of variables that were used for scaling before PCA
pr.out$scale   # the standard deviation of variables that were used for scaling before PCA 
pr.out$rotation # each column is a principal component loading vector, or the right singular vector v_i in lecture notes.
dim(pr.out$x)
par(mfrow=c(1,1))
biplot(pr.out, scale=0)
# "scale=0" ensures that the arrows are scaled to represent the loadings.
pr.out$rotation=-pr.out$rotation
pr.out$x=-pr.out$x
biplot(pr.out, scale=0)
pr.out$sdev # standard deviation of each principal component

pr.var=pr.out$sdev^2
pr.var  # variance explained by each principal component

pve=pr.var/sum(pr.var)
pve # proportion of variance explained by each principal component

plot(pve, xlab="Principal Component", ylab="Proportion of Variance Explained", ylim=c(0,1),type='b')
plot(cumsum(pve), xlab="Principal Component", ylab="Cumulative Proportion of Variance Explained", ylim=c(0,1),type='b')




## PCR and PLS Regression


### Principal Components Regression

###
# install.packages("pls")
library(pls)
# the "pls" package contains the pcr() function, which performs principal component regression
# and the plsr() function, which performs the partial least squares analysis.

set.seed(2)
pcr.fit <- pcr(Salary ~ ., data = Hitters, scale = TRUE,
    validation = "CV")
# scale=TRUE standardizes each predictor so that their sample variances are the same.
# validation="CV" causes pcr to compute 10-fold cv error for each possible number of principal components.

###
summary(pcr.fit)  #"Fit method: svdpc" means fitting a PCR model using the singular value decomposition.
# "adjCV" (for RMSEP and MSEP) is the bias-corrected cross-validation estimate. 
# bias correction refers to the fact that, when we do perform resampling (bootstrap or cross-validation)
# we almost certainly do not use our whole sample of size N; this potential leads to the biased estimates 
# of the MSEP (Mean Squared Error of Prediction)
# MSEP:  Mean Squared Error of Prediction
# RMSEP: Root Mean Squared Error of Prediction
###
validationplot(pcr.fit, val.type = "MSEP")
###
set.seed(1)
pcr.fit <- pcr(Salary ~ ., data = Hitters, subset = train,
    scale = TRUE, validation = "CV")
validationplot(pcr.fit, val.type = "MSEP")
###
pcr.pred <- predict(pcr.fit, x[test, ], ncomp = 5)
mean((pcr.pred - y.test)^2)
###
pcr.fit <- pcr(y ~ x, scale = TRUE, ncomp = 5)
summary(pcr.fit)

### Partial Least Squares

###
set.seed(1)
pls.fit <- plsr(Salary ~ ., data = Hitters, subset = train, scale = TRUE, validation = "CV")
summary(pls.fit)
validationplot(pls.fit, val.type = "MSEP")
###
pls.pred <- predict(pls.fit, x[test, ], ncomp = 1)
mean((pls.pred - y.test)^2)
###
pls.fit <- plsr(Salary ~ ., data = Hitters, scale = TRUE,
    ncomp = 1)
summary(pls.fit)
###
