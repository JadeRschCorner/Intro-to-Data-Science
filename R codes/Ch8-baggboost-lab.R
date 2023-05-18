
# Lab: Decision Trees


## Fitting Classification Trees

###
# install.packages("tree")
library(tree)
# the "tree" package contains functions for constructing trees for regression and classification.

###
library(ISLR2)
attach(Carseats)
High <- factor(ifelse(Sales <= 8, "No", "Yes"))
###
Carseats <- data.frame(Carseats, High)
# add a new column "High" to the Carseats data set
###
tree.carseats <- tree(High ~ . - Sales, Carseats)
# tree() function from "tree" package fits a classification tree
###
summary(tree.carseats)
# In the output of summary(), deviance is -2*n_mk*log(p_mk) summed over all leaves and all classes. 
# See Page 353 of textbook for definition. A smaller deviance indicates a better fit to the training data. 
# Residual mean deviance is deviance/(number of observations - number of terminal nodes)
###
plot(tree.carseats)
text(tree.carseats, pretty = 0)
###
tree.carseats
###
#split the data into a training set and a testing set
set.seed(2)
train <- sample(1:nrow(Carseats), 200)
Carseats.test <- Carseats[-train, ]
High.test <- High[-train]
tree.carseats <- tree(High ~ . - Sales, Carseats,
    subset = train)
tree.pred <- predict(tree.carseats, Carseats.test,
    type = "class")
table(tree.pred, High.test)
(104 + 50) / 200
###
# tree pruning
set.seed(7)
cv.carseats <- cv.tree(tree.carseats, FUN = prune.misclass)
# cv.tree() function performs cross-validation to determine optimal 
# level of tree complexity "FUN=prune.misclass" indicates classification 
# error rate (1-max(p_mk)) instead of deviance is used to guide the cv 
# and pruning process.
names(cv.carseats)
cv.carseats
# In this example, the $dev in the output corresponds to cross-validation 
# error rate; and k is the tuning parameter alpha in tree pruning. 
# "k=-Inf" corresponds to a very small (negative) alpha value (something like -1.0e+200). 
# Note that by default the tree() function is based on minimum deviance 
# (which is # similar to cross entropy). 
# "k=0" correspond to the the full tree from the tree() function
# based on minimum classification error rate.

###
par(mfrow = c(1, 2))
plot(cv.carseats$size, cv.carseats$dev, type = "b")
plot(cv.carseats$k, cv.carseats$dev, type = "b")
###
prune.carseats <- prune.misclass(tree.carseats, best = 9)
# the prune.misclass() function prunes the tree to the specified number of nodes.
plot(prune.carseats)
text(prune.carseats, pretty = 0)
###
tree.pred <- predict(prune.carseats, Carseats.test,
    type = "class")
table(tree.pred, High.test)
(97 + 58) / 200
###
# prune to a larger tree (14 nodes)
prune.carseats <- prune.misclass(tree.carseats, best = 14)
plot(prune.carseats)
text(prune.carseats, pretty = 0)
tree.pred <- predict(prune.carseats, Carseats.test,
    type = "class")
table(tree.pred, High.test)
(102 + 52) / 200

## Fitting Regression Trees

# The "Boston" data set records medv (median house value) for 506 neighborhoods 
# around Boston. 
# CRIM - per capita crime rate by town
# ZN - proportion of residential land zoned for lots over 25,000 sq.ft.
# INDUS - proportion of non-retail business acres per town.
# CHAS - Charles River dummy variable (1 if tract bounds river; 0 otherwise)
# NOX - nitric oxides concentration (parts per 10 million)
# RM - average number of rooms per dwelling
# AGE - proportion of owner-occupied units built prior to 1940
# DIS - weighted distances to five Boston employment centres
# RAD - index of accessibility to radial highways
# TAX - full-value property-tax rate per $10,000
# PTRATIO - pupil-teacher ratio by town
# B - 1000(Bk - 0.63)^2 where Bk is the proportion of blacks by town
# LSTAT - % lower socialeconomic status of the population
# MEDV - Median value of owner-occupied homes in $1000's

###
set.seed(1)
train <- sample(1:nrow(Boston), nrow(Boston) / 2)
tree.boston <- tree(medv ~ ., Boston, subset = train)
summary(tree.boston)
###
plot(tree.boston)
text(tree.boston, pretty = 0)
###
cv.boston <- cv.tree(tree.boston)
plot(cv.boston$size, cv.boston$dev, type = "b")
###
prune.boston <- prune.tree(tree.boston, best = 5)
plot(prune.boston)
text(prune.boston, pretty = 0)
###
yhat <- predict(tree.boston, newdata = Boston[-train, ])
boston.test <- Boston[-train, "medv"]
plot(yhat, boston.test)
abline(0, 1)
mean((yhat - boston.test)^2)  #test MSE

## Bagging and Random Forests

###
# install.packages("randomForest")
library(randomForest)
# the randomForest() function in the "randomForest" package can perform both random
# forests and bagging, because bagging is a special case of random forests.

par(mfrow=c(1,1))
set.seed(1)
bag.boston <- randomForest(medv ~ ., data = Boston,
    subset = train, mtry = 12, importance = TRUE)
# mtry=12 indicates all 12 predictors are considered for each split of the tree
# This is essentially bagging.
# importance=TRUE means that the importance of predictors are assessed.
bag.boston
###
yhat.bag <- predict(bag.boston, newdata = Boston[-train, ])
plot(yhat.bag, boston.test)
abline(0, 1)
mean((yhat.bag - boston.test)^2)
###
bag.boston <- randomForest(medv ~ ., data = Boston,
    subset = train, mtry = 12, ntree = 25)
# ntree=25 defines 25 trees to grow by randomForest.
yhat.bag <- predict(bag.boston, newdata = Boston[-train, ])
mean((yhat.bag - boston.test)^2)
###
set.seed(1)
rf.boston <- randomForest(medv ~ ., data = Boston,
    subset = train, mtry = 6, importance = TRUE)
# mtry=6 indicates 6 predictors are considered for each split of the tree. This is random forest.

yhat.rf <- predict(rf.boston, newdata = Boston[-train, ])
mean((yhat.rf - boston.test)^2)
###
importance(rf.boston)
# importance() function outputs the importance of each variable.
# The output contains two measures: %IncMSE and IncNodePurity
# %IncMSE: increase in MSE if that variable is excluded from the model.
# IncNodePurity: increase in node purity when the tree is split over that variable.
# In regression trees, the node impurity is measured by the training RSS.
# In classification trees, the node impurity is measured by the deviance.
# Higher values of these two measures indicate higher importance.
###
varImpPlot(rf.boston)

## Boosting

###
# install.packages("gbm")
library(gbm)
# the gbm() function in the "gbm" package can fit boosted trees for regression or classification

set.seed(1)
boost.boston <- gbm(medv ~ ., data = Boston[train, ],
    distribution = "gaussian", n.trees = 5000,
    interaction.depth = 4)
###
summary(boost.boston)
# produces a relative influence plot and statistics.
###
par(mfrow=c(1,2))
plot(boost.boston, i = "rm")
# Partial dependence plot. These plots illustrate the marginal effect 
# of the selected variables on the response after integrating out the other variables.
plot(boost.boston, i = "lstat")
###
yhat.boost <- predict(boost.boston,
    newdata = Boston[-train, ], n.trees = 5000)
mean((yhat.boost - boston.test)^2)
###
boost.boston <- gbm(medv ~ ., data = Boston[train, ],
    distribution = "gaussian", n.trees = 5000,
    interaction.depth = 4, shrinkage = 0.2, verbose = F)
# use a different shrinkage parameter of 0.2, different from the default value of 0.001.

yhat.boost <- predict(boost.boston,
    newdata = Boston[-train, ], n.trees = 5000)
mean((yhat.boost - boston.test)^2)

## Bayesian Additive Regression Trees

###
# install.packages("BART") 
library(BART)
x <- Boston[, 1:12]
y <- Boston[, "medv"]
xtrain <- x[train, ]
ytrain <- y[train]
xtest <- x[-train, ]
ytest <- y[-train]
set.seed(1)
bartfit <- gbart(xtrain, ytrain, x.test = xtest)
###
yhat.bart <- bartfit$yhat.test.mean
mean((ytest - yhat.bart)^2)
###
ord <- order(bartfit$varcount.mean, decreasing = T)
bartfit$varcount.mean[ord]
###
