## Excercise 8.4.8
## Purva Redkar
## Summer 2018


# 8. In the lab, a classification tree was applied to the Carseats data set after converting Sales
# into a qualitative response variable. Now we will seek to predict Sales using regression trees
# and related approaches, treating the response as a quantitative variable.

library(ISLR)
attach(Carseats)

# a. Split the data set into a training set and a test set

set.seed(1)
train <- sample(1:nrow(Carseats), nrow(Carseats) / 2)
Carseats.train <- Carseats[train, ]
Carseats.test <- Carseats[-train, ]

# b. Fit a regression tree to the training set. Plot the tree, and inter-pret the results. 
# What test MSE do you obtain?

library(tree)
tree.carseats <- tree(Sales ~ ., data = Carseats.train)
summary(tree.carseats)

plot(tree.carseats)
text(tree.carseats, pretty = 0)

error <- predict(tree.carseats, newdata = Carseats.test)
mean((error - Carseats.test$Sales)^2)

## From summary, we can say that "ShelveLoc","Price","Age","Advertising","Income" ,"CompPrice" have been used to construct tree.
## Number of node terminals for the tree is 18.
## ShelveLoc is used as main criteria for spliting the tree.
## MSE (Mean Squared error) obtained is 4.148897.(i.e the average squared difference between 
## the estimated values and what is estimated is 4.148897)


# c. cross-validation in order to determine the optimal level oftree complexity.
# Does pruning the tree improve the test MSE

cv.carseats <- cv.tree(tree.carseats)
plot(cv.carseats$size, cv.carseats$dev, type = "b")
tree.min <- which.min(cv.carseats$dev)
points(tree.min, cv.carseats$dev[tree.min], col = "red", cex = 2, pch = 20)

## Based on Cross validation, optimal size of tree is 8

pruning.carseats <- prune.tree(tree.carseats, best = 8)
plot(pruning.carseats)
text(pruning.carseats, pretty = 0)

error <- predict(pruning.carseats, newdata = Carseats.test)
mean((error - Carseats.test$Sales)^2)

## After pruning, MSE is 5.09. This increased the MSE. Hence there is a declination in error rate .

# d.Use the bagging approach in order to analyze this data.
# What test error rate do you obtain ? Use the “importance()”
# function to determine which variables are most important

library(randomForest)


bag.carseats <- randomForest(Sales ~ ., data = Carseats.train, mtry = 10, ntree = 500, importance = TRUE)
yhat.bag <- predict(bag.carseats, newdata = Carseats.test)
mean((yhat.bag - Carseats.test$Sales)^2)

## MSE is 2.63. there is a decrease in MSE. Which means that error rate has been improved.

importance(bag.carseats)

## “Price” and “ShelveLoc” have are most pure node and can be used as best split 
##  as they have higher IncNodePurity value

# e. Use random forests to analyze this data. What test error rate do you obtain ? Use the “importance()” function to determine which variables are most important. Describe the effect of m, 
# the number of variables considered at each split, on the error rate obtained

rf.carseats <- randomForest(Sales ~ ., data = Carseats.train, mtry = 3, ntree = 500, importance = TRUE)
error.rf <- predict(rf.carseats, newdata = Carseats.test)
mean((error.rf - Carseats.test$Sales)^2)

## MSE is 3.32. there is a increase  in MSE. Which means that error rate has been decreased (compared to bagging).

importance(rf.carseats)

## “Price” and “ShelveLoc” have are most pure node and can be used as best split 
##  as they have higher IncNodePurity value

