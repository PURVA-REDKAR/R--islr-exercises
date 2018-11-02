## Exercise 6.8.10
## Purva Redkar
## Summer 2018

 
# 10. We have seen that as the number of features used in a model increases,
# the training error will necessarily decrease, but the test error may not.
# We will now explore this in a simulated data set.
#(a) Generate a data set with p = 20 features, n = 1,000 observations,
# and an associated quantitative response vector generated
# according to the model
# Y = Xβ + ,
# where β has some elements that are exactly equal to zero.
# 264 6. Linear Model Selection and Regularization


library(leaps)
set.seed(1)
p <- 20
n <- 1000
x <- matrix(rnorm(n*p),n,p)
b <- rnorm(p)
b[3] <- 0
b[5] <- 0
b[8] <- 0
b[9] <- 0
b[11] <- 0

y <- x %*% b + rnorm(1000)


# (b) Split your data set into a training set containing 100 observations
# and a test set containing 900 observations.

train <- sample(seq(1000),100,replace=FALSE)
x.train <- x[ train,]
x.test <- x[-train,]
y.train <- y[ train,]
y.test <- y[-train,]


# (c) Perform best subset selection on the training set, and plot the
# training set MSE associated with the best model of each size.


regfit <- regsubsets(y~.,data = data.frame(x=x.train, y=y.train), nvmax=20)
matrix <- model.matrix(y~.,data = data.frame(x=x.train, y=y.train), nvmax=20)
error <- rep(NA,p)
for (i in 1:20) {
coef <- coef(regfit,id=i)
pred <- matrix[,names(coef)] %*% coef
error[i]<- mean((pred-y.train)^2)
}
plot(error,xlab = "predictors",ylab = " MSE",main = " MSE associated with the best model",pch = 19,type = "b" )


# (d) Plot the test set MSE associated with the best model of each size.

test.matrix <- model.matrix(y~.,data = data.frame(x=x.test, y=y.test), nvmax=20)
test.error <- rep(NA,p)
for (i in 1:20) {
coef <- coef(regfit,id=i)
pred <- test.matrix[,names(coef)] * coef
test.error[i]<- mean((pred-y.test)^2)
}


plot(test.error,ylab = "Test MSE",xlab = " predictors",main = " MSE associated with the best model of each size",pch = 19,type = "b")



# (e) For which model size does the test set MSE take on its minimum
# value? Comment on your results. If it takes on its minimum value
# for a model containing only an intercept or a model containing
# all of the features, then play around with the way that you are
# generating the data in (a) until you come up with a scenario in
# which the test set MSE is minimized for an intermediate model
# size.

points(which.min(test.error),test.error[which.min(test.error)],
col="red",pch=19)

minerr <- which.min(test.error)
minerr

coef(regfit, which.min(test.error))

## 0 beta features are 3,5,8,9,11. Subset selection on training data set was performed.
## The plot obtained showed that  20th -variables  has the smallest test MSE




# (f) How does the model at which the test set MSE is minimized
# compare to the true model used to generate the data? Comment
# on the coefficient values.

summary(regfit) 
coef(regfit, minerr)

## the 20th model has best all zero coefficient values 
## it gives lower MSE when we compared to the MSE of  the complete model and uses the ideal number. 
## it also has optimal number of features




#(g) Create a plot displaying 'p j=1(βj − βˆr j )2 for a range of values of r, where βˆr
# j is the jth coefficient estimate for the best model containing r coefficients. Comment on what you observe. How
# does this compare to the test MSE plot from (d)?

val.errors <- rep(NA, 20)
x_cols = colnames(x, do.NULL = FALSE, prefix = "x.")
for (i in 1:20) {
    coefi <- coef(regfit, id = i)
    val.errors[i] <- sqrt(sum((b[x_cols %in% names(coefi)] - coefi[names(coefi) %in% x_cols])^2) + sum(b[!(x_cols %in% names(coefi))])^2)
}
plot(val.errors, xlab = "Number of coefficients", ylab = "Error between estimated and true coefficients", pch = 19, type = "b")
points(which.min(val.errors),val.errors[which.min(val.errors)],col="blue",pch=19)

## from the graph we can see that the model with 7 variables gives minimum error between the true and estimated coefficient.
## from (d), we can conclude that test error is lower for Error between estimated and true coefficients
## when we try this model with different variables the test error was  the minimum in (d
## From this  we can conclude that the suitable fit for the true co-efficients will not direclty relate to the minimized test error. 



