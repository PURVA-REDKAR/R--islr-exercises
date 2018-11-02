## Exercise 5.4.6
## Purva Redkar
## Summer 2018


# 6. We continue to consider the use of a logistic regression model to predict the probability of
# default using income and balance on the Default data set. In particular, we will now compute
# estimates for the standard errors of the income and balance logistic regression coefficients in
# two different ways: (1) using the bootstrap, and (2) using the standard formula for computing
# the standard errors in the glm() function. Do not forget to set a random seed before beginning
# your analysis.

library(ISLR)
attach(Default)
set.seed(120)


summary(glm(default~income+balance,family = binomial,data=Default))$coefficients


# (b) Write a function, boot.fn(), that takes as input the Default data set as well as an index of
# the observations, and that outputs the coefficient estimates for income and balance in the
# multiple logistic regression model.


boot.fn <- function(data, index) {
      return (coef(glm(default ~ income + balance, data = data, family = "binomial", subset = index)))
}


# (c) Use the boot() function together with your boot.fn() function to estimate the standard
# errors of the logistic regression coefficients for income and balance.

library(boot)
boot(Default, boot.fn, R = 10)
boot(Default, boot.fn, R = 100)
boot(Default, boot.fn, R = 1000)


# (d) Comment on the estimated standard errors obtained using the glm() function and using
# your bootstrap function.


##  The standard errors as got from the glm function and bootstrap function differ by small amounts
##  standard error got from glm function are 4.985167e-06(income) and  2.273731e-04(balance)
##  standard error got from bootstrap is 5.063803e-06 and 2.481834e-04.
##  glm function needs noise to be removed and predictors to be fixed.  
##   bootstrap doesnt assumes that noise is removed which makes bootstrap to yeild more accurate results





