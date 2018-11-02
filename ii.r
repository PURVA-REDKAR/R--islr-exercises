## Exercise 3.7.9
## PURVA REDKAR
## Summer 2018

# This question involves the use of multiple linear regression on the Autodata set.
# a) Produce a scatterplot matrix which includes all of the variablesin the data set

auto = read.csv("Auto.csv",header = T)
summary(auto)
pairs(auto)


# (b) Compute the matrix of correlations between the variables usingthe function cor(). 
# You will need to exclude the name variable,cor()which is qualitative.

correlations = cor(auto[ ,sapply(auto,is.numeric)])
correlations


# c) Use the lm()function to perform a multiple linear regression with mpgs the response and 
# all other variables exceptnameasthepredictors.
lin = lm(mpg~.-name, data = auto)

#  Use the summary()function to print the results.
summary(lin)

# Comment on the output. For instance:
# i. Is there a relationship between the predictors and the re-sponse?

## To determine whether there is a relationship between predictors and response, we have to see F-statistic
## We can see high  F-statistic value  and the p-value associated with this small, there can be a relationship between predictors and response.
## but we cannot say all predictors and response have might have relationship.


# ii. Which predictors appear to have a statistically significant relationship to the response?

## weight , year ,origin,displacement have high p value, hence they have significant relationship to the response


# iii. What does the coefficient for theyearvariable suggest

##  we can see that value of mgp increase by 0.75. (assumption other variable are constant)

# (d) Use the plot() function to produce diagnostic plots of the linear regression fit. Comment
# on any problems you see with the fit. Do the residual plots suggest any unusually large outliers?
#  Does the leverage plot identify any observations with unusually high leverage?

plot(lin)

## residual vs fitted curve is shows non linear variability and variability cannot be detected 


# (e) Use the * and : symbols to fit linear regression models with interaction effects. Do any
# interactions appear to be statistically significant?

cor(auto[ ,sapply(auto,is.numeric)])

fit= lm(mpg ~ weight*displacement+ horsepower*displacement, data = auto[,1:8])
summary(fit)

## Cor matrix shows that mpg  has non linear realtion with weight, displacement and horsepower
## We can see interaction between displacement and horsepower

#try a few different transformations of the variables, such aslog(X),√X,X2. Comment on your findings.



par(mfrow = c(2, 2))
plot(log(auto$weight), auto$acceleration)
plot(sqrt(auto$weight), auto$acceleration)
plot((auto$weight)^2, auto$acceleration)

## for  weight  and acceleration transformations log and sqrt similar transformations

par(mfrow = c(2, 2))
plot(log(auto$cylinders), auto$weight)
plot(sqrt(auto$cylinders), auto$weight)
plot((auto$cylinders)^2, auto$weight)


## for  auto and weight transformations log and sqrt  similar transformations

par(mfrow = c(2,2))
plot(sqrt(auto$displacement), auto$cylinders)
plot(log(auto$displacement), auto$cylinders)
plot((auto$displacement)^2, auto$cylinders)
## the transformations are similar for displacement and cylinders for log and sqrt.


par(mfrow = c(2, 2))
plot(log(auto$acceleration), auto$mpg)
plot(sqrt(auto$acceleration), auto$mpg)
plot((auto$acceleration)^2, auto$mpg)

## for  weight  and acceleration transformations all have similar transformations


