## Exercise 4.7.11
## PURVA REDKAR
## Summer 2018

# In this problem, you will develop a model to predict whether a given
# car gets high or low gas mileage based on the “Auto” data set.

require(ISLR)
data("Auto")

# Create a binary variable, “mpg01”, that contains a 1 if “mpg” contains a 
# value above its median, and a 0 if “mpg” contains a value below its 
# median. You can compute the median using the median() function. Note 
# you may find it helpful to use the data.frame() function to create a single 
# data set containing both “mpg01” and the other “Auto” variables



mpg01 <- rep(1, length(Auto$mpg))
mpg01[Auto$mpg < median(Auto$mpg)] <- 0
auto <- data.frame(Auto, mpg01)
summary(auto)


# b.Explore the data graphically in order to investigate the association
# between mpg01 and the other features. Which of the other features seem
# most likely to be useful in predicting mpg01?
# Scaterplots and boxplots may be useful tools to answer this question. Describe your findings


cor(auto[ ,sapply(auto,is.numeric)])

## Scaterplots
pairs(auto[, -9])

par(mfrow=c(3,3))
qplot(jitter(as.integer(auto$mpg01)),auto$horsepower,main = "horsepower vs mpg01")
qplot(jitter(as.integer(auto$mpg01)),auto$cylinders,main = "cylinders vs mpg01")
qplot(jitter(as.integer(auto$mpg01)),auto$weight,main = "weight vs mpg01")
qplot(jitter(as.integer(auto$mpg01)),auto$displacement ,main = "displacement vs mpg01")

par(mfrow=c(3,3))
plot( auto$mpg01, auto$cylinders, main = "cylinders vs mpg01")
plot(auto$mpg01, auto$weight, main = "weight vs mpg01")
plot(auto$mpg01, auto$displacement, main = "displacement vs mpg01")
plot(auto$mpg01, auto$horsepower, main = "“horsepower vs mpg01")



## boxplots

par(mfrow=c(3,3))
boxplot(auto$cylinders ~ auto$mpg01, main = "Cylinders and  mpg01",xlab="Cylinders", ylab="mpg01")
boxplot(auto$displacement ~ auto$mpg01, main = "Displacement and mpg01" ,xlab="Displacement", ylab="mpg01")
boxplot(auto$horsepower ~ auto$mpg01, main = "Horsepower vs mpg01" ,xlab="Horsepower", ylab="mpg01" )
boxplot(auto$weight ~ auto$mpg01, main = "Weight vs mpg01" ,xlab="Weight", ylab="mpg01")
boxplot(auto$acceleration ~ auto$mpg01,  main = "Acceleration vs mpg01" ,xlab="Acceleration", ylab="mpg01")
boxplot(auto$year ~ auto$mpg01,  main = "Year vs mpg01" , xlab="Year", ylab="mpg01")



## strong association between “mpg01” with “cylinders”, “weight”, “displacement” and “horsepower" can we seen 
## from correlation matrix
## from Boxplot we can see that below median as mpg01 decreases,values of cylinders, weight,
## displacement, horsepower increases. hence we we can confidently predict mpg01
## Moreover, we can see outliers in each category


# c.Split the data into a training set and a test set.
require(caTools)
train <- (auto$year %% 2 == 0)
Auto.train <- Auto[train, ]
Auto.test <- Auto[!train, ]
mpg01.test <- mpg01[!train]

# d. perform LDA on the training data in order to predictmpg01using the variables 
# that seemed most associated with mpg01 in(b). What is the test error of 
# the model obtained
require(MASS)
 
lda.fit = lda(mpg01 ~ cylinders + weight + displacement + horsepower, data = Auto, subset =train)
lda.pred = predict(lda.fit, Auto.test)
lda.class = lda.pred$class
table(lda.class, mpg01.test)
mean(lda.class != mpg01.test)
mean(lda.class == mpg01.test)

round(sum(lda.pred$class!=mpg01.test)/nrow(Auto.test )*100,2)

## error rate = 12.6


# (e) Perform QDA on the training data in order to predict mpg01 using the variables that
# seemed most associated with mpg01 in (b). What is the test error of the model obtained?

qda.fit = qda(mpg01 ~ cylinders + weight + displacement + horsepower, data = Auto, subset =
train)
qda.pred = predict(qda.fit, Auto.test)
qda.class = qda.pred$class

table(mpg01.test, qda.class)
mean(qda.class != mpg01.test)
mean(qda.class == mpg01.test)

round(sum(qda.pred$class!=mpg01.test)/nrow(Auto.test)*100,2)
## error rate = 13.19

# (f) Perform logistic regression on the training data in order to pre- dict mpg01 using the
# variables that seemed most associated with mpg01 in (b). What is the test error of the model
# obtained?


glm.fit = glm(mpg01 ~ cylinders + displacement+ horsepower +weight , data = Auto, family =
binomial, subset = train)
summary(glm.fit)

glm.probs = predict(glm.fit, Auto.test ,type="response")
pred.glm <- rep(0, length(glm.probs))
pred.glm[glm.probs > 0.5] <- 1
table(pred.glm, mpg01[!train])
mean(pred.glm != mpg01[!train])



round(sum(pred.glm!=mpg01.test)/nrow(Auto.test)*100,2)

## error rate = 12.09

# (g) Perform KNN on the training data, with several values of K, in order to predict mpg01. Use
# only the variables that seemed most associated with mpg01 in (b). What test errors do you
# obtain? Which value of K seems to perform the best on this data set?

library(class)
traini = cbind(Auto$cylinders, Auto$weight, Auto$displacement,
Auto$horsepower)[train, ]
tests = cbind(Auto$cylinders, Auto$weight, Auto$displacement,
Auto$horsepower)[!train, ]
set.seed(1)
for(i in 1:10) {
nearest <- knn(traini, tests, mpg01[train], k=i)
print(paste0("k-->",i," , error=",
mean(nearest != mpg01.test)))
}

## "k-->1 , error=0.153846153846154"
## "k-->2 , error=0.153846153846154"
## "k-->3 , error=0.137362637362637"
## "k-->4 , error=0.159340659340659"
## "k-->5 , error=0.148351648351648"
## "k-->6 , error=0.148351648351648"
## "k-->7 , error=0.148351648351648"
## "k-->8 , error=0.159340659340659"
## "k-->9 , error=0.159340659340659"
## "k-->10 , error=0.153846153846154"
##  Error Rate for k=3 is lowest so k=3 is optimal answer



