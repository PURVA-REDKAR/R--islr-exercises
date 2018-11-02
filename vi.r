## Exercise 6.8.10
## Purva Redkar
## Summer 2018


# Wage data set contains a number of other features not exploredin this chapter, such as 
# marital status (maritl), job class (jobclass),and others
# .Explore the relationships between some of these otherpredictors andwage, 
# and use non-linear fitting techniques in order tofit flexible models to the data.
# Create plots of the results obtained,and write a summary of your findings.

library("ISLR")
library("ggplot2")
set.seed(120)
attach(Wage)

summary(Wage[, c("maritl", "jobclass")] )


par(mfrow = c(1, 2))


ggplot(Wage, aes(x=maritl, y=wage)) + 
    geom_boxplot()


## From the graph we observe that people with who have marital status as married have highest wage.
## People who are never married have lowest wage
## median wages of married people is higher that all other groups 

ggplot(Wage, aes(x=jobclass, y=wage)) + 
geom_boxplot()
  
## From the graph we observe that there are 2 jobclass: industry and information.
## Information jobclass have higher wages than industrial jobclass
## median wages of information jobclass is higher than median of industrial jobclass

## With above results may conclude that a married couple earns more money on average, 
## and also that informational jobs earns more on average


library(gam)
gam.fit0 <- gam(wage ~ lo(year, span = 0.7) + s(age, 5) + education, data = Wage)
gam.fit1 <- gam(wage ~ lo(year, span = 0.7) + s(age, 5) + education + jobclass, data = Wage)
gam.fit2 <- gam(wage ~ lo(year, span = 0.7) + s(age, 5) + education + maritl, data = Wage)
gam.fit3 <- gam(wage ~ lo(year, span = 0.7) + s(age, 5) + education + jobclass + maritl, data = Wage)
anova(gam.fit0, gam.fit1, gam.fit2, gam.fit3)

par(mfrow = c(3, 3))
plot(gam.fit3, se = T, col = "blue")

## The function of age has 5 degrees of freedom. Marital status and job class are qualitative 
## From the p-value(which  indicates the difference between the model and previous model is statistically significant or not)
## and deviance, we can see that marital status and job and education class (gam.fit3) improve
## models significantly.

summary(Wage[, c("health", "race")] )


ggplot(Wage, aes(x=race, y=wage)) + 
geom_boxplot()
## Asians have highest wage 

ggplot(Wage, aes(x=health, y=wage)) + 
geom_boxplot()

## People in very good health have higher wages.

gam.m1 = gam(wage~s(year ,4)+s(age ,5) ,data=Wage)
gam.m2 = gam(wage~s(year ,4)+s(age ,5)+health ,data=Wage)
gam.m3 = gam(wage~s(year ,4)+s(age ,5)+race ,data=Wage)
anova(gam.m1, gam.m2, gam.m3)
par(mfrow=c(2,2))
plot(gam.m2, se=TRUE, col="blue")

## From the p-value and deviance, we can see that health  improve
## models significantly.
