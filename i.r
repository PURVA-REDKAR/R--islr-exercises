## Exercise 2.4.8
## PURVA REDKAR
## Summer 2018

# 8. This exercise relates to the College data set,
#    which can be found in the file College.csv. 
#    IT contains a number of variables for 777 different
#    universities and colleges in the US. The
#    variables are
# • Private : Public/private indicator
# • Apps : Number of applications received
# • Accept : Number of applicants accepted
# • Enroll : Number of new students enrolled
# • Top10perc : New students from top 10 % of high school class
# • Top25perc : New students from top 25 % of high school class
# • F.Undergrad : Number of full-time undergraduates
# • P.Undergrad : Number of part-time undergraduates
# • Outstate : Out-of-state tuition
# • Room.Board : Room and board costs
# • Books : Estimated book costs
# • Personal : Estimated personal spending
# •PhD: Percent of faculty with Ph.D.’s
# •Terminal: Percent of faculty with terminal degree
# •S.F.Ratio: Student/faculty ratio
# •perc.alumni: Percent of alumni who donate
# •Expend: Instructional expenditure per student
# •Grad.Rate: Graduation rate

# Before reading the data into R, it can be viewed in
# Excel or a text editor.
# (a) Use the read.csv() function to read the data into
# R. Call the loaded data college. Make sure that you
# have the directory set to the correct location for the
# data.
college = read.csv("College.csv",header = T)

# b) Look at the data using the fix() function. You
#   should notice that the first column is just the name of
#   each university. We don’t really want R to treat this
#   as data. However, it may be handy to have these names
#    for later. Try the following commands:

rownames(college) = college[, 1]
fix(college)

# You should see that there is now arow.names column with thename of each university recorded.
# This means thatRhas giveneach row a name corresponding to the appropriate university.
# Rwill not try to perform calculations on the row names.
# However,we still need to eliminate the first column in the data where thenames are stored. 

college = college[, -1]
fix(college)

# Now you should see that the first data column is
# Private. Note that another column labeled row.names now
# appears before the Private column. However, this is not
# a data column but rather the name that R is giving to
# each row.
# i. Use the summary() function to produce a numeric
# of the variables in the data set.
 summary(college)
 
# Use the pairs()function to produce a scatterplot matrix ofthe
# first ten columns or variables of the data. Recall thatyou can reference the 
# first ten columns of a matrixAusing A[,1:10]

pairs(college[, 1:10])

# ii. Use the plot() function to produce side-by-side
#    boxplots of Outstate versus Private
plot(college$Outstate ~ college$Private, xlab = "Private", ylab = "Outstate")

# iii.create a new qualitative variable, calledElite,
# by binning the Top 10perc variable.
# We are going to divide universitiesinto 
# two groups based on whether or not the proportionof students coming from the
# top 10 % of their high schoolclasses exceeds 50 %

Elite=rep("No",nrow(college))
Elite[college$Top10perc >50]="Yes"
Elite=as.factor(Elite)
college=data.frame(college ,Elite)

# i.v Use the summary()function to see how many elite 
# univer-sities there are. 
# Now use the plot()
# function to produceside-by-side boxplots of Out state versus Elite

summary(Elite)

plot(college$Outstate ~ college$Elite, xlab = "Elite", ylab = "Outstate", main="outstation vs ellite")

# v. Use thehist()function to produce some histograms withdiffering numbers of
# bins for a few of the quantitative vari-ables. You may find the
# command par(mfrow=c(2,2))useful:it will divide the print window 
# four regions so that fourplots can be made simultaneously. 
# Modifying the argumentsto this function will divide the screen in other way

par(mfrow = c(2, 2))
hist(college$Apps)
hist(college$Accept)
hist(college$Top10perc)
hist(college$Expend)
hist(college$PhD)

# vi.Continue exploring the data, 
# and provide a brief summaryof what you discover

par(mfrow = c(3, 3))
contrasts(college$Private)
## higer enrollerment for public college
plot(college$Private, college$Enroll , main="Enrollment")


contrasts(college$Elite)
## PhD is greater in Ellite Universitites
plot(college$Elite,college$PhD,main="PhD enrollerment")



## sum of expenses higher in private , mean of private college expense is greater in private college
college$Sum <- college$Room.Board + college$Books + college$Personal + college$Outstate
plot(college$Private,college$Sum, main="Expense")
 
## lesser Application to Elite school
plot(college$Elite,college$Apps, main="Number of Application")
 
## Grad rate higher in private college
plot(college$Private, college$Grad.Rate ,main="Graduation Rate")

## more private college in top 10 perc
plot(college$Private, college$Top10perc ,main="Top10perc colleges") 
 