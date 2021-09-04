# below is the code for both parts of the lab. Done by James Booth on January 14, 2021 


# read.csv is the function we use to read the contents of a *.csv file.
# This assigns the contents of that file to a new dataframe (see below).
# the file.choose() option directs R to allow you to
# choose which file is read in. You can also type in the
# path to the directory structure (in quotes)
anscombe1.df=read.csv(file.choose())
# We can look at the new object to make sure we read it in correctly
# This returns the first 6 lines
head(anscombe1.df)
##     X1 Y1
## 1 8.04 10
## 2 6.95  8
## 3 7.58 13
## 4 8.81  9
## 5 8.33 11
## 6 9.96 14
# This views the entire object. Note the capital V!
View(anscombe1.df)


mean(anscombe1.df$X1) # calculate the mean value for the first column
## [1] 7.500909
mean(anscombe1.df$Y1) # calculate the mean value for the second column
## [1] 9
sd(anscombe1.df$X1) # the standard deviation of the first column
## [1] 2.031568
var(anscombe1.df$X1) # the variance of the first column
## [1] 4.127269
summary(anscombe1.df$X1) # the 5-number summary of the first column
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
##   4.260   6.315   7.580   7.501   8.570  10.840
summary(anscombe1.df)
##        X1               Y1      
##  Min.   : 4.260   Min.   : 4.0  
##  1st Qu.: 6.315   1st Qu.: 6.5  
##  Median : 7.580   Median : 9.0  
##  Mean   : 7.501   Mean   : 9.0  
##  3rd Qu.: 8.570   3rd Qu.:11.5  
##  Max.   :10.840   Max.   :14.0
# gives the 5-number summary for each column
# in the data frame


hist(anscombe1.df$X1) # a histogram of the first column
boxplot(anscombe1.df$X1) # a boxplot of the first column
plot(anscombe1.df$X1,anscombe1.df$Y1)


# create a random set of 100 x variables
# rnorm(n.mu.sigma) draws a random set of n (100 in this case)
# draws from a normal distribution with mean mu (10 in this case)
# and standard deviation sigma (2 in this case)
x.fake=rnorm(100,10,2)
# Now we create a set of fake y-values that have a linear
# relationship with the x-values, with a random error component
y.fake=100-1.5*x.fake+rnorm(100,0,2)


# first setup the plotting window using par
# mgp specifies the lines in the margin at which the
# axis label, axis tick marks, and axis line are drawn
# las=1 ensures that axis numbers are aligned in the reading direction
par(mfrow=c(1,2),mar=c(3,3,0.5,0.5),mgp=c(2,0.5,0),las=1)
# this creates a vertical boxplot.
boxplot(x.fake,ylab ="Fake x data")
boxplot(y.fake,ylab="Fake y data")


# Note the number of lines for side 3 (top) are increased here
# This makes room for the plot title.
par(mfrow=c(1,2),mar=c(3,3,2,0.5),mgp=c(2,0.5,0),las=1)
# we use the main argument to write a title on each graph
hist(x.fake,xlab ="Fake x data",col="purple",
     main="These are fake data!")
hist(y.fake,xlab="Fake y data",col="magenta",
     main="These also are fake data!")


par(mar=c(3,3,2,2),mgp=c(2,0.5,0),las=1)
plot(x.fake,y.fake,xlab ="Fake x data",
     ylab="Fake y data",pch=16,main="Still fake data with an extra fake
point")
# add points to the current plot at the
# specified coordinates. Change the point symbol and color
points(x=9,y=80,pch=17,col="turquoise")
