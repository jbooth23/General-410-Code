cdi.df=read.csv(file.choose()) #reads in data from cdi.csv
head(cdi.df) #prints the first few lines of cdi.df

# calculates population density and crimes per thousand. Includes as new columns in cdi.df
cdi.df$PopDens=cdi.df$TotalPop/cdi.df$LandArea
cdi.df$PerThousCrimes=cdi.df$TotalSeriousCrime/cdi.df$TotalPop*1000
cdi4.df=cdi.df[cdi.df$Region==4,] #creates new data set cdi4.df for cities in region 4

rownames(cdi4.df)=1:nrow(cdi4.df)

#this command creates a multivariate linear model between crime rate and income per capita, population density, and % high school
#diplomas
cdi4.lm=lm(PerThousCrimes~PerCapInc+PopDens+PercHS,data=cdi4.df)

head(cdi4.df) #this provides the first few lines of data in cdi4.df.

summary(cdi4.lm) #gives us information about the multivariable linear model including the coefficients, standard errors, 
#RSE, R^2 value, p values, and the degrees of freedom.

#below is code for the boxplot and QQ plot of the linear model residuals.

par(mfrow=c(1, 2)) #reorders plot space into 1 row, 2 columns

boxplot(cdi4.lm$residuals,ylab="Residual values",col="grey") #this makes a boxplot of the linear model residuals
qqnorm(cdi4.lm$residuals, ylab="Residual Values") #this makes a QQ plot of the linear model residuals
qqline(cdi4.lm$residuals, lwd=2) #best fit line for the QQ plot

cdi4.infl=influence(cdi4.lm)
# first three leverage values
cdi4.infl$hat[1:3]

# this object stores the results of the summary command

cdi4.lm.sum=summary(cdi4.lm)

# look at the "attributes" of this new object

attributes(cdi4.lm.sum)

# One of the attributes is "sigma," which is the residual standard error. We can use this to calculate our standardized residuals

cdi4.stdRes=cdi4.lm$residuals/(cdi4.lm.sum$sigma*sqrt(1-cdi4.infl$hat))

# The function R standard also calculates the standardized residuals directly

cdi4.stdRes=rstandard(cdi4.lm)

par(mfrow=c(1, 2)) #reorders plot space into 1 row, 2 columns

boxplot(cdi4.stdRes,ylab="Standardized Residuals",col="yellow")
qqnorm(cdi4.stdRes, ylab="Standardized Residuals") #this makes a QQ plot of the standardized residuals
qqline(cdi4.stdRes, lwd=2) #best fit line for the QQ plot

par(mfrow=c(2, 2)) #reorders plot space into 1 row, 2 columns

plot(cdi4.df$PopDens, cdi4.stdRes, xlab="Population Density",
     ylab="Standardized Residuals",pch=16)
plot(cdi4.df$PerCapInc,cdi4.stdRes,xlab="Personal Per Capita Income ($)",
     ylab="Standardized Residuals",pch=16)
plot(cdi4.df$PercHS,cdi4.stdRes,xlab="% of population with high school diploma",
     ylab="Standardized Residuals",pch=16)
plot(cdi4.lm$fitted.values,cdi4.stdRes,xlab="Plotted y values",
     ylab="Standardized Residuals",pch=16)

length(cdi4.stdRes)
length(cdi4.df$PopDens)   #double checks that all the variables contain the same number of observations
length(cdi4.lm$fitted.values)

#below we create four plots to examine the residuals and standardized residuals in R

cdi4.cooks=cooks.distance(cdi4.lm)
par(mfrow=c(1,1),mar=c(3.5,3.5,0.5,0.5),mgp=c(2.5,0.5,0),las=1)
plot(cdi4.cooks,ylab="Cook's distance")
abline(h=c(1,2),lwd=2) #draws horizontal lines at values 1 and 2 for Cook's distance

# set up the graphing window
par(mfrow=c(2,2),mar=c(3.5,3.5,1.5,0.5),mgp=c(2.5,0.5,0),las=1)
plot(cdi4.lm)

#This command returns details for the 15th, 44th, and 52nd rows of data which are all flagged in the Residuals vs Leverage
#chart. 
cdi4.df[c(15,44,52),]
