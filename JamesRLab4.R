cdi.df=read.csv(file.choose()) #reads in data from cdi.csv
head(cdi.df) 

# creates column for population density
cdi.df$PopDens=cdi.df$TotalPop/cdi.df$LandArea
# creates new column for crimes per 1000 people by dividing the total crimes by the total population and then
# multiplying by 1000
cdi.df$PerThousCrimes=cdi.df$TotalSeriousCrime/cdi.df$TotalPop*1000

# creates separate data set for cdi, region 4
cdi4.df=cdi.df[cdi.df$Region==4,]

par(mfrow=c(2,3)) #reorders the plot space into 2 rows and 3 columns

#below is the code for all 6 scatterplots

plot(cdi.df$PerCapInc,cdi.df$PerThousCrimes,xlab="Personal Per Capita Income ($)",
     ylab="Crime Rate (per 1,000 people)",pch=16)
plot(cdi.df$PopDens,cdi.df$PerThousCrimes,xlab="Population Density",
     ylab="Crime Rate (per 1,000 people)",pch=16)
plot(cdi.df$PercHS,cdi.df$PerThousCrimes,xlab="% of population with high school diploma",
     ylab="Crime Rate (per 1,000 people)",pch=16)
plot(cdi.df$PerCapInc,cdi.df$PopDens,xlab="Personal Per Capita Income ($)",
     ylab="Population Density",pch=16)
plot(cdi.df$PerCapInc,cdi.df$PercHS,xlab="Personal Per Capita Income ($)",
     ylab="% of population with high school diploma",pch=16)
plot(cdi.df$PopDens,cdi.df$PercHS,xlab="Population Density",
     ylab="% of population with high school diploma",pch=16)

head(cdi4.df) 

cdi4.lm = lm(PerThousCrimes ~ PerCapInc + PopDens + PercHS, data=cdi4.df) #creates a linear model for cdi4.df, 
#this will be used throghout the rest of the lab

summary(cdi4.lm) #this gives the equation of the MLS line and additional information for this lab

#the following calculates the p-values for each coefficient
2*(1-pt(6.038, 73))
2*(1-pt(0.364, 73))
2*(1-pt(2.845, 73))
2*(pt(-2.447,73))

boxplot(cdi4.lm$residuals,ylab="Residual values",col="grey") #this makes a boxplot of the linear model residuals
qqnorm(cdi4.lm$residuals, ylab="Residual Values") #this makes a QQ plot of the linear model residuals
qqline(cdi4.lm$residuals, lwd=2) #best fit line for the QQ plot

newdata=data.frame(PopDens=c(900,10,10000),PercHS=c(75,80,72),PerCapInc=c(11000,19000,35000))

cdi4.pred=predict(cdi4.lm,interval = "prediction",newdata = newdata) #this creates a prediction interval for newdata
head(cdi4.pred) #this gives the predicted values for each combination of the three predictor variables and 
#also provies the upper and lower bounds