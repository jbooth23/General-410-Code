foot2.df=read.csv(file.choose())
head(foot2.df) #imports data from foot2.csv and renames to foot2.df

foot2.lm = lm(ï..Shoe.Print~Height, data=foot2.df) #creates a linear model for foot2.df, this will be used throghout the rest
#of the lab

summary(foot2.lm) #gives various information about the model including intercept, slope, and R^2 value

y.hat=foot2.lm$fitted.values

head(y.hat)

y.bar=mean(foot2.df$Height) #mean height
x.bar=mean(foot2.df$ï..Shoe.Print) #mean shoe print size

y.hat=foot2.lm$fitted.values

foot2.sse=sum((foot2.df$Height-y.hat)^2)
foot2.sse #prints the value of the SSE of foot2.df's data, calculated in the line of code above

foot2.ssr=sum((y.hat-y.bar)^2)
foot2.ssr #prints the value of the SSR of foot2.df's data, calculated in the line of code above

foot2.sst=sum((foot2.df$Height-y.bar)^2)
foot2.sst #prints the value of the SST of foot2.df's data, calculated in the line of code above

1- (1342.475/3958.755) #calculates the value of R^2 given by formula 1 - (SSE/SST)

1342.475 + 2616.28 #Adds the SSR and the SSE together. Prints output which is the SST

newdata=data.frame(ï..Shoe.Print=sort(foot2.df$ï..Shoe.Print)) #creates a new data set, which is just a 
#reordering of the 40 shoe print sizes

foot2.pred=predict(foot2.lm,interval = "prediction",newdata = newdata) #this creates a prediction interval for newdata
head(foot2.pred) #this gives the first six predicted heights based on the best fit line and also provies the upper and lower
#bounds

foot2.conf=predict(foot2.lm, interval="confidence", newdata = newdata) #creates a confidence interval for newdata
head(foot2.conf)

par(mfrow=c(1,1),mar=c(3.5,3.5,0.5,0.5),mgp=c(2.25,0.5,0),
    las=1)

plot(foot2.df$ï..Shoe.Print,foot2.df$Height,xlab="Shoe print length (cm)",
     ylab="Height (cm)",pch=16)
abline(foot2.lm$coefficients,lwd=2) #this plots observations for the two objects in foot2.lm with a best fit line

#add a line whose coordinates are the y-values in newdata. This will be for column 2, consisting of the lower bounds of the 
#prediction interval

lines(newdata$ï..Shoe.Print,foot2.pred[,2])

#now add a line for the upper interval (column 3), consisting of the upper bounds of the prediction interval

lines(newdata$ï..Shoe.Print, foot2.pred[,3])

lines(newdata$ï..Shoe.Print, foot2.conf[,2]) #add a line whose coordinates are y values in column 2, the lower bounds of the
#confidence interval for newdata

lines(newdata$ï..Shoe.Print, foot2.conf[,3]) #add a line whose coordinates are y values in column 3, consisting of the upper
#bounds of the confidence interval

newdata2=data.frame(ï..Shoe.Print=c(15,22.1,35.9,28.2,25.7))  #inserts five new data values for shoe print sizes into a new
#data set

foot2pred2.conf=predict(foot2.lm, interval="confidence", newdata = newdata2)
head(foot2pred2.conf) #produces upper and lower bounds for the confidence interval of our five x values in newdata2