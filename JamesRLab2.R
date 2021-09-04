foot.df=read.csv(file.choose())
head(foot.df)

#imported data from FOOT.excel file. head command reads the first six lines of data

dim(foot.df) #this tells us the number of observations in the data set. There are 40 observations each for 6 columns of data

#the predictor variable is shoe print length and the response variable is height, because we are trying to use foot data of males
#to predict their respective heights.

foot2.df=read.csv(file.choose()) #reads in data from foot2.csv, a file containing only the shoe print measurements and the
#heights of various men

head(foot2.df)

dim(foot2.df) #there are 40 observations for 2 columns of data

View(foot2.df)

foot2.lm = lm(ï..Shoe.Print~Height, data=foot2.df)

summary(foot2.lm)

plot(foot2.df, xlab = "Shoe Print Length (cm)", ylab = "Suspect Height (cm)")  #provides a scatterplot of the data in foot2.csv
#labels axis appropriately

qt(0.975, 38)

