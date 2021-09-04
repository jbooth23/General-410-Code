senic.df=read.csv(file.choose()) #picks the appropriate file for my lab
head(senic.df) #prints the first few rows of all 7 variables in scenic.df

senic.df$MedSchAff=factor(senic.df$MedSchAff)

summary(senic.df$MedSchAff) #this line of code tells us that 17 of the 113 hospitals are affiliated with a medical school and
#96 are not

par(mfrow=c(7,7))
plot(senic.df) #this plots the 6 predictor variables and 1 response variable against all other variables. 
#We can use this to assess collinearity among the variables depending on how closely they vary with each other

senic1.lm=lm(InfectionRisk~LenStay+Age+NumBeds+PatientsPerDay+NumNurse,
             data=senic.df) #creates a linear model of the senic.df data between infection risk and 5 predictors
car::vif(senic1.lm) #this command tells us the VIF value for each response variable in the linear model

#what we found here is there is 'acceptable' to minimal levels of multicollinearity for lenstay, age, and numnurse, while finding
#severe multicollinearity for numbeds and patientsperday

senic.df$PatientsPerNurse=senic.df$PatientsPerDay/senic.df$NumNurse

senic1b.lm=lm(InfectionRisk~LenStay+Age+PatientsPerNurse, data=senic.df) #creates a new linear model for infection risk by
#three predictor variables

car::vif(senic1b.lm) #calculates the VIF for each predictor variable in senic1b

senic2.lm=lm(InfectionRisk~LenStay*MedSchAff+Age*MedSchAff+
               PatientsPerNurse*MedSchAff,data=senic.df)

# AIC is the default
senicStep.aic=step(senic2.lm)
senicStep.aic #uses the AIC to calculate a model that minimizes complexity (no. of predictor variables) and maximizes accuracy of
#the model

senic3.lm=lm(InfectionRisk ~ LenStay + PatientsPerNurse, data=senic.df) #creates a
#linear model that minimizes the AIC

summary(senic3.lm) #provides a summary of the linear model including residual squared error (RSE), the estimated coefficients
#and intercept, the p values for each, and the degrees of freedom

par(mfrow=c(2,2))
plot(senic3.lm) #this creates 4 residual plots of scenic3.lm


