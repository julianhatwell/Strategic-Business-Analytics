rm(list=ls(all=TRUE))

# Let's load our dataset and call it data
dataold=read.table('DATA_3.01_CREDIT.csv',sep=',',header=TRUE) # The function read.table enables us to read flat files such as .csv files
datanew=read.table('DATA_4.01_CREDIT2.csv',sep=',',header=TRUE) # The function read.table enables us to read flat files such as .csv files

# Now let's have a look at our variables and see some summary statistics
str(datanew) # The str() function shows the structure of your dataset and details the type of variables that it contains
summary(datanew) # The summary() function provides for each variable in your dataset the minimum, mean, maximum and quartiles

linreg=lm(Rating~.,data=dataold) # Estimate a linear regression model of Rating as a function of everything else.
predcreditscore = predict(linreg,newdata=datanew,type="response") 

cor(linreg$fitted.values,dataold$Rating) # Computes the correlation between the fitted values and the actual ones
plot(dataold$Rating,linreg$fitted.values) # Plot the fitted values vs. the actual ones
cor(predcreditscore,datanew$Rating) # Computes the correlation between the fitted values and the actual ones
plot(datanew$Rating,predcreditscore) # Plot the fitted values vs. the actual ones


############################################################
####        EXAMPLE N°2 - HR ANALYTICS 3                ####
############################################################
rm(list=ls(all=TRUE))

# Let's load our dataset
dataold=read.table('DATA_3.02_HR2.csv', header = T,sep=',') # The function read.table enables us to read flat files such as .csv files
datanew=read.table('DATA_4.02_HR3.csv', header = T,sep=',') # The new dataset on which we want to make the prediction

str(datanew) # The str() function shows the structure of your dataset and details the type of variables that it contains
summary(datanew) # The summary() function provides for each variable in your dataset the minimum, mean, maximum and quartiles

logreg = glm(left ~ ., family=binomial(logit), data=dataold) # Estimate the drivers of attrition
probaToLeave=predict(logreg,newdata=datanew,type="response") # Make predictions on the out-of-sample data

predattrition = data.frame(probaToLeave) # Structure the prediction output in a table
View(predattrition) # View the predattrition dataframe

predattrition$performance=datanew$LPE # Add a column to the predattrition dataframe containing the performance
View(predattrition) # View the predattrition dataframe

plot(predattrition$probaToLeave,predattrition$performance)
abline(v= 0.39, col = "red")
abline(h= 0.47, col = "red")
text(0.27, 0.76, col = "red", "As Usual")
text(0.7, 0.76, col = "red", "To be retained")
text(0.4, 0.4, col = "red", "Shape up or ship out")

predattrition$priority=predattrition$performance*predattrition$probaToLeave
View(predattrition)

orderpredattrition=predattrition[order(predattrition$priority,decreasing = TRUE),]
View(orderpredattrition)

topPerfs <- predattrition[predattrition$performance > 0.9,]
topPerfs[topPerfs$probaToLeave == min(topPerfs$probaToLeave),]

###########################################################
####        EXAMPLE N°3 - PREDICTIVE MAINTENANCE       ####
###########################################################

# Set your directory to the folder where you have downloaded the Predictive Maintenance dataset 

# to clean up the memory of your current R session run the following line
rm(list=ls(all=TRUE))

# Let's load the data
dat=read.table('DATA_4.03_MNT.csv',sep=',',header=TRUE)

str(dat) # The str() function shows the structure of your dataset and details the type of variables that it contains
summary(dat) # The summary() function provides for each variable in your dataset the minimum, mean, maximum and quartiles


linregmodel = lm(lifetime~.-broken,data=dat)  # Build a linear regression model
summary(linregmodel) # The summary() function shows the output of your model

install.packages("survival") # Install the survival package to your computer
library(survival) # Load the survival package

dependantvars = Surv(dat$lifetime, dat$broken) # choose the dependant variables to be used in the survival regression model with the Surv() function
survreg = survreg(dependantvars~pressureInd+moistureInd+temperatureInd+team+provider, dist="gaussian",data=dat) # Create your survival regression model
summary(survreg)  # The summary() function shows the output of your model

Ebreak=predict(survreg, newdata=dat, type="quantile", p=.5) # Make predictions based on the model. Here we estimate the median lifetime as the expected moment of "death"

Forecast=data.frame(Ebreak) # Create a dataframe to store the ouput of Ebreak

Forecast$lifetime=dat$lifetime  # Add a column in the Forecast dataframe indicating the lifetime of the piece
Forecast$broken=dat$broken # Add a column in the Forecast dataframe indicating whether or not the piece is broken
Forecast$RemainingLT=Forecast$Ebreak-dat$lifetime # Computed Expected Remaining Lifetime

View(Forecast) # View the complete Forecast dataframe

Forecast=Forecast[order(Forecast$RemainingLT),] # Order the elements by Expected Remaining Lifetime
ActionsPriority=Forecast[Forecast$broken==0,] # And keep only those who are not broken yet
View(ActionsPriority) # View the output and take actions!

survreg = survreg(dependantvars~pressureInd+moistureInd+temperatureInd, dist="gaussian",data=dat) # Create your survival regression model
summary(survreg)
###########################################################
####        EXAMPLE N°4 - SEASONAL SALES OF CHOCOLATES ####
###########################################################

# Set your directory to the folder where you have downloaded the Chocolate dataset 

# to clean up the memory of your current R session run the following line
rm(list=ls())

# Let's load our dataset and call it data
dat=read.table('DATA_4.04_CHOC.csv',sep=',',header=TRUE) # The function read.table enables us to read flat files such as .csv files

# Now let's have a look at our variables and see some summary statistics
str(dat) # The str() function shows the structure of your dataset and details the type of variables that it contains
summary(dat$sales) # The summary() function provides for each variable in your dataset the minimum, mean, maximum and quartiles

plot(dat$time
     , dat$sales
     , main="Chocolate sales over time"
     , xlab="Time (in month)"
     , ylab="Monthly sales"
     , ylim=c(0, max(dat$sales * 1.2))
     , type='l')

# this does not make sense for a time series
regres=lm(sales~month,data=dat) # Build a linear regression model
summary(regres)

# Boxplots:
plot(dat$month
     , dat$sales
     , main="Chocolate sales by month"
     , xlab="Month"
     , ylab="Monthly sales"
     , ylim=c(0,max(dat$sales*1.2)))

# Recovery thanks to the model:
plot(dat$time
     , dat$sales
     , main="Chocolate sales over time"
     , xlab="Time (in month)"
     , ylab="Monthly sales"
     , ylim=c(0,max(dat$sales) * 1.2)
     , type='l')

lines(dat$time
      , regres$fitted.values
      , type = 'l'
      , col = 'blue'
      , lty = 2)
legend("topleft"
       , c("Actual sales"
           , "Sales by the model")
       , lty = c(1, 2)
       , col = c('black', 'blue'))