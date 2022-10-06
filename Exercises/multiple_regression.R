#Do it now for Multiple rgression

setwd("~/OneDrive - Butler University/DS110/class/data")
library(car)
adData <-
  read.csv(file = 'Advertising.csv', header = TRUE) #default
# str(adData)
summary(adData[,2:5]) # key descriptive stats and relevant data

# Let's build a test and train data set and use for training and testings
n <- nrow(adData)
set.seed(110)
rnd <- sample(1:n, n*.8, replace=FALSE) #create a sequence of row numbers
rnd <- sort(rnd)
adTrain <- adData[rnd,] #specifies the rows in rnd and take all variables
adTest <- adData[-rnd,]

# build a multiple regression model
adModel_Train <- lm(sales ~ radio + TV + newspaper, data = adTrain)
summary(adModel_Train) ###RSE train = 1.79
anova(adModel_Train)
library(olsrr)
ols_vif_tol(adModel_Train) ###variance inflaition tolerance, tolerance must be >.1 and VIF <10)
#av_plots (adModel_Train)


# Test of linearity and bivariate normality assumptions
##Residual vs Fit result is not perfect but is acceptable
###Normal Q-Q looks great
par(mfrow=c(2,2))
plot(adModel_Train)
par(mfrow=c(1,1))

#now that we have the model - let's see how well it works on new data
adTest_predict <- predict(object = adModel_Train, newdata = adTest) # predict cases based on Train data
#adTest_resid <- resid(object = adModel_Train, newdata = adTest) # residual scores (this command working right, use below)
asTest_resid <- adTest$sales - adTest_predict

###Evaluating model here!
sqrt(sum(asTest_resid^2)/36) # Residual standard error based on Train - Note: Why 36? Becausse there are 4 degrees of freedom. Answer should be 1.59
var(adTest_predict) / var(adTest$sales) # R2 based on Train slope - bad measure as too much variance based on largetr data


###RSE test: 1.79
###RSE train: 1.59

### This is a valid model because of RSE