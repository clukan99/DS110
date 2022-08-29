
setwd("~/OneDrive - Butler University/DS110/class/data")

adData <-
  read.csv(file = 'Advertising.csv', header = TRUE) #default
str(adData)

# detach (adData)

summary(adData[,2:5]) # key descriptive stats and relevant data
boxplot (adData[,2:4])
boxplot(adData[,"sales"])

# the full model we have seen in the Lecture Slide
adModel <- lm(sales ~ radio, data = adData)
# abModel <- glm(sales ~ radio, family = gaussian, data = adData)
summary(adModel) # model summary
anova(adModel) # ANOVA table and significance test

adModel$coefficients #components from adModel

# Measures of Fit
RSE <- sqrt(sum((adModel$fitted.values- adData$sales)^2) / adModel$df.residual)
RSE
MSE <- sum((adModel$fitted.values- adData$sales)^2)/nrow(adData)
MSE # Mean square error

# Let's draw a scatterplot and regression line
plot(adData$radio,adData$sales)
abline(adModel, col="red") #Actual best fit line


# Test of linearity and bivariate normality assumptions
par(mfrow=c(2,2)) 
plot(adModel)
par(mfrow=c(1,1))

#Back to Lecture slides about test and train data sets

# Let's build a test and train data set and use for training and testings
n <- nrow(adData)
set.seed(110)
rnd <- sample(1:n, n*.8, replace=FALSE) #create a sequence of row numbers
rnd <- sort(rnd)
adTrain <- adData[rnd,] #specifies the rows in rnd and take all variables
adTest <- adData[-rnd,]

# Let's do some Machine Learning!
adModel_Train <- lm(sales ~ radio, data = adTrain)
summary(adModel_Train)
# anova(adMode_Trainl)
TrainSlope <-adModel_Train$coefficients[2]
TrainInt <- adModel_Train$coefficients[1]
TrainRSE <- sqrt(sum((adModel_Train$fitted.values- adTrain$sales)^2) / adModel_Train$df.residual)

#now that we have the model - let's see how well it works on new data
adTest_predict <- predict(object = adModel_Train, newdata = adTest) # predict cases based on Train data
adTest_resid <- resid(object = adModel_Train, newdata = adTest) # residual scores 
sqrt(sum(adTest_resid^2)/38) # Residual standard error based on Train - Note: Why 38?
var(adTest_predict) / var(adTest$sales) # R2 based on Train slope - bad measure as too much variance based on largetr data

#now let's compare that to what we would get best-case by recomputing regression on adTest
adModel_Test <- lm(sales ~ radio, data = adTest)
plot(adTest$radio,adTest$sales)
abline(adModel_Test, col="red") #Actual best fit line
abline(TrainInt,TrainSlope, col="blue", lty=2) #line based on Train data
summary (adModel_Test)

# Back to  Regression Lecture Sides
