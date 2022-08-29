# Example Support Vector Machine

library(e1071)

#setwd("~/OneDrive - Butler University/DS110/class/data")
setwd("~/DS110/data")

bake <- read.csv("baking.csv")
bake$Type <- factor(bake$Type)

bake$x <- seq(1,nrow(bake)) # Row number to generate a 2d plot

svmfit = svm(Type ~ Flour, data = bake)
summary(svmfit)
table ("truth" = bake$Type,"predict" = svmfit$fitted)

svmfit = svm(Type ~ Flour + x, data = bake)
summary(svmfit)
table ("truth" = bake$Type,"predict" = svmfit$fitted)
plot(svmfit,bake,x~Flour)



svmfit = svm(Type ~ Butter + x, data = bake,
             kernel = "linear", cost = 1, scale = FALSE,
             coef0 = 3)

summary (svmfit)
plot (svmfit,bake,Butter ~ x, fill = TRUE)


table ("truth" = bake$Type,"predict" = svmfit$fitted)

library(pROC)
y <- as.numeric(bake$Type)
x <- as.numeric(svmfit$fitted)
svm_roc = roc(y ~ x, plot = TRUE, print.auc = TRUE)


##############################################
# Let's work with data we know

loans <-
  read.csv(file = 'loans.csv', header = TRUE) #default


# normalize the data
z <-  function(x) {(x-mean(x))/sd(x)} 

# An alternative way to get the data scaled 
loans_norm <- as.data.frame(lapply(loans[,3:4],z))
loans_norm$employed <- loans$employed
loans_norm$Approve <- loans$Approve

#Note that svm has a scale parameter you can set.

loans_norm$Approve <- factor(loans$Approve, 
                             levels = c(0,1), labels = c("Deny","Approve"))

table(loans_norm$Approve)

#Let's use SVM 
?svm #let's look at the documentation

svmfit = svm(Approve ~ ., data = loans_norm,
             kernel = "linear", cost = 5, scale = FALSE)

summary(svmfit)
(t <- table ("truth" = loans_norm$Approve,"predict" = svmfit$fitted))
sum(diag(t))/nrow(loans) # Accuracy

# Let's come back to using tune
svmTune <- tune(svm, loans_norm[,-4], loans_norm$Approve, kernel="linear", 
                ranges=list(cost=10^(-2:2), gamma=2^(-2:2)))
print(svmTune)


#Let's try a different kernal
svmfit = svm(Approve ~ ., data = loans_norm,
             kernel = "radial", cost = 2, scale = FALSE,
             cross = 10) #cross does built in cross validation
summary(svmfit)
(t <- table ("truth" = loans_norm$Approve,"predict" = svmfit$fitted))
sum(diag(t))/nrow(loans) # Accuracy


library (caret)
confusionMatrix(t, positive = "Approve") 


library(pROC)
par(pty="s")
y <- as.numeric(loans_norm$Approve)
x <- as.numeric(svmfit$fitted)
svm_roc = roc(y ~ x, plot = TRUE, print.auc = TRUE, 
              auc.polygon=FALSE, legacy.axes=TRUE)
lines (x=c(.5,1), y=c(.5,1), col="green") #isobias line
par(pty="m") # forces plot to be max again

