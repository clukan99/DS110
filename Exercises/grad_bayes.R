# Naive Bayes Classifier

setwd("~/OneDrive - Butler University/DS110/class/data")
grad <-  read.csv("grad_admit.csv", header = TRUE)
grad$admit <- factor(grad$admit, levels=c(0,1),  labels=c("No","Yes"))
head(grad,10)
# grad$rank <-  factor(grad$rank, levels=c(4,3,2,1))

library (ggplot2)
library(GGally)
ggpairs(grad, aes(colour = admit))


#Let's do a Naive Bayes Classification
library (naivebayes)

grad_model <-naive_bayes(admit ~  gre + gpa + rank, data=grad)
print(grad_model)
summary(grad_model)
plot(grad_model)

# Let's' look at the predictions
grad_predict <- predict (grad_model, type="prob") #get the no and yes probabilities
head(grad_predict,10)
grad_predict <- predict(grad_model, type="class") # get the classification
(t <- table ("predict" = grad_predict, "truth" = grad$admit))
sum(diag(t)) / nrow(grad) # Accuraccy


#project model onto next year's data 
grad21 <- read.csv("grad_admit3.csv", header = TRUE)
grad21$admit <- factor(grad21$admit, levels=c(0,1),  labels=c("No","Yes"))

grad21_predict <- predict (object=grad_model, newdata=grad21, type="class") #get classification

library (caret)
confusionMatrix(grad21_predict, reference=grad21$admit, positive = "Yes")  #which value is consider a "positive" case

par(pty="s")
library (pROC)
y <- as.numeric(grad21$admit)
x <- as.numeric(grad21_predict)
grad_roc = roc(y ~ x, plot = TRUE, print.auc = TRUE, 
               auc.polygon=FALSE, legacy.axes=TRUE)
lines (x=c(.5,1), y=c(.5,1), col="green") #isobias line
par(pty="m") # forces plot to be max again

