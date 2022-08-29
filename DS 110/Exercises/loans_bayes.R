# Naive Bayes Classifier

#setwd("~/OneDrive - Butler University/DS110/class/data")
# setwd ("~/DS110/data")
loans <-  read.csv("loans.csv", header = TRUE)
loans$Approve <- factor(loans$Approve, levels=c(0,1),  labels=c("No","Yes"))

# should factor emplyed as well
loans$employed <- factor(loans$employed, levels=c(0,1),  labels=c("No","Yes"))

head(loans,10)
loans <- loans[,2:5] #drop id column

library (ggplot2)
library(GGally)
ggpairs(loans, aes(colour = Approve))

# Split the data into training (80%) and test set (20%)
set.seed(123)
n <- nrow(loans)
rnd <- sample(1:n, n*.8, replace=FALSE) #create a sequence of row numbers
rnd <- sort(rnd)
train_loans <- loans[rnd,] #specifies the rows in rnd and take all variables
test_loans <- loans[-rnd,]


#Let's do a Naive Bayes Classification
library (naivebayes)

loan_model <-naive_bayes(Approve ~ FICO + Income + employed, data=train_loans)
print(loan_model)
summary(loan_model)
plot(loan_model)

# Let's' look at the predictions
loan_predict <- predict (loan_model, type="prob") #get the no and yes probabilities
head(loan_predict,10)
loan_predict <- predict(loan_model, type="class") # get the classification
(t <- table ("predict" = loan_predict, "truth" = train_loans$Approve))
sum(diag(t)) / nrow(train_loans) # Accuraccy

# Let's project onto the test model
loan_predict <- predict(loan_model, type="class", newdata = test_loans) # get the classification
(t <- table ("predict" = loan_predict, "truth" = test_loans$Approve))
sum(diag(t)) / nrow(test_loans) # Accuraccy



library (caret)
confusionMatrix(loan_predict, reference=test_loans$Approve, positive = "Yes")  #which value is consider a "positive" case

par(pty="s")
library (pROC)
y <- as.numeric(test_loans$Approve)
x <- as.numeric(loan_predict)
loan_roc = roc(y ~ x, plot = TRUE, print.auc = TRUE, 
               auc.polygon=FALSE, legacy.axes=TRUE)
lines (x=c(.5,1), y=c(.5,1), col="green") #isobias line
par(pty="m") # forces plot to be max again

