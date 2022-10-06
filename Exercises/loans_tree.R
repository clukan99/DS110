#Decision Tree plot Example

options(scipen=100000)
loans <- read.csv("loans.csv", header=TRUE)
Approve <- factor(loans$Approve, levels=c(0,1), labels=c("No", "Yes"))
table(Approve)

# Split the data into training (80%) and test set (20%)
set.seed(123)
n <- nrow(loans)
rnd <- sample(1:n, n*.8, replace=FALSE) #create a sequence of row numbers
rnd <- sort(rnd)
train_loans <- loans[rnd,] #specifies the rows in rnd and take all variables
test_loans <- loans[-rnd,]

library (rpart)

loans_model <- rpart(formula = Approve ~ FICO + Income + employed,
                     data = train_loans,
                     method = "class",
                     control = rpart.control(cp = 0.001), #complexity parameter (cp) in rpart is the minimum improvement in the model needed at each node.
                     maxdepth = 9,
                     minsplit = 5)
# The Prediction
loans_predict <- predict(object = loans_model,
                         newdata = test_loans,
                         type = "class")
# Plotting the tree
library(rpart.plot)
prp(loans_model, extra = 1, faclen=0,  nn = T,
    box.col=c("green", "red"))

printcp(loans_model) # complexity parameter (cp) in rpart is the minimum improvement in the model needed at each node.
plotcp(loans_model) # complexity parameter (cp) in rpart is the minimum improvement in the model needed at each node.

# Let's prune!
# gain nothing after cp .03
loans_model <- rpart(formula = Approve ~ FICO + Income + employed,
                     data = train_loans,
                     method = "class",
                     control = rpart.control(cp = 0.03), #complexity parameter (cp) in rpart is the minimum improvement in the model needed at each node.
                     maxdepth = 9,
                     minsplit = 5)

#Redo prediction - based on pruned tree
loans_predict <- predict(object = loans_model,
                         newdata = test_loans,
                         type = "class")

#Redo the tree plot
prp(loans_model, extra = 1, faclen=0,  nn = T,
    box.col=c("green", "red"))


# The Confusion Matrix
library(caret)
confusionMatrix(data = loans_predict,
                reference = factor(test_loans$Approve, levels=c(0,1)),
                positive = "1")

library(pROC)
par(pty="s") # forces plot to be square as ROC plots should be!

y <- as.numeric(test_loans$Approve)
x <- as.numeric(loans_predict)
loans_roc = roc(y ~ x, plot = TRUE, print.auc = TRUE, direction = "<",
                auc.polygon=FALSE, legacy.axes=TRUE, add=FALSE)
lines (x=c(.5,1), y=c(.5,1), col="green") #isobias line

par(pty="m") #back to normal (maximum size)l


