#install.packages("neuralnet")

library (neuralnet)
?neuralnet

# setwd("~/OneDrive - Butler University/DS110/class/data")
setwd("~/DS110/data")

loans <- read.csv("loans.csv")
temp <- loans[,2]
loans <- as.data.frame(scale(loans[,3:5])) #scale the variables
loans$Approve <- factor(temp, levels = c(0,1), labels = c("No", "Yes")) #put approve back in

## Set up Train and Test Data
table(loans$Approve)
# Split the data into training (80%) and test set (20%)
set.seed(123)
n <- nrow(loans)
rnd <- sample(1:n, n*.8, replace=FALSE) #create a sequence of row numbers
rnd <- sort(rnd)
train_loans <- loans[rnd,] #specifies the rows in rnd and take all variables
test_loans <- loans[-rnd,]

## Run a Basic Neural Net Model - default paramiters
nn.loans <- neuralnet(Approve ~ ., 
                data=train_loans, linear.output = FALSE)
summary (nn.loans)
print (nn.loans$result.matrix)
plot(nn.loans)

nn.predict <- predict(nn.loans,newdata = test_loans) #returns prob for each category
loan_predict <- ifelse(nn.predict[,1] >  nn.predict[,2], 0,1)
loan_predict <- factor(loan_predict, levels = c(0,1), labels = c("No","Yes"))

(t <- table ("predict" = loan_predict, "truth" = test_loans$Approve))
sum(diag(t)) / nrow(test_loans) # Accuraccy

library (caret)
confusionMatrix(factor(loan_predict), reference=test_loans$Approve, positive = "Yes")  #which value is consider a "positive" case

par(pty="s")
library (pROC)
y <- as.numeric(test_loans$Approve)
#x <- as.numeric(loan_predict)
x = nn.predict[,2]
loan_roc = roc(y ~ x, plot = TRUE, print.auc = TRUE, 
               auc.polygon=FALSE, legacy.axes=TRUE)
lines (x=c(.5,1), y=c(.5,1), col="green") #isobias line
par(pty="m") # forces plot to be max again



