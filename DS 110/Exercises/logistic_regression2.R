# Multiple predictors with logistic regression

setwd("~/OneDrive - Butler University/DS110/class/data")

options(scipen = 10L) #reduce printing in scientific notation
#options(scipen = 0L)


loans <-
  read.csv(file = 'loans.csv', header = TRUE) #default
Approve <- factor(loans$Approve, levels=c(0,1), labels=c("No", "Yes"))
table(Approve)

model_logit2 <- glm(Approve ~ FICO + Income + employed,  
                    family=binomial, data = loans)
summary(model_logit2)

#Let's plot each IV separately to view predictions.
#Why dos the "curve" not look like it did before???

plot (loans$FICO, model_logit2$fitted.values, main = "Fitted Values Plot")
abline (h=.5, lty=3) #Plot the predLoan boundary 
abline (v=600, col = "red") # plot your FICO Score

plot (loans$Income,model_logit2$fitted.values, main = "Fitted Values Plot")
abline (h=.5, lty=3) #Plot the predLoan boundary 

plot (loans$employed ,model_logit2$fitted.values, main = "Fitted Values Plot")
abline (h=.5, lty=3) #Plot the predLoan boundary 

table("Employed"=loans$employed, "Approved"= Approve)

#Now Let's Evaluate the Model using the actual results

logitR2 <- 1- (model_logit2$deviance / model_logit2$null.deviance) # not a real R2
logitR2 #McFadden's R2 (https://stats.idre.ucla.edu/other/mult-pkg/faq/general/faq-what-are-pseudo-r-squareds/)

# Let's not do all the confusion table by "hand"
criteria <- .50 # we can set this
predLoan <- ifelse (model_logit2$fitted.values > criteria, "Yes", "No") #We can set this

t = table (predLoan, "Truth" = Approve)
t
library (caret)
confusionMatrix(t, positive = "Yes")  #"Yes" is you get the loan



library(pROC)
par(pty="s") # forces plot to be square as ROC plots should be!
logit_roc = roc(loans$Approve ~ model_logit2$fitted.values, 
                plot = TRUE, axis = TRUE, xlim=c(1,0), ylim=c(0,1),
                legacy.axes = TRUE, print.auc = TRUE, grid=TRUE,
                identity=TRUE, add = FALSE)

lines (x=c(.5,1), y=c(.5,1), col="green") #isobias line
points (0.8804348,0.6297468,col="blue", lwd=5) #criteria = .8
points (0.5054348,0.9984177,col="green", lwd=5) #criteria = .2
points (0.6168478,0.9556962,col="red", lwd=5) #criteria = .5

par(pty="m") #back to normal (maximum size)l
