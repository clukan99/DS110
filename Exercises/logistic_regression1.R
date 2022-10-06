# Using Logistic Regression
# Review of odds ratios - https://youtu.be/ckkiG-SDuV8

library(ggplot2)
setwd("~/OneDrive - Butler University/DS110/class/data")

loans <-
  read.csv(file = 'loans.csv', header = TRUE) #default
Approve <- factor(loans$Approve, levels=c(0,1), labels=c("No", "Yes"))
table(Approve)


model_logit <- glm(Approve ~ FICO,  family=binomial, data = loans)
summary(model_logit)

B0 <- model_logit$coefficients[1] #  "intercept"
B1 <- model_logit$coefficients[2] # "slope" - * is an odds ration

# model.pred <- predict(model_logit, newdata = Test_logit)
# model already contains the predicted values - but handy for test data! 

head(cbind ("FICO"=loans$FICO, "prob" = model_logit$fitted.values), 15)

# Let's plot all the values...
plot (loans$FICO,model_logit$fitted.values, main = "Fitted Values Plot")
abline (h=.5, col="red") #50/50 threshold

#This is the function that predicts scores based on the logit model
#Do you know how to find it?

yourScore <- exp(B0+B1*600) / (1 + exp(B0+B1*600)) #exp - opposite of log
lines(c(600,600),c(0,yourScore), col="blue")
lines(c(0,600),c(yourScore,yourScore), col="blue", lty=2)
yourScore

# reverse function to find the "predLoan boundary" or a 50/50 chance of aproval
dec_boundary<- (-1*B0)/B1
dec_boundary
lines(c(dec_boundary,dec_boundary),c(0,.5), col="red")


#other values using reverse - say a 80% chance
(log(.80/(1-.80)) + (-1*B0))/B1

# Now let's plot the actual Logistic function 
df <- as.data.frame(cbind("Approve" = loans$Approve, 
                          "Prob" = model_logit$fitted.values,
                          "FICO" = loans$FICO))
ggplot (df, aes(x=FICO, y=Approve)) +
  geom_point()+
  geom_smooth(aes(y=Prob), method = "glm",  
              method.args = list(family = "binomial"), se = FALSE) +
  geom_vline(xintercept = dec_boundary)+
  geom_hline(yintercept = .5) 

#Do Slides on SDT, Confusion Matrix and ROC

#Now Let's Evaluate the Model using the actual results

#McFadden's R2 (https://stats.idre.ucla.edu/other/mult-pkg/faq/general/faq-what-are-pseudo-r-squareds/)
#r2 as improvement over just predicting the mean of y in regression
# 

logitR2 <- 1- (model_logit$deviance / model_logit$null.deviance) # not a real R2
logitR2 

criteria <- .50 # we can set this

predLoan <- ifelse (model_logit$fitted.values > criteria, "Yes", "No") #We can set this
CD <- sum(Approve=="Yes" & predLoan == "Yes") 
FA <- sum(Approve=="No" & predLoan == "Yes") 
Miss <- sum(Approve=="Yes" & predLoan == "No") 
CR <- sum(Approve=="No" & predLoan == "No") 

Accuracy <- (CD+CR)/nrow(loans)
Sensitivity <- CD/(CD+Miss)
Specificity <- 1-(FA/(FA+CR))
Prevelence <- (CD+Miss)/nrow(loans)
DetectionRate <- CD/nrow(loans)
Precision <- CD/(CD+FA)

cbind (Accuracy,Sensitivity,Specificity,Prevelence,DetectionRate,Precision) 

t = table (predLoan, "Truth" = Approve)
t

library (caret)
confusionMatrix(t, positive = "Yes")  #"Yes" is you get the loan

#Let's plot the ROC curve and various Thresholds

library(pROC)

par(pty="s") # forces plot to be square as ROC plots should be!

logit_roc = roc(Approve ~ model_logit$fitted.values, 
                plot = TRUE, axis = TRUE, xlim=c(1,0), ylim=c(0,1),
                legacy.axes = TRUE, print.auc = TRUE, grid=TRUE,
                identity=TRUE, add = FALSE)

lines (x=c(.5,1), y=c(.5,1), col="green") #isobias line
points (0.9320652,0.2768987,col="blue", lwd=5) #criteria = .8
points (0.02173913,0.9968354,col="green", lwd=5) #criteria = .2
points (0.3994565,0.846519,col="red", lwd=5) #criteria = .5

par(pty="m") #back to normal (maximum size)l






#### - Code Below - not for calss use you cna ignore

#Add functions to do confusion matrix stuff
get_logistic_pred = function(mod, data, res = "y", pos = 1, neg = 0, cut = 0.5) {
  probs = predict(mod, newdata = data, type = "response")
  ifelse(probs > cut, pos, neg)
}

test_pred_30 = get_logistic_pred(model, data = loans, res = "default", 
                                 pos = "Yes", neg = "No", cut = 0.3)

test_pred_50 = get_logistic_pred(model, data = loans, res = "default", 
                                 pos = "Yes", neg = "No", cut = 0.5)

test_pred_70 = get_logistic_pred(model, data = loans, res = "default", 
                                 pos = "Yes", neg = "No", cut = 0.7)

Approve <- ifelse(loans$Approve == 1, "Yes","No")
test_tab_30 = table(predicted = test_pred_30, actual = Approve)
test_tab_50 = table(predicted = test_pred_50, actual = Approve)
test_tab_70 = table(predicted = test_pred_70, actual = Approve)

library(caret)
library(e1071)
test_con_mat_30 = confusionMatrix(test_tab_30, positive = "Yes")
test_con_mat_50 = confusionMatrix(test_tab_50, positive = "Yes")
test_con_mat_70 = confusionMatrix(test_tab_70, positive = "Yes")

metrics = rbind(
  c(test_con_mat_30$overall["Accuracy"], 
    test_con_mat_30$byClass["Sensitivity"], 
    test_con_mat_30$byClass["Specificity"]),
  c(test_con_mat_50$overall["Accuracy"], 
    test_con_mat_50$byClass["Sensitivity"], 
    test_con_mat_50$byClass["Specificity"]),
  c(test_con_mat_70$overall["Accuracy"], 
    test_con_mat_70$byClass["Sensitivity"], 
    test_con_mat_70$byClass["Specificity"])
)

rownames(metrics) = c("c = 0.30", "c = 0.50", "c = 0.70")
metrics

library(pROC)
test_prob = predict(model, newdata = loans, type = "response")
test_roc = roc(loans$Approve ~ test_prob, plot = TRUE, print.auc = TRUE)

abline (h = 0.9746)
abline (v = .1222826)


# predict gives the predicted value in terms of logits
plot.dat <- data.frame(prob = loans$Approve,
                       FICO = loans$FICO,
                       fit = predict(model, loans))

# convert those logit values to probabilities
plot.dat$fit_prob <- exp(plot.dat$fit)/(1+exp(plot.dat$fit))

library(ggplot2)
ggplot(plot.dat, aes(x=FICO, y=prob)) +
  geom_point() +
  geom_line(aes(x=FICO, y=fit_prob))

