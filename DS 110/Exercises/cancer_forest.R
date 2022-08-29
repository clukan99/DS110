#Random Forest 

#Already normalized.
cancer <- read.csv("breast-cancer-wisconsin.csv", header=TRUE)

#normalizing - this command returns z-scores
#scale - only takes numeric values 
temp <- scale(cancer[,2:10], center=TRUE, scale=TRUE)  #this is equivelent to zscore
cancer.norm <- data.frame(cbind(temp, class = cancer[,"class"]))

#eliminate the id column - if not done already
cancer <- cancer[,2:11]

#fixup class as factor
cancer$class <- factor(cancer$class, levels=c(2,4), labels=c("Benign","Malignant"))
table(cancer$class)



library(randomForest)
set.seed(123)
cancer_model <-randomForest(class~.,data=cancer, ntree=500) 
print(cancer_model)

#find the best mtry
mtry <- tuneRF(cancer[-10],cancer$class, ntreeTry=500, # remove DV
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE,
               doBest=FALSE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)

#redo model with best mtry
set.seed(123)
cancer_model <-randomForest(class~., data=cancer, mtry=2, ntree=500) 
print(cancer_model)

#evaluate importance - decrease in gini impurity
importance(cancer_model)
varImpPlot(cancer_model)

library (caret)
confusionMatrix(cancer_model$predicted, reference=cancer$class, positive = "Malignant")  #which value is consider a "positive" case

par(pty="s")
library (pROC)
y <- as.numeric(cancer$class)
x <- as.numeric(cancer_model$predicted)
cancer_roc = roc(y ~ x, plot = TRUE, print.auc = TRUE, 
              auc.polygon=FALSE, legacy.axes=TRUE)
lines (x=c(.5,1), y=c(.5,1), col="green") #isobias line
par(pty="m") # forces plot to be max again




