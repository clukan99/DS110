# KNN Example

setwd("~/DS110/data")
cancer <- read.csv("breast-cancer-wisconsin.csv")
# this version already normalized - all between 1-10

View(cancer)
# fixup class as factor
# cancer$class <- factor(cancer$class, levels=c(2,4), labels=c("Benign","Malignant"))
table(cancer$class)

#eliminate the id column
cancer <- cancer[,2:11]

#we have seen this trick before
library(GGally)
ggpairs(cancer, aes(colour = class))


#Skip this in class
#we have seen this trick before
cancer_longer <-
  cancer %>% pivot_longer (-class, names_to="Measurement", 
                         values_to = "Value")
ggplot(data=cancer_longer, mapping=aes(x=Measurement, y=Value)) +
  geom_boxplot(aes(fill=class))


#Let's do some KNN now

library(class) 

set.seed(1234)
n <- nrow(cancer)
rnd <- sample(1:n, n*.8, replace=FALSE) #create a sequence of row numbers
rnd <- sort(rnd)
train_cancer <- cancer[rnd,] #specifies the rows in rnd and take all variables
test_cancer <- cancer[-rnd,]

knn_predict <- knn(train = train_cancer, 
                   test = test_cancer,
                   cl = train_cancer$class,
                   prob = TRUE,
                   k = 10)

table ("Truth" = test_cancer$class, "Predict" = knn_predict)
t <- table ("Truth" = test_cancer$class, "Predict" = knn_predict)

#accuracy
sum(diag(t)) / nrow(test_cancer) # Accuraccy


library (caret)
confusionMatrix(t, positive = "4")  #which value is consider a "positive" case


library(pROC)
par(pty="s") # forces plot to be square as ROC plots should be!
y <- as.numeric(test_cancer$class)
x <- as.numeric(knn_predict)
knn_roc = roc(y ~ x, plot = TRUE, print.auc = TRUE, 
              auc.polygon=FALSE, legacy.axes=TRUE)
lines (x=c(.5,1), y=c(.5,1), col="green") #isobias line

par(pty="m") # forces plot to be max again



setwd("~/OneDrive - Butler University/DS110/class/data")
cancer <- read.csv("breast-cancer-wisconsin.csv")
# this version already normalized - all between 1-10

View(cancer)
#fixup class as factor
cancer$class <- factor(cancer$class, levels=c(2,4), labels=c("Benign","Malignant"))
table(cancer$class)

#eliminate the id column
cancer <- cancer[,2:11]

#we have seen this trick before
library(GGally)
ggpairs(cancer, aes(colour = class))


#we have seen this trick before
cancer_longer <-
  cancer %>% pivot_longer (-class, names_to="Measurement", 
                         values_to = "Value")
ggplot(data=cancer_longer, mapping=aes(x=Measurement, y=Value)) +
  geom_boxplot(aes(fill=class))


#Let's do some KNN now

library(class)

set.seed(1234)
n <- nrow(cancer)
rnd <- sample(1:n, n*.8, replace=FALSE) #create a sequence of row numbers
rnd <- sort(rnd)
train_cancer <- cancer[rnd,] #specifies the rows in rnd and take all variables
test_cancer <- cancer[-rnd,]

knn_predict <- knn(train = train_cancer, 
                   test = test_cancer,
                   cl = train_cancer$class,
                   k = 10)
table ("Truth" = test_cancer$class, "Predict" = knn_predict)
t <- table ("Truth" = test_cancer$class, "Predict" = knn_predict)

#accuracy
acc <- 0
for (i in 1:ncol(t)) {
  acc <- acc + t[i,i] }
acc/nrow(test_cancer)

library (caret)
confusionMatrix(t, positive = "4")  #which value is consider a "positive" case

par(pty="s") # forces plot to be square as ROC plots should be!

library(pROC)
y <- as.numeric(test_cancer$class)
x <- as.numeric(knn_predict)
knn_roc = roc(y ~ x, plot = TRUE, print.auc = TRUE, 
              auc.polygon=FALSE, legacy.axes=TRUE)
lines (x=c(.5,1), y=c(.5,1), col="green") #isobias line

par(pty="m") # forces plot to be max again


