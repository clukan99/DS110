
library (tidyverse)
data ("iris")
View(iris)

summary (iris[,1:4])
table (iris$Species)


#let the data speak to you
ggplot (data=iris, aes(y=Sepal.Length)) +
  geom_boxplot(aes(fill=Species))

ggplot (data=iris, aes(x=Sepal.Length, y=Sepal.Width)) +
  geom_point(aes(color=Species))

ggplot (data=iris, aes(x=Petal.Length, y=Petal.Width)) +
  geom_point(aes(color=Species))

ggplot (data=iris, aes(x=Sepal.Length)) +
  geom_density(aes(fill=Species), alpha =.4)

ggplot (data=iris, aes(x=Sepal.Length)) +
  geom_histogram(bin=20, aes(fill=Species)) +
  facet_wrap(~Species, nrow = 3)

#ggplot on overdrive
library(GGally)
ggpairs(iris, aes(colour = Species))


iris_longer <-
  iris %>% pivot_longer (Sepal.Length:Petal.Width, names_to="Measurement", 
                         values_to = "Value")

ggplot(data=iris_longer, mapping=aes(x=Measurement, y=Value)) +
    geom_boxplot(aes(fill=Species))

# Shall we learn about LDA Now?

# Split the data into training (80%) and test set (20%)
set.seed(123)
n <- nrow(iris)
rnd <- sample(1:n, n*.8, replace=FALSE) #create a sequence of row numbers
rnd <- sort(rnd)
train_iris <- iris[rnd,] #specifies the rows in rnd and take all variables
test_iris <- iris[-rnd,]

library(MASS)
iris_model <- lda(Species~., data = train_iris)
iris_model

iris_predict <- predict (iris_model, newdata = test_iris)
str(iris_predict)
iris_predict$class

# do a scatterplot of the cases on the 2 new dimensions
plot(iris_model)

#to get a ggplot version have to create new dataframe
iris_lda <- cbind(test_iris,iris_predict$x) #predict model has LDA scores in X
ggplot (data=iris_lda, aes(x=LD1, y=LD2))+
  geom_point(aes(color=Species))

t <- table ("Predict" = iris_predict$class, "Truth" = test_iris$Species)
library (caret)
confusionMatrix(t) 

par(pty="s") # forces plot to be square as ROC plots should be!

library(pROC)
y <- as.numeric(test_iris$Species)
x <- as.numeric(iris_predict$class)
iris_roc = multiclass.roc(y ~ x, plot = TRUE, print.auc = TRUE,
              auc.polygon=FALSE, legacy.axes=TRUE, direction = "<")
lines (x=c(.5,1), y=c(.5,1), col="green") #isobias line

par(pty="m") #back to normal (maximum size)l

