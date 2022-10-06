
setwd("~/OneDrive - Butler University/DS110/class/data")

#1
ames <- read.csv("ames.csv", header = TRUE)
ames <- read.csv("../DATA/ames.csv")


#2
amesRed <- ames[ames$Gr.Liv.Area < 4000,c("Order","Gr.Liv.Area","Lot.Area",
                                          "Overall.Cond","SalePrice")]

overall_model <- lm(SalePrice ~ Gr.Liv.Area + Lot.Area + Overall.Cond, 
           data = amesRed)
summary (overall_model)

#3
n <- nrow(amesRed)
set.seed(110)
rnd <- sample(1:n, n*.8, replace=FALSE) #create a sequence of row numbers
rnd <- sort(rnd)
amesTrain <- amesRed[rnd,] #specifies the rows in rnd and take all variables
amesTest <- amesRed[-rnd,]

#4
amesTrain_model <- lm(SalePrice ~ Gr.Liv.Area + Lot.Area + Overall.Cond, data = amesTrain)
summary(amesTrain_model)

#5
amesTest_predict <- predict(object = amesTrain_model, newdata = amesTest) 
amesTest_resid <- resid(object = amesTrain_model, newdata = amesTest)


#6
sqrt(sum(amesTest_resid^2)/581) 
var(amesTest_predict) / var(amesTest$SalePrice) 

sqrt(sum(amesTest_resid^2)/581)  <= 54130 # if true not a bad model

