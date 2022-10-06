# http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/118-principal-component-analysis-in-r-prcomp-vs-princomp/
# PCA in R

library (FactoMineR)
library (factoextra)

# setwd("~/OneDrive - Butler University/DS110/class/data")
setwd("~/DS110/data")

cancer <- read.csv("breast-cancer-wisconsin.csv")
cancer$class <- factor(cancer$class, levels=c(2,4), labels=c("Benign","Malignant"))

df <- cancer[,2:10] #remove id and class - keep the IVs

#run a PCA
pca.model <- princomp(x = df, cor=FALSE, scores = TRUE) # cor - center/standardize 

# How many should we retain?
print(pca.model) #summary 
fviz_eig(pca.model) # Scree plot to use that criteria
pca.model$sdev^2 # variance greater than a single variable 1.0
sum(pca.model$sdev[1:3]^2) / sum(pca.model$sdev^2)  # How many PCs to keep to get to 90%

#Scores - create a new data frame with reduced number of predictors
#see and save the eigan values to use for an analysis 
head(pca.model$scores,5) # see the eigen values

print (pca.model$loadings) # see the loadings (after rotation) - does not print loading unber .1


# Now Visualize Summary of Variables - loading of variable on PC
fviz_pca_var(pca.model,
             axes = c(1, 2), # which PCs do you want to visualize
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), # just makes it pretty
             repel = TRUE     # Avoid text overlapping
)

# A "glimps" forward towards clustering - plot cases on the dimentions
fviz_pca_ind(pca.model,
             axes = c(1, 2), # which PC do you want to visualize
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = FALSE     # Avoid text overlapping
)

#And now plot variables and observations together - likely skip
fviz_pca_biplot(pca.model, 
                axes = c(1, 2), # which PC do you want to visualize
                repel = FALSE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color 
)   

####################################################################
# Using the PCs we keep

#First revive old model 

library(randomForest)
cancer_model <-randomForest(class~., data=cancer, mtry=2, ntree=500) 
print(cancer_model)

library (caret)
confusionMatrix(cancer_model$predicted, reference=cancer$class, positive = "Malignant")  #which value is consider a "positive" case


#Let's put factor scores in a new data frame so we can use them.
reducedDF <- data.frame("class"=cancer$class, "PC1" = pca.model$scores[,1]) 
cancer_model2 <-randomForest(class ~ PC1, data=reducedDF, mtry=1, ntree=500) 
print(cancer_model2)
confusionMatrix(cancer_model2$predicted, reference=cancer$class, positive = "Malignant")  #which value is consider a "positive" case

# We loose next to nothing

par(pty="s") # make graph square
library (pROC)
y <- as.numeric(cancer$class)
x <- as.numeric(cancer_model$predicted)
cancer_roc = roc(y ~ x, plot = TRUE, print.auc = TRUE, 
                 auc.polygon=FALSE, legacy.axes=TRUE)
lines (x=c(.5,1), y=c(.5,1), col="green") #isobias line

x <- as.numeric(cancer_model2$predicted) # now add 
cancer_roc2 = roc(y ~ x, plot = TRUE, print.auc = TRUE, 
                 auc.polygon=FALSE, legacy.axes=TRUE, add=TRUE, col="blue")

par(pty="m") # forces plot to be max again

cancer_roc$auc # All variables random forest
cancer_roc2$auc # first PC random forest

