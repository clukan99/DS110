#load libraries into R session
library(tidyverse)

getOption("scipen") # scientific notation
options(scipen = 10L)
#options(scipen = 0L)

setwd("~/OneDrive - Butler University/DS110/class/data")
setwd("~/DS110/data")

ames <- read.csv ("./DATA/ames.csv")


ames<- ames[ames$Gr.Liv.Area < 4000,]

#Missing data warnings - Make it stop!
options(warn = -1)
options(warn = 0)

# Let's start with the basic Histogram

ggplot(data=ames, mapping=aes(x=SalePrice)) +
  geom_histogram(bins=30) + xlim(0,650000)

?geom_histogram

# let's make it look nice
ggplot(data=ames, mapping=aes(x=SalePrice)) +
  geom_histogram(bins=30, fill="steelblue4")

#let's add a layer and assign it to a variable
p <- ggplot(data=ames, mapping=aes(x=SalePrice)) +
  geom_histogram(bins=30, na.rm = TRUE, fill="steelblue4")
p
p <- p + geom_freqpoly(bins = 30, color="red4") 
p

#bar plots - when x is a category
ggplot(data=ames, mapping=aes(x=Foundation)) + 
  geom_bar() 

#density plot - smoothed histograms
ggplot(data=ames, mapping=aes(x=SalePrice)) +
  geom_density() 

# because densities are areas, they can be filled.
ggplot(data=ames, mapping=aes(x=SalePrice)) +
  geom_density(fill="steelblue4", alpha=.5) #alpha 0-1

# and densities can be stacked - notice the difference!
ggplot(data=ames, mapping=aes(x=SalePrice)) +
  geom_density(aes(fill=factor(Full.Bath)), alpha=.3) #alpha 0-1
###Aesthetic where fill is a factor of full bath. The fill is a different density plot of theh  full baths


#box and whiskers plots 
ggplot(data=ames, mapping=aes(x=factor(Exterior.1st), y=SalePrice)) + 
  geom_boxplot(fill="blue", alpha=.2) 

ggplot(data=ames, mapping=aes(x=factor(Neighborhood), y=SalePrice)) + 
  geom_boxplot(fill="blue", alpha=.2) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) #google

#lets add an aesthetic and flip
ggplot(data=ames, mapping=aes(x=SalePrice, y=Land.Contour)) + 
  geom_boxplot(aes(color=Lot.Shape))
  
# Scatter plots base of the graph
ggplot(data = ames, mapping=aes(x=Gr.Liv.Area, y=SalePrice))

# add a layer
ggplot(data = ames, mapping=aes(x=Gr.Liv.Area, y=SalePrice)) +
  geom_point()


#add another layer - say a regression line 
ggplot(data = ames, mapping=aes(x=Gr.Liv.Area, y=SalePrice)) +
  geom_point() +
  geom_smooth(method="lm", formula = y ~x, se=FALSE, color="red")

#and add reference lines for the 2 X 2!
ggplot(data = ames, mapping=aes(x=Gr.Liv.Area, y=SalePrice)) +
  geom_point() +
  geom_smooth(method="lm", formula = y ~x, se=FALSE, color="red") +
  geom_vline(xintercept = mean(ames$Gr.Liv.Area))+
  geom_hline(yintercept = mean(ames$SalePrice))

#let's fit a curve to the data
ggplot(data = ames, mapping=aes(x=Lot.Area, y=SalePrice)) +
  geom_point() +
  geom_smooth(method="lm", formula = y ~x, se=FALSE, color="red") +
  geom_smooth(method="loess", formula = y ~x, se=TRUE, level=.99,
              color="blue", linetype = "dashed")

#okay - that looks stupid, so let's fix it using dplyr
ames %>%
  filter (Lot.Area < 25000) %>%
  ggplot(mapping=aes(x=Lot.Area, y=SalePrice)) +
  geom_point() +
  geom_smooth(method="lm", formula = y ~x, se=FALSE, color="red") +
  geom_smooth(method="loess", formula = y ~x, se=TRUE, level=.99,
              color="blue", linetype = "dashed")

#let's add  an aesthetics to a scatter plot.
#compare the following
ggplot(data = ames, mapping=aes(x=Gr.Liv.Area, y=SalePrice)) +
  geom_point(color = "blue", )

ggplot(data = ames, mapping=aes(x=Gr.Liv.Area, y=SalePrice)) +
  geom_point(aes(color = Overall.Cond))

# back to slides for the "wrap up"

#Titles
p <-
ggplot(data=ames, mapping=aes(x=SalePrice)) +
  geom_density(fill="steelblue4", alpha=.5)  + 
  labs(x = "Sale Price", y = "Density",
       title = "Sales in Ames Iowa",
       subtitle = "Public Data Set",
       caption = "Data: Ames Realty Group",
       tag = "Fig. 1")
p

#legend - automatic but many settings
ggplot(data = ames, mapping=aes(x=Gr.Liv.Area, y=SalePrice, color=House.Style)) +
  geom_point() +
  theme(legend.position = c(.15,.65))

#themes
ggplot(data = ames, mapping=aes(x=Gr.Liv.Area, y=SalePrice, color=House.Style)) +
  geom_point() +
  theme_linedraw() +
  theme(legend.position = "top")

#facets
ggplot(data = ames, mapping=aes(x=Gr.Liv.Area, y=SalePrice)) +
  geom_point() +
  facet_wrap(~ Sale.Condition)



