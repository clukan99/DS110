#----------------------------------------------------------------
#' #3. Base R Visualizations
#----------------------------------------------------------------

vehicles <-
  read.csv(file = '../DATA/vehicles.csv', header = TRUE) #default
attach (vehicles)

#Basic Histogram
?hist
hist(co2emissions, freq=TRUE, main = "EPA Emissions")

#Basic Histogram - with superimposed Normal Distribution
hist(co2emissions, freq=FALSE, main = "EPA Emmissions")
lines(seq(0, 1200), dnorm(seq(0, 1200),
                          mean(co2emissions), sd(co2emissions)), col="red", lwd=2)

#density plots
d <- density(co2emissions)
plot (d, main = "EPA Emissions - Density")
lines(seq(0, 1200), dnorm(seq(0, 1200),
                     mean(co2emissions), sd(co2emissions)), col="red", lwd=2)
###linens(length, normm of length, mean, standard dev)
###Denstiy plot is good for interval scale

polygon(d, col="gray") # fill the density plot

#bar graph - counts of categorical variables
####Table gets the counts of categorical variables
counts <- table(class)
barplot (counts, main = "Car Class")

# pie chart
pie (counts, main = "Car Class")

#stacked bar graph - matrix categorical variables
counts <- table(cylinders,class)
barplot (counts, legend = rownames(counts), beside=FALSE)

# What does the boxplot of emissions by class type tell us?
boxplot(co2emissions)
boxplot(co2emissions ~ class)
#add a lines (options: col - color, lwd - thickness, lty = styple)
abline(h = mean(co2emissions), col="red", lwd=2, lty=3)

# Create a scatterplot (with regression line) of emissions by highwaympg
plot(highwaympg,co2emissions, pch = 4)
abline(lm(co2emissions ~ highwaympg), col = "red", lwd=4)
abline(v= mean(highwaympg, na.rm=TRUE), col = "blue")
abline(h= mean(co2emissions))


library("car") #Functions to Accompany J. Fox and S. Weisberg, An R Companion to Applied Regression, Third Edition, Sage, 2019.
scatterplot(highwaympg,co2emissions)
# returns: the points
# the regression line (solid line)
# the smoothed conditional spread (in shade)
# the non-parametric regression smooth (dotted line)








