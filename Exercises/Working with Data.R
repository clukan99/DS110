# Managing Data with R

#----------------------------------------------------------------
#' #1. Loading Data - CSV
#----------------------------------------------------------------
# Change your working directory by specifying the path to the desired folder

# Load the data and use the `col_types` argument to specify type of 
# all columns('n' stands for numerical and 'f' stands for factor).
vehicles <-read.csv(file = '../DATA/vehicles.csv', header = TRUE) #default

head(vehicles, 5) # or 
vehicles[1:5,]
tail (vehicles, 5) #or
start <- nrow(vehicles)-4
end <-nrow(vehicles)
vehicles[start:end,]

View (vehicles)
str(vehicles) # See the structure - tidyverse - glimpse

#Referencing the data in a dataframe by name or position
mean(vehicles$highwaympg, na.rm = TRUE) # remove missing data
mean(vehicles$highwaympg[1:200], na.rm = TRUE) # rnow it's a vector

mean(vehicles[,5], na.rm = TRUE) # every row, on column 5 (highwaympg)

# Referencing parts of a dataframe
mean(vehicles[1:200,5], na.rm = TRUE)
smallData <- vehicles[1:10,c(2,4,6)] #so what does this do?

smallData
# Filtering observation from a dataframe
#Get a dataframe of the Nissan vehicles
nissan <- vehicles[vehicles$make=="Nissan" & vehicles$year > 2016,]
table (nissan$model)

#----------------------------------------------------------------
#  Creating Sequences, Random Numbers, and Random Samples
#----------------------------------------------------------------

# creating sequences

x <- 1:10
y <- seq(from = 10, to = 100, by = 10)
z <- seq(100,1,-2)

# creating sudo-random numbers

set.seed (123) # for the seed for the next call of any random function
x <- runif(50) # 50 random numbers between 0 and 1
y <- trunc(runif(1000, min=1,max=7),0) #let's roll some dice
table(y)

z <- rbinom(1000, 1, .8) #1000 binomial values of 1 trial each with probability = .8 
table (z)
z <- rbinom(10000, 16, .5) #Let's flip some coins! - Buzz and Doris approved!
table(z)

x <- rnorm(1000, mean=100, sd=15) #normal distribution
cbind('mean' = mean(x), 'sd' = sd(x)) #column bind
hist(x)

y <- sample (x, 10, replace = FALSE)
z <- sample (x, 800, replace=TRUE) # turns out this is very useful!
cbind('mean' = mean(z), 'sd' = sd(z)) #column bind

#Let' divide vehicles into two set, train and test using sample

#add a column of row numbers to the vehicles dataframe
n = nrow(vehicles)
vehicles <- cbind.data.frame('id' = 1:n,vehicles) #remember this trick, but it's dangerous!  This adds a count column

set.seed(1234)
rnd <- sample(1:n, n*.8, replace=FALSE) #create a sequence of row numbers
rnd <- sort(rnd)

train <- vehicles[rnd,] #specifies the rows in rnd and take all variables.  Applies the filter  of the random  sampling
test <- vehicles[-rnd,]
View(train)
View(test)

#----------------------------------------------------------------
#' #2. Describe the Data
#----------------------------------------------------------------
# Let's try to describe our data.

# Get summary statistics of the data.

summary(vehicles$highwaympg)
attach(vehicles)
summary(class)
# Get all the values and their associated counts for categorical features.
table(class)
table(cylinders,class)

# Get the proportional distribution for all values in the categorical variable.
prop.table(table(class))

#improved summary data

library(psych)
describe (co2emissions)
describeBy(co2emissions, group=class)

library(tidyverse)
glimpse(co2emissionss)

search()
detach(vehicles)
