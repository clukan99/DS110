# Basics of R
# Robert Padgett 

#-------------------------------------------------------------------------------
#' 1. Basic Assignment, Arithmetic, Relational Operators, and Logical Operators
#-------------------------------------------------------------------------------

x <- 2 # option x = 2 but not the preferred
y <- 3 # option y = 4

x + y # addition
x - y # subtraction
x * y # multipliation
x / y # division
x ^ y # power - **
y %% x  # - modulus option - x mod y
y %/% x # integer division


# Special math functions
log(x) # natural log
exp (log(x)) 

# Order of operations... Why does this return the "wrong? answer
-4^2+2
# versus
(-4)^2+2

# Let's see the difference between numeric variable and integer variable.
# and why an integer is NOT an integer
y <- 1
yint <- 1L

class(y)
class(yint)


is.integer(yint)
is.numeric(yint)

y==yint
1==1.0

class(yint/y)
class(yint/yint)

class(yint%%yint)
class(y%/%y)


# Compare... The second should be 5, but it's not....????
1/.2
1 %/% .2
.2 * 5

#Scientific Notation
a <- 100000e-5
a

#Relational Operators
x < y # <	Less than
x > y # >	Greater than
x <= y # <=	Less than or equal to
x >= y # >=	Greater than or equal to
x == y # ==	Equal to
x != y # !=	Not equal to
x %in% c(1,2,3) # %in% in the set or vector

#Logical Oporators

# !	Logical NOT
# &	Element-wise logical AND
# |	Element-wise logical OR

# &&	Logical AND - vector 1st element 
# ||	Logical OR -  vector 1st element 

#----------------------------------------------------------------
#' 2. More Data Types in R 
#----------------------------------------------------------------

# Characters
name <- "Bob"

# Matrices 


# Create (atomic) vectors - the central type of data in R
names <- c('Mike', 'Renee', 'Richard', 'Matthew', 'Christopher')
scores <- c(85, 92, 95, 97, 96)

# Check individual component in the vectors.
names[1]
names[2]
scores[3]

# Perform calculations on the entire numeric vector.
mean(scores)
median(scores)
min(scores)
max(scores)
sum(scores)

# Perform calculations every value of a vector - the real power of R
scores <- scores - 10
zscores <- (scores - mean(scores)) / sd(scores)
zscores

# If we attempt to create a vector containing both strings 
# and numbers, R would convert all the elements to strings.
mixed <- c('Mike', 85, 'Renee', 92, 'Richard', 95, 'Matthew', 97, 'Christopher', 96)
mixed

# R does have a "list" type for mixed data types - r is NOT Python!
x <- list("a" = 2.5, "b" = TRUE, "c" = "Cat")
x

# Combine different types of vectors in a data frame.
testResults <- data.frame(names, scores)
testResults

str(testResults)

# Use `$` operator to access a specific column in the data frame.
testResults$scores
# or bracket [ ] notation - Note: rows,columns
testResults[,"scores"]
# or by column position
testResults[,2]
# or attaching  - but not a good test here - note warning
attach (testResults)
scores
names
detach(testResults)

#Advanced referencing of 
testResults$scores[testResults$names == "Mike" | testResults$names =="Richard"]
testResults$scores[testResults$names %in% c("Mike", "Richard")]

# -----------------------------------------------------------
#
# Practice what we have learned so far Homework!
#
# ------------------------------------------------------------


#----------------------------------------------------------------
# More Data Types
#----------------------------------------------------------------
x <- TRUE
y <- 1
z <- 'Hello World'

# Check the data type of the object.
class(x)
class(y)
class(z)

# Factors in R 
#categorical data

# Create a character vector named 'productCategories' and check its data type.
productCategories <- c('fruit', 'vegetable', 'fruit', 'fruit', 'dry goods', 'dry goods', 'vegetable')
class(productCategories)
# Convert the vector from character to factor.
productCategories <- factor(productCategories)
# Check the data type again.
class(productCategories)

#ordering of a factor
table (productCategories)
productCategories <- c('fruit', 'vegetable', 'fruit', 'fruit', 'dry goods', 'dry goods', 'vegetable')
productCategories <- factor(productCategories, levels = c('fruit','vegetable','dry goods'))
table (productCategories)

# Matrices 
myMat <- matrix(data=c(4.3,3.1,8.2,8.2,3.2,0.9,1.6,6.5),nrow=4,ncol=2,byrow=TRUE)
myMat
# All the matrix addition subtraction, multiplication, identity, determinant

#Lists - This is not Python
myList <- list(myMat,productCategories,z)
myList

# Creating functions 

zScore <- function(x) {  # all variable names are local to the function
  Zx <- (x-mean(x)) /sd(x)
  return (Zx)
}

testData <- c(1,2,3,4,5)
zScore(testData)

# checking and converting data types

# Check the length of the objet.
length(x)
length(y)
length(mymat)
dim(mymat)
length(myList)
dim(myList)
length(productCategories)

# Check whether the object is of a specific data type.
is.numeric(x)
is.character(x)
is.integer(x)
is.logical(x)
is.factor(x)
is.list(x)
is.matrix(x)
is.data.frame(x)

## The `is` function also works for vectors.
is.character(names)
is.numeric(names)
is.character(scores)
is.numeric(scores)
is.integer(scores)
is.na(x) #this one works as logical on vectors


#----------------------------------------------------------------
#' 4. Converting Data Types
#----------------------------------------------------------------
# Use `as` function to convert data from one type to another.
# Note: There is no reasonable way to convert a word into an number
as.numeric("1.5")
as.integer("1.5")
as.character(3.14159)
as.integer("apple") # I told you this
as.logical(1)
as.logical(0)
as.logical("true")
as.logical("apple") #you should have anticipated this one

#----------------------------------------------------------------
#' 5. R Packages
#----------------------------------------------------------------
# Install a package.
install.packages("RWeka") # Weka is a collection of machine learning algorithms for data mining tasks 

# Load a package.
library(tidyverse)

# Check the vignette of the package.
vignette(package = 'dplyr')

# Check the programming topic in the vignette of the package.
vignette(package = 'dplyr', topic = 'programming')

#List all the installed packages
library()
#List all loaded packages and attached objects
search()

attach (testResults)
search()
detach(testResults)

