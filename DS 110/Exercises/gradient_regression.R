# Gradient Descent Linear Regression Example

# A basic gradient decent algorithm
update_w_and_b <- function(x, y, w, b, alpha){ 
  N = length(x)
  dr_dw <- -2 * x * (y - (w * x + b))  # slope - Use of the chain rule for derivatives
  dr_db <- -2 * (y - (w * x + b)) # intercept, again based each val
  # now update w and b based on partial derivative calculations
  w <- w - sum(dr_dw)/N * alpha
  b <- b - sum(dr_db)/N * alpha
  RSE <- sqrt(sum((y - (w*x+b))^2)/(N-2)) #added for reporting usage only
  MSE <- sum((y - (w*x+b))^2)/N # overall 
  results <- list(w,b,RSE,MSE)
  return (results)
}

setwd("~/OneDrive - Butler University/DS110/class/Data")
data <- read.csv("advertising.csv")
x <- data$radio
y <- data$sales
N <- length(x)

#Let's see the "right answer"
lnRegress <- lm (y ~ x)
plot (x,y, xlab="Ads", ylab = "Sales")
abline(reg = lnRegress, col = 'red', lty=1, lwd=2)
w_T <- lnRegress$coefficients[2] #slope
b_T <- lnRegress$coefficients[1] #intercept
RSE_T <- sqrt(sum(lnRegress$residuals^2)/(N-2))
MSE_T <- sum(lnRegress$residuals^2)/N
summary(lnRegress)
cbind(w_T,b_T,RSE_T,MSE_T)


#set initial params and Let's "learn" the values from the data
reportCnt = 1
epochs = 1
w <- 1
b <- 5
alpha <- .001

# start with a clean plot
plot (x,y)

#plot the initial estimate
abline(b,w, col=3, lty=2)
RSE = sqrt(sum((y - (w*x+b))^2)/(N-2))
MSE <- sum((y - (w*x+b))^2)/N
cbind(w,b,RSE,MSE)

# Now let's start the iterations
for (e in 1:1) {
  results <- update_w_and_b(x,y,w,b,alpha)
  epochs <-epochs + 1
  w <- as.numeric(results[1])
  b <- as.numeric(results[2])
  RSE <- as.numeric(results[3])
  MSE <- as.numeric(results[4])
  if (e %% 1 == 0) {
    print (c(w,b,RSE,MSE))
    abline(b,w, col=reportCnt, lty=2)
    reportCnt <- reportCnt + 1
  }
}

#Let's re-plot the last values and the "right" values and line
print (c(w,b,RSE,MSE))
print (c(w_T,b_T,RSE_T,MSE_T))
abline(reg = lnRegress, col = 'red', lwd=2)




#  if (round(MSE,digits = 3) == round(MSE_T,digits = 3)) {break}
