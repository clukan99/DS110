
setwd ("~/DS110/data")

daily <-
  read.csv(file = 'daily.csv', header = TRUE) #default

attach(daily)
search()

#1
boxplot(n_riders ~ day_of_week)
day_of_week <- factor(day_of_week, levels=c("Sun","Mon","Tues","Wed","Thurs","Fri","Sat"))
boxplot(n_riders ~ day_of_week)

#2
boxplot(n_riders ~ month)
month <- ordered(month, levels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
boxplot(n_riders ~ month)

#Question where are those low ride dates?
low_Rides <- daily[n_riders < 5,]
low_Rides

#3
hist(n_riders, freq = TRUE)
plot(density (n_riders))
lines(0:500, dnorm(0:500,mean(n_riders),sd(n_riders)), col="red")

#4
plot(n_rides,n_riders, pch = 1)
abline(lm(n_riders ~ n_rides), col = "red", lwd=2)
abline(v = mean(n_rides))
abline (h = mean(n_riders))

detach(daily)
