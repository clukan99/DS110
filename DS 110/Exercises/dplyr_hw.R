# dplyr Homework

setwd("~/OneDrive - Butler University/DS110/class/data")

library(dplyr)
vehicles <-
  read.csv(file = 'vehicles.csv', header = TRUE) #default

str(vehicles)
View(vehicles)

#1 
vehicles %>%
  filter(year > 2007, drive == "Rear-Wheel Drive") %>%
  summarise(AvgCity = mean(citympg, na.rm = TRUE), 
            SdCity = sd(citympg,  na.rm = TRUE),
            n = n() )
#2
fuel_eff <- vehicles %>%
  filter (cylinders == 4 & highwaympg >= 40) %>%
  select (year,make,model,highwaympg,co2emissions)

#3
fuel_eff %>%
  arrange(desc(highwaympg)) %>%
  head(10)

#4
vehicles %>%
  group_by(transmissiontype) %>%
  summarize(avgCity = mean(citympg, na.rm = TRUE))

#5
vehicles %>%
  filter(year == 2018) %>%
  mutate (mpgDif = highwaympg - citympg) %>%
  summarize(avgDif = mean(mpgDif, na.rm = TRUE))

