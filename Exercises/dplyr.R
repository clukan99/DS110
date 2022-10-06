# -----------------------------------------------
#
# The basics of dplyr
#
# -----------------------------------------------
library(dplyr)
install.packages ("gapminder")
library(gapminder)
?gapminder

View(gapminder)
str(gapminder)

daily <- read.csv("./DATA/daily.csv")
summary(gapminder$lifeExp)

# begining pipes
mean(gapminder$lifeExp)
# use a pipe to send the results of one operation on to the next
gapminder$lifeExp %>% mean()

# That's stupid - of course it is, but..

#Let's start applying the grammar

# Select - columns using  dplyr
gapminder %>%
  select(country, year, lifeExp) %>%
  summary()

#base R
summary(gapminder[,c("country","year","lifeExp")])

# Filter - rows using dplyr
gapminder %>%
  select(country, year, lifeExp) %>%
  filter(year == 2007) %>%
  summary()

#base R
is2007 <- gapminder$year==2007 #create a logical
summary(gapminder[is2007,c("country","year","lifeExp")])

# Sorting in dplyr
gapminder %>%
  select(country, year, lifeExp) %>%
  filter(year == 2007) %>%
  arrange(lifeExp) %>%
  head(10) #top_n(10) 

#Base R
is2007 <- gapminder$year==2007 #create a logical
reduced <- gapminder[is2007,c("country","year","lifeExp")]
reduced <- reduced[order(reduced$lifeExp),]
head(reduced)

# Computing new variables in dplyr
gapminder %>%
  select(country, year, pop, gdpPercap) %>%
  filter(year == 2007) %>%
  mutate(gdp_Bil = pop * gdpPercap / 1e10)  %>%
  arrange(desc(gdp_Bil)) %>%
  head (5)

#Base R
is2007 <- gapminder$year==2007 #create a logical
reduced <- gapminder[is2007,c("country","year","pop","gdpPercap")]
reduced$gdp_Bil <- reduced$pop * reduced$gdpPercap / 1e10
reduced <- reduced[order(-reduced$gdp_Bil),]
head(reduced)

# Summary functions
gapminder %>%
  filter(year == 2007 & continent == "Europe") %>%
  summarize(avgLifeExp = mean(lifeExp))

# Base R
myFilter <- (gapminder$year == 2007 & gapminder$continent == "Europe")
mean(gapminder[myFilter,"lifeExp"]$lifeExp) # remeember [] returns dataframe

#Group_by in dplyr
gapminder %>%
  filter(year == 2007) %>%
  group_by(continent) %>%
  summarize(avgLifeExp = mean(lifeExp)) %>%
  arrange(desc(avgLifeExp))

# Base R
is2007 <- gapminder$year==2007 #create a logical
reduced <- gapminder[is2007,c("continent","lifeExp")]
temp <- aggregate(reduced$lifeExp, by=list(reduced$continent),
          FUN=mean, na.rm=TRUE)
temp[order(-temp$x),]


###Daily
daily$day_of_week <- factor(daily$day_of_week, levels = c("Mon","Tues","Wed","Thurs","Fri","Sat","Sun"))

sampled <- sample(1:nrow(daily),nrow(daily)*.6, replace  = TRUE)
daily[sampled,]

# Your turn! dplyr_hw.R



