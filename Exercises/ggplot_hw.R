

# 1
ggplot(data=diamonds, mapping=aes(x=clarity)) + 
  geom_bar(aes(fill=cut))+
  theme(legend.position = c(.85,.75))

#2
ggplot(data=diamonds, mapping=aes(x=price)) +
  geom_density(aes(fill = cut), alpha=.2) + 
  theme(legend.position = c(.85,.75))

#3
ggplot(data=diamonds, mapping=aes(x=color, y=price)) + 
  geom_boxplot(fill="steelblue4")

#4
ggplot(data=diamonds, mapping=aes(x=color, y=price)) + 
  geom_boxplot(fill="steelblue4") +
  facet_wrap (~clarity, ncol = 2)

#5
ggplot(data = diamonds, mapping=aes(x=carat, y=price)) +
  geom_point() +
  geom_smooth(method="lm", formula = y ~x, se=FALSE, color="red")



