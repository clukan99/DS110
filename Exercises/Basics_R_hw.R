# Example answers for each item

# 1
a <- 2.3
(6*a + 42) / 3^(4.2-3.62)

# 2
b <-  3^2 * 4^(1/8) / 2.33

# 3
c <- -8.2 * 10^-13

# 4
b / c

# 5
v1 <- c(23, 32, 67, 12, 19, 40, 18, 45)

# 6
v1[3]

#7
v2 <- v1^2 - 15

#8
cbind("mean" = mean(v2), "sd" = sd(v2))

#9
df <- data.frame(v1, v2)

#10
df$v1[df$v1 > 35]
v1[v1>35]
# subset(v1, v1 > 35)

#11
df$v3 <- df$v1 %% 2 == 0
# v3 <- ifelse (v1%%2==0, TRUE, FALSE)

#12
df$v1[df$v3]
