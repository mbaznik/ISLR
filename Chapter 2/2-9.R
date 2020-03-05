
library(car)

Auto <- read.csv("U:/My Documents/Desktop/5. Modelling/26. Intro to Stat Learning/Chapter 2/Auto.csv")
fix(Auto)
summary(Auto)
table(Auto$horsepower)
head(Auto[Auto$horsepower=='?',])
table(Auto$name)

Auto_fixed <- Auto[Auto$horsepower!='?',]
nrow(Auto)
nrow(Auto_fixed)
table(Auto_fixed$horsepower)
summary(Auto_fixed)
attach(Auto_fixed)
fix(Auto_fixed)

range(mpg)
result <- apply(Auto_fixed[,1:7], 2, range)
result

mean <- apply(Auto_fixed[,1:3], 2, mean)
sd <- apply(Auto_fixed[,1:7], 2, sd)

Auto_remain <- Auto_fixed[-c(10:85),]
fix(Auto_remain)
mean <- apply(Auto_remain[,1:3], 2, mean)
mean

attach(Auto_fixed)
pairs(Auto_fixed)
par(mfrow=c(2,2))
plot(mpg, cylinders)
plot(mpg, displacement)
plot(mpg, weight)
plot(mpg, acceleration)
Auto_fixed[1,1]
head(Auto_fixed)

m1 <- lm(mpg~cylinders+displacement+weight+acceleration,
         data=Auto_fixed)
summary(m1)
vif(m1)
m2 <- lm(mpg~cylinders+weight+acceleration, data=Auto_fixed)
vif(m2)
