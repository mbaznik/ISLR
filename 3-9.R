
rm(list=ls())
library(data.table)
library(dplyr)
options(scipen=999)

Auto <- fread("U:/My Documents/Desktop/5. Modelling/26. Intro to Stat Learning/Chapter 3/Auto.csv")
Autodf <- data.frame(Auto)
fix(Autodf)
attach(Autodf)

# a. 
pairs(Autodf)
#b.
cor((Autodf %>% select(mpg,cylinders,displacement)))
#c
m2 <- lm(mpg~.-name, data=Autodf)
summary(m2)
#c.1: yes relationship, since F p=value is so small
#c.2:  year, origin, acceleration, etc.
#c.3: positive coef, so larger year var is (aka newer car),
#better gas mileage will be, ceteris paribus.
#d
par(mfrow=c(2,2))
plot(m2)
#e
m3 <- lm(mpg~cylinders*displacement)
summary(m3)
m4 <- lm(mpg~cylinders+displacement+cylinders:displacement)
summary(m4)

#f
m44 <- lm(mpg~cylinders)
summary(m44)
m5 <- lm(mpg~cylinders+cylinders^2)
summary(m5)
m6 <- lm(mpg~cylinders+sqrt(cylinders))
summary(m6)