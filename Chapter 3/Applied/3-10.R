
rm(list=ls())
library(ISLR)
library(tidyverse)

#a
fix(Carseats)
m1 <- lm(Sales~Price+Urban+US, data=Carseats)
summary(m1)

#b
#Price = for a 1 unit increase in price, sales pred to decrease by -0.05
#Urban = for moving from nonUrban to Urban, sales pred to decrease by -0.022
#US = moving from nonUS to US, sales pred to increase by 1.2

#c. on paper.

#d. can reject the null for Price and US (at the 5% level)

#e. 
m2 <- lm(Sales~Price+US, data=Carseats)
summary(m2)

#f. 
#m2 has a slighly higher adjusted R^2, which indicates that it fits better.
# p-values are similar.
#both residuals are similar.
par(mfrow=c(2,2))
plot(m1$residuals)
plot(m2$residuals)
m1$AIC

#g.
confint(m2)

#h
# There are several points that exceed
# 4/nrow(Carseats), so yes some pts with lev.
par(mfrow=c(2,2))
plot(m2)

# Since all resid bounded by 3 in abs value, no
# outliers.
plot(predict(m2), rstudent(m2))
