
library(data.table)
options(scipen=999)

Auto <- fread("U:/My Documents/Desktop/5. Modelling/26. Intro to Stat Learning/Chapter 3/Auto.csv")
fix(Auto)
summary(Auto$horsepower)
table(Auto$horsepower)

attach(Auto)
m1 <- lm(mpg~horsepower, data=Auto)
summary(m1)
# a.
# i. yes there is a relationship
# ii. it depends on the level, but in most cases it is quite strong, as p-value
#       smaller than 5% in most cases.
# iii. Negative relationship, as higher values of horsepower have smaller 
#       coefficients, thus meaning higher horsepower implies smaller mpg. 
pred_mpg = (29.00000000000009948-8.75000000000007105)
plot(horsepower, mpg)
# iv. pred mpg = 20.25
predict(m1, data.frame(horsepower=c('98')), interval="confidence")
predict(m1, data.frame(horsepower=c('98')), interval="prediction")
(29.00000000000009948-8.75000000000007105)+(c(1.96*3.39318401971619155,3.39318401971619155))

# b.
plot(horsepower,mpg)
abline(m1)

#c. 
plot(horsepower,m1$residuals)
# Clearly heterskedasticity as variance decreases with larger values of horsepower.
par(mfrow=c(2,2))
plot(m1)
