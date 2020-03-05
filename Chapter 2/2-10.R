
rm(list=ls())
library(MASS)
library(data.table)

head(Boston)
summary(Boston)
nrow(Boston)
ncol(Boston)
fix(Boston)

# Scatterplots:
pairs(Boston)
attach(Boston)

#plot(indus,crim)
#plot(black,crim)
#plot(age,crim)
#plot(tax,crim)
plot(medv,crim)
#plot(ptratio,crim)

#Crime rates and tax rates:
Boston_dt <- data.table(Boston)
head(Boston[order(-crim),], 5)
# Yes 381, 419, and 406 have high crime rates.

# How many bound Charles River?
nrow(Boston[chas==1,])
# 35 districts.

# f. Median pteacher ratio:
ncol(Boston)
median(Boston[,12])
# 391.44

# g. Lowest value
head(Boston[order(medv),],5)
# district 399.

nrow(Boston[rm>7,])
# 64
nrow(Boston[rm>8,])
# 13.