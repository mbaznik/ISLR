
rm(list=ls())
library(ISLR)
library(stats)
library(boot)

# a. Estimated std errors using GLM function:
fix(Default)
set.seed(1)
m1 <- glm(default~income+balance, data=Default, family="binomial")
summary(m1)
ls(m1)
# Indicated std errors 
ideal <- summary(m1)$coefficients[2:3,2]

# b. 
set.seed(1)
boot.fn <- function(data,index)
{return( coef(glm(default~income+balance, data=data, subset=index, family=binomial)))}
boot.fn(Default,vec)

# c.
set.seed(1)
o1 <- boot(Default, boot.fn, R=500)

# d. 
# Std error estimates from my bootstrap are similar to those estimated by 
# the glm function.
