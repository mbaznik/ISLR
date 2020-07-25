
# a-c
rm(list=ls())
set.seed(1)
x <- rnorm(100, mean=0, sd=1)
eps <- rnorm(100, mean=0, sd=sqrt(0.25))
y <- (-1+0.5*x+eps)
length(y)
# y is of length 100.
# b_0 = -1. and b_1 = 0.5

# d.
plot(x,y)
# I observe a line with a positive slope and y_int close to -1.

# e. 
m1 <- lm(y~x)
summary(m1)
confint(m1)
# b_0_hat=-1.019, which is close to -1.
# b_1_hat=0.49947, which is close to 0.5

# f.
plot(x,y)
abline(m1, col=4)
abline(-1,0.5,col=3)
legend(-1, c("model_fit","pop reg line"), col=4:3, lwd=3)

# g. 
m2 <- lm(y~x+I(x^2))
summary(m2)
# R squared increases from 0.467 to 0.478,
# and adjusted R squared increases too from 0.462 to .467.
# Both of the above provide some evidence that adding 
# the second order term improves fit, yet the p-value 
# corresponding to the F stat for the whole regression
# increases, suggesting worse fit. 

# h.
# Repeat with with less noise:
rm(list=ls())
set.seed(1)
x <- rnorm(100, mean=0, sd=1)
eps <- rnorm(100, mean=0, sd=sqrt(0.10))
y <- (-1+0.5*x+eps)
length(y)
plot(x,y)
m1 <- lm(y~x)
summary(m1)
confint(m1)
# b_0_hat=-1.012, which is closer to -1.
# b_1_hat=0.49966, which is closer to 0.5
plot(x,y)
abline(m1, col=4)
abline(-1,0.5,col=3)
legend(-1, c("model_fit","pop reg line"), col=4:3, lwd=3)
# Est parms fit true parms better, fitted line is closer
# to pop reg line.

# i.
# Repeat with with more noise:
rm(list=ls())
set.seed(1)
x <- rnorm(100, mean=0, sd=1)
eps <- rnorm(100, mean=0, sd=sqrt(1))
y <- (-1+0.5*x+eps)
length(y)
plot(x,y)
m1 <- lm(y~x)
summary(m1)
confint(m1)
# b_0_hat=-1.038, which is further from -1.
# b_1_hat=0.4989, which is further from 0.5
plot(x,y)
abline(m1, col=4)
abline(-1,0.5,col=3)
legend(-1, c("model_fit","pop reg line"), col=4:3, lwd=3)
# Est parms fit true parms worse, fitted line is further
# from pop reg line.

# j.
# CI's for b_0 from:
# orig: -1.1150804 -0.9226122
# less: -1.0727832 -0.9510557
# more: -1.2301607 -0.8452245
# CI's for b_1 from:
# orig: 0.3925794  0.6063602
# less: 0.4320613  0.5672681
# more: 0.2851588  0.7127204
# Clearly less/more noise in data decreases/increases
# the size of the confidence intervals.
