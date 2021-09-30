
rm(list=ls())
library(leaps)
library(dplyr)
library(glmnet)

# a.
set.seed(1)
x <- rnorm(100)
epsilon <- rnorm(100)
head(x); head(epsilon)

# b.
# Control - Define constants:
b0 <- 4; b1 <- 5; b2 <- -4; b3 <- 3
# Construct response:
y <- b0 + b1*x + b2*x^2 + b3*x^3 + epsilon 
head(y)

# c.
data <- y
for (i in 1:10)
{
  data <- cbind(data, x^i)
}
data_df <- as.data.frame(data)
head(data_df)
data_df_names <- (data_df %>% rename(y=data, x=V2, x2=V3, x3=V4, x4=V5, x5=V6, x6=V7, x7=V8, x8=V9,
                                     x9=V10, x10=V11))
head(data_df_names)
# Best subset:
m1 <- regsubsets(y~., data=data_df_names, nvmax=10)
m1_summary <- summary(m1)
names(m1_summary)
# Plots:
plot(seq(1,10,by=1), m1_summary$cp)
plot(seq(3,10,by=1), m1_summary$cp[3:10])
# Mallow's c implies that the 4 variable model is optimal
plot(seq(1,10,by=1), m1_summary$adjr2)
plot(seq(3,10,by=1), m1_summary$adjr2[3:10])
# Adj R^2 as well implies that the 4 variable model is optimal
plot(seq(1,10,by=1), m1_summary$bic)
plot(seq(3,10,by=1), m1_summary$bic[3:10])
# BIC implies that the 3-variable model is optimal.  Not surprising
# that it is a more parsimonious model than cp and adjr2 b/c
# BIC has a higher penalty, and often is too punitive on more parms.
# Given a 2 of 3 plurality vote, I select the 4-variable model
# and below report its coefs:
coef(m1, 4)
# (Intercept)           x          x2          x3          x5 
# 4.07200775  5.38745596 -4.15424359  2.55797426  0.08072292
coef(m1, 3)
# However, BIC correctly selects the DGP, and its coefs are
# quite close to the true ones!
# (Intercept)           x          x2          x3 
# 4.061507    4.975280   -4.123791    3.017639

# d. 
# Forward stepwise selection:
m2 <- regsubsets(y~., data=data_df_names, nvmax=10, method="forward")
m2_summary <- summary(m2)
names(m2_summary)
# Plots:
plot(seq(1,10,by=1), m2_summary$cp)
plot(seq(3,10,by=1), m2_summary$cp[3:10])
# Suggests 4, same as above.
plot(seq(1,10,by=1), m2_summary$adjr2)
plot(seq(3,10,by=1), m2_summary$adjr2[3:10])
# Suggests 4, same as above.
plot(seq(1,10,by=1), m2_summary$bic)
plot(seq(3,10,by=1), m2_summary$bic[3:10])
# Suggests 3, same as above.
# Backward:
m3 <- regsubsets(y~., data=data_df_names, nvmax=10, method="backward")
m3_summary <- summary(m3)
names(m3_summary)
# Plots:
plot(seq(1,10,by=1), m3_summary$cp)
plot(seq(3,10,by=1), m3_summary$cp[3:10])
# Suggests 4, same as above.
plot(seq(1,10,by=1), m3_summary$adjr2)
plot(seq(3,10,by=1), m3_summary$adjr2[3:10])
# Suggests 4, same as above.
plot(seq(1,10,by=1), m3_summary$bic)
plot(seq(3,10,by=1), m3_summary$bic[3:10])
# Suggests 3, same as above.
# All of the conclusions are the same, regardless of which
# of the 3 methods we choose. 

# e.
m4 <- cv.glmnet(as.matrix(data_df_names[,2:11]), 
                as.matrix(data_df_names[,1]), alpha=1)
summary(m4)
best_lam <- m4$lambda.min
#  Optimal value of lambda is 0.05792
plot(m4$lambda, (m4$cvsd)^2, xlim=c(0,0.1))
# Plot shows best_lam produces lowest cvsd, and thus lowest cv error.
m5 <- glmnet(as.matrix(data_df_names[,2:11]), 
                as.matrix(data_df_names[,1]), alpha=1, lambda=best_lam)
coef(m5)
# Selected coefficients:
# s0
# (Intercept)  4.02436329
# x            5.31155555
# x2          -4.08452199
# x3           2.60461865
# x4           .         
# x5           0.06799156
# x6           .         
# x7           .         
# x8           .         
# x9           .         
# x10          .         
# Results obtained suggest that very little penalty is optimal.
# Perhaps it realizes that x6-x10 coef are close to 0 (since simply
# noise), and it only penalizes a little bit just to set most of 
# those coef = 0, but no need to shrink x-x3 coef significantly,
# since those are truly signal from the DGP.

# f.
# Generate DGP:
# Construct response:
y2 <- b0 + b2*x^7 + epsilon
# Best subset selection:
m6 <- regsubsets(y2~., data=data_df_names, nvmax=10)
m6_summary <- summary(m6)
# It correctly identifies the model with only an int and  x^7 as
# the best 1 variable model.
names(m6_summary)
# Plots:
plot(seq(1,10,by=1), m6_summary$cp)
plot(seq(6,10,by=1), m6_summary$cp[6:10])
which.min(m6_summary$cp)
which.min(m6_summary$adjr2)
which.min(m6_summary$bic)
# Mallow's Cp recommends the model with all 10 variables in it,
# while adjr2 recommends 1, and bic recommends 6.
coefficients(m6, id=1)
dd <- x^7
m6_5 <- lm(y2~dd)
summary(m6_5)
# Estimates Int=3.95 instead of 4, and x^7 coef of -3.99 instead of -4.
# LASSO:
m7 <- cv.glmnet(as.matrix(data_df_names[,2:11]), 
                as.matrix(y2), alpha=1)
summary(m7)
best_lam <- m7$lambda.min
# Optimal value of lambda is 7.754672
m8 <- glmnet(as.matrix(data_df_names[,2:11]), 
             as.matrix(y2), alpha=1, lambda=best_lam)
summary(m8)
m8$beta
m8$a0
# This estimates int=3.4 and x^7 coef of -3.87, so not 
# as close to true DGP as best subset.
