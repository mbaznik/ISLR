
rm(list=ls())
library(data.table)
library(ISLR)
library(tidyverse)
library(stats)
library(caret)
library(dplyr)
library(MASS)
library(class)

# Import
Auto <- fread("U:/My Documents/Desktop/5. Modelling/26. Intro to Stat Learning/Chapter 3/Auto.csv", header=TRUE)
fix(Auto)

# a.
attach(Auto)
Auto$mpg01 <- ifelse(mpg>=median(mpg), 1, 0)
Auto$mpg011 <- rep(0, nrow(Auto))
Auto$mpg011[mpg>=median(mpg)] <- 1

# b.
summary(Auto)
cor(Auto[,-c(4,9)])
pairs(Auto[,-c(4,9)])
# Obviously mpg, but that is not useful b/c we
# are looking at a variable derived from it.
# Weight certainly shows promise, as low values
# of weight related to 1 values of mpg01.
# Similarly acceleration shows promise.

# c.
ind <- ifelse(runif(nrow(Auto))>=0.3, 1, 0)
fix(ind)
train <- Auto[ind==1,]
test <- Auto[ind==0,]
nrow(train); nrow(test)

# d.
m1 <- lda(mpg01~weight+acceleration, data=train)
pred1 <- predict(m1, type="response", newdata=test)$class
head(pred1)
x1 <- table(test$mpg01, pred1)
1-(sum(x1[1,1])+sum(x1[2,2]))/sum(x1)
# Test error rate = 17.4%.

# e.
m2 <- qda(mpg01~weight+acceleration, data=train)
pred2 <- predict(m2, type="response", newdata=test)$class
head(pred2)
x2 <- table(test$mpg01, pred2)
1-(sum(x2[1,1])+sum(x2[2,2]))/sum(x2)
# Test error rate = 15.7%.

# f.
m3 <- glm(mpg01~weight+acceleration, data=train, family="binomial")
pred3 <- ifelse(predict(m3, newdata=test, type="response")>=0.5, 1, 0)
x3 <- table(pred3, test$mpg01)
1-(sum(x3[1,1])+sum(x3[2,2]))/sum(x3)
# Test error rate = 14.9%.

# h.
train2 <- cbind(train$weight, train$acceleration)
test2 <- cbind(test$weight, test$acceleration)
storage <- c()
for (i in 1:100)
{
  # Train the algorithm:
  model <- knn(train=train2, test=test2, cl=train[,11], k=i)
  
  x <- table(model, test$mpg01)
  error <- (1-(x[1,1]+x[2,2])/sum(x))
  storage <- rbind(storage, error)
}
par(mfrow=c(1,1))
plot(seq(1,100,by=1), storage)
min(storage)
storage[43,]
# Value of k=42 appears to be best.  Test error rate is 8%.