
rm(list=ls())
library(ISLR)
library(tidyverse)
library(stats)
library(caret)
library(dplyr)
library(MASS)
library(class)

# a
fix(Weekly)
attach(Weekly)
class(Weekly)
# Plot Today vs. Time:
time <- seq(1,nrow(Weekly),by=1)
plot(time, Today)
# Does not appear to be clearly trending up or down, but 
# increased variance of data around the 600th and 1000th weeks.
cor(Weekly[,2:8])
# Today exhibits only weak correlation with Lags 1-5 and Volume.
# The same can be said for correl between other pairs of vars.
summary(Weekly)
# Lags 1-5 and Today all have similar Max, Min, and Means.
# I do not see many patterns in the data.

# b.
logreg_data <- Weekly[,c(2:7,9)]
fix(logreg_data)
head(logreg_data)
m1 <- glm(Direction~., data=logreg_data, family=binomial)
summary(m1)
# At the 5% level, Volume and all Lags except Lag2 appear stat sig.

# c.
# Confusion Matrix: (1 is Up, and 0 is Down)
glm.probs = predict(m1, type = "response")
glm.pred = rep("Down", length(glm.probs))
glm.pred[glm.probs > 0.5] = "Up"
# Confusion
table(glm.pred, Direction)
sum(table(glm.pred, Direction)[1],table(glm.pred, Direction)[4])/sum(table(glm.pred, Direction))
# 56% of predictions are correct. The logistic regression is usually correct
# when it predicts the market will go up, and usuall wrong when it predicts the
# market will go down.

# d.
# Train
fix(Weekly)
train <- Weekly[Year<2009,][,c(3,7,9)]
fix(train)
m2 <- glm(Direction~Lag2, data=train, family=binomial)
summary(m2)
# Test:
test <- Weekly[Year>=2009,][,c(3,7,9)]
pred2 <- predict(m2, type="response", newdata=test)
glm.pred <- rep("Down", length(pred2))
head(glm.pred, 20)
glm.pred[pred2>=0.5] = "Up"
x1 <- table(glm.pred, test$Direction)
perc_correct <- (x1[1,1]+x1[2,2])/sum(x1)
# 0.625 correct. 

# e.
lda.fit <- lda(Direction~Lag2, data=train)
summary(lda.fit)
pred3 <- predict(lda.fit, type="response", newdata=test)$class
head(pred3, 10)
x1 <- table(pred3, test$Direction)
perc_correct <- (x1[1,1]+x1[2,2])/sum(x1)
# 0.625 correct.

# f.
nrow(Weekly)
qda.fit <- qda(Direction~Lag2, data=train)
summary(qda.fit)
pred4 <- predict(qda.fit, type="response", newdata=test)$class
head(pred4, 10)
x1 <- table(pred4, test$Direction)
perc_correct <- (x1[1,1]+x1[2,2])/sum(x1)
# 0.587 correct.

# g.
set.seed(1)
knn.pred <- knn(data.frame(train[,1]), data.frame(test[,1]), train[,2], k=1)
head(knn.pred)
class(knn.pred)
predictions <- ifelse(knn.pred>=0.5, "Up", "Down")
x1 <- table(knn.pred, test$Direction)
perc_correct <- (x1[1,1]+x1[2,2])/sum(x1)
# 0.5 correct.

# h.
# LDA and Logistic Regression give the best results on this data. 

# i. Experiment with different methods:
# 1. Logistic regression:
fix(train)
fix(test)
mii <- glm(Direction~Lag2+Volume+Lag2*Volume, data=train, family=binomial)
summary(mii)
probs <- predict(mii, type="response", newdata=test)
preds <- rep("Down", length(probs))
preds[probs>=0.5] <- "Up"
x <- table(preds, test$Direction)
(x[1,1]+x[2,2])/sum(x)
# 0.538 percent correct.
mij <- glm(Direction~Lag2+Volume, data=train, family=binomial)
summary(mij)
p1 <- predict(mij, type="response", newdata=test)
preds <- rep("Down", length(p1))
preds[p1>=0.5] <- "Up"
x <- table(preds, test$Direction)
(x[1,1]+x[2,2])/sum(x)
# 0.538 correct.  This was not helpful.
# 2. LDA
lda.fit <- lda(Direction~Lag2+Volume, data=train)
summary(lda.fit)
pred3 <- predict(lda.fit, type="response", newdata=test)$class
head(pred3, 10)
x1 <- table(pred3, test$Direction)
perc_correct <- (x1[1,1]+x1[2,2])/sum(x1)
# 0.538 correct.  Same as logistic regression.
# 3. QDA
qda.fit <- qda(Direction~Lag2+Volume, data=train)
summary(qda.fit)
pred4 <- predict(qda.fit, type="response", newdata=test)$class
head(pred4, 10)
x1 <- table(pred4, test$Direction)
perc_correct <- (x1[1,1]+x1[2,2])/sum(x1)
# 0.471 correct.
# 4. KNN with 2 and 3 neighbours.
set.seed(1)
model1 <- knn(data.frame(train[,1:2]), data.frame(test[,1:2]), train[,3], k=2)
x2 <- table(model1, test$Direction)
(x2[1,1]+x2[2,2])/sum(x2)
# 0.558 correct. This is an improvement on k=1.
model2 <- knn(data.frame(train[,1:2]), data.frame(test[,1:2]), train[,3], k=3)
x3 <- table(model2, test$Direction)
(x3[1,1]+x3[2,2])/sum(x3)
# 0.548 predicted correctly.
model3 <- knn(data.frame(train[,1:2]), data.frame(test[,1:2]), train[,3], k=4)
x4 <- table(model3, test$Direction)
(x4[1,1]+x4[2,2])/sum(x4)
# 0.49 predicted correctly. 
# Clearly KNN with k=2 seems to be optimal. 