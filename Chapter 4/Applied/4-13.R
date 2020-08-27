
rm(list=ls())
library(MASS)
library(stats)
library(class)

# Data prep
attach(Boston)
head(Boston, 5)
Boston$crim_above <- rep(0, nrow(Boston))
Boston$crim_above[crim>median(crim)] <- 1
# Test/Train:
ind <- ifelse(runif(nrow(Boston))>0.3, 1, 0)
train <- Boston[ind == 1,]
nrow(train)
test <- Boston[ind != 1,]
nrow(train)+nrow(test);nrow(Boston)

# Var selection:
pairs(Boston)
cor(Boston)
# Given the above, let's try nox and indus.

# a. Logistic Regression
# One Var
m1 <- glm(crim_above~nox, data=train, family="binomial")
p1 <- predict(m1, type="response", newdata=test)
p1_pred <- ifelse(p1>=0.5, 1, 0)
x1 <- table(p1_pred, test$crim_above)
test_err <- c()
e <- (x1[1,1]+x1[2,2])/sum(x1)
test_err <- rbind(test_err, e)
# Two Var
m2 <- glm(crim_above~nox+indus, data=train, family="binomial")
p2 <- predict(m2, type="response", newdata=test)
p2_pred <- ifelse(p2>=0.5, 1, 0)
x1 <- table(p2_pred, test$crim_above)
e <- (x1[1,1]+x1[2,2])/sum(x1)
test_err <- rbind(test_err, e)

# b. LDA
# One var
m3 <- lda(crim_above~nox, data=train)
p3 <- predict(m3, type="response", newdata=test)$class
x1 <- table(p3, test$crim_above)
e <- (x1[1,1]+x1[2,2])/sum(x1)
test_err <- rbind(test_err, e)
# Two var
m4 <- lda(crim_above~nox+indus, data=train)
p4 <- predict(m4, type="response", newdata=test)$class
x1 <- table(p4, test$crim_above)
e <- (x1[1,1]+x1[2,2])/sum(x1)
test_err <- rbind(test_err, e)

# c. KNN
# One var
m5 <- knn(train=cbind(train[,5]), test=cbind(test[,5]), cl=train[,15], k=3)
x1 <- table(m5, test$crim_above)
e <- (x1[1,1]+x1[2,2])/sum(x1)
test_err <- rbind(test_err, e)
# Two var
m6 <- knn(train=cbind(train[,c(3,5)]), test=cbind(test[,c(3,5)]), cl=train[,15], k=3)
x1 <- table(m6, test$crim_above)
e <- (x1[1,1]+x1[2,2])/sum(x1)
test_err <- rbind(test_err, e)
# Here KNN with two var and k=3 is optimal among alternative tried.
