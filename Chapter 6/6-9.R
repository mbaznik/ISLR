
rm(list=ls())
library(ISLR)
library(leaps)
library(dplyr)
library(glmnet)
library(pls)

# a.
head(College)
set.seed(1)
train_ind <- ifelse(runif(nrow(College))>0.7, FALSE, TRUE)
sum(train_ind)
train <- College[train_ind==TRUE,]
test <- College[train_ind==FALSE,]
nrow(train); nrow(test); nrow(train)+nrow(test)==nrow(College)
head(train)

# b. 
m1 <- lm(Apps~., data=train)
summary(m1)
head(m1$fitted.values)
pred <- predict(m1, newdata=test)
head(pred)
head(test)
rmse <- sqrt(mean((pred-test$Apps)^2))
print(rmse)
# The below plot showing that predicted lines up well with actual
# suggests that OLS (and other methods too) do a good job predicting actual
# apps.
plot(pred, test$Apps, main="Actual vs. Pred", xlim=c(0, 20000))

# c.
is.data.frame(train)
table(train$Private)
train$Private_bin <- ifelse(train$Private=="Yes", 1, 0)
test$Private_bin <- ifelse(test$Private=="Yes", 1, 0)
lambda_1 <- cv.glmnet(as.matrix(train[,-c(1,2)]), as.matrix(train[,2]), alpha=0, lambda=seq(0,40,by=0.01))
plot(lapply(lambda_1$lambda, rev), lambda_1$cvm, main="Ridge")
# The above confirms that the min cvm occurs for lambda around 20-25.
# Thus the below value of 22.37 is reasonable.
lambda_1_min <- lambda_1$lambda.min
m2 <- glmnet(as.matrix(train[,-c(1,2)]), as.matrix(train[,2]), alpha=0, lambda=lambda_1_min)
# Score the predictions:
head(test)
pred2 <- predict(m2, newx=as.matrix(test[,-c(1,2)]), s=lambda_1_min)
head(pred2)
rmse_ridge <- sqrt(mean((pred2-test$Apps)^2))
print(rmse_ridge)
print(rmse_ridge/rmse-1)
# This shows ridge produces a 4.7% deterioration in rmse relative to OLS.

# d.
lambda_2 <- cv.glmnet(as.matrix(train[,-c(1,2)]), as.matrix(train[,2]), alpha=1, lambda=seq(0,40,by=0.01))
plot(lapply(lambda_2$lambda, rev), lambda_2$cvm, main="LASSO")
# The above confirms that the min cvm occurs for lambda around 15.
# Thus the below value of 14.49 is reasonable.
lambda_2_min <- lambda_2$lambda.min
m3 <- glmnet(as.matrix(train[,-c(1,2)]), as.matrix(train[,2]), alpha=1, lambda=lambda_2_min)
print(m3$a0); print(m3$beta); length(m3$beta)
# 14 feature non-zero est + 1 int = 15 non-zero
# Score the predictions:
head(test)
pred3 <- predict(m3, newx=as.matrix(test[,-c(1,2)]), s=lambda_2_min)
head(pred3)
rmse_lasso <- sqrt(mean((pred3-test$Apps)^2))
print(rmse_lasso)
print(rmse_lasso/rmse-1)
# This shows LASSO produces a 2.9% deterioration in rmse relative to OLS.

# e.
set.seed(2)
train_data <- train[,-1]
head(train_data)
pcr.fit <- pcr(Apps~., data=train_data, scale=TRUE, validation='CV')
validationplot(pcr.fit, val.type="MSEP")
summary(pcr.fit)
# Surprisingly, 17 components is viewed as optimal by cv rmse.
pred4 <- predict(pcr.fit, test[,-c(1,2)], ncomp=17)
head(pred4)
# Compute rmse:
rmse_pcr <- sqrt(mean((pred4-test$Apps)^2))
print(rmse_pcr); print(rmse_pcr/rmse-1)
# Not surprisingly, when we use all the components, we get the same
# results as OLS. 

# f.
set.seed(3)
pls.fit <- plsr(Apps~., data=train_data, scale=TRUE, validation='CV')
summary(pls.fit)
validationplot(pls.fit, val.type="MSEP")
# This suggests that minimum adjusted CV is first attained at 13 comps:
pred5 <- predict(pls.fit, test[,-c(1,2)], ncomp=13)
# Compute rmse:
rmse_pls <- sqrt(mean((pred5-test$Apps)^2))
print(rmse_pls); print(rmse_pls/rmse-1)
# RMSE here is 1342, representing slight 0.05% deterioration as opposed to OLS.
# There is not much difference among the test errors obtained.
# The above suggests that given the above data, given the approaches we tried,
# we are not able to predict our target variable any better than OLS.
# This suggests to me that each of the 17 feature variables seems to contribute
# some useful and unique info, as excluding 1 or more of the features
# generally causes slight deterioration in predictive power.
# The above plot shows that Actual and Predicted Apps line up well, thus
# showing good predictive power.
