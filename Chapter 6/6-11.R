
rm(list=ls())
library(MASS)
library(ISLR)
library(leaps)
library(dplyr)
library(glmnet)
library(pls)
library(stats)

# Data
fix(Boston)

# a. 
# Here I choose to try out: best subset selection, the LASSO, and PCR.
# Split dset into Train/Test
set.seed(100)
nrow(Boston)
test_index <- sample(1:nrow(Boston), nrow(Boston), replace=FALSE)
# Ensure this worked properly:
head(test_index)
sum(test_index)==sum(1:nrow(Boston))
class(Boston)
# This worked properly.
is.vector(test_index)
test_index_final <- test_index[1:floor(0.3*nrow(Boston))]
train <- Boston[-test_index_final,]
test <- Boston[test_index_final,]
print(nrow(train)); print(nrow(test)); print((nrow(train)+nrow(test))==nrow(Boston))
fix(train)
fix(test)
# Successful in creating a dataset with 70% train, 30% test.

# (i.) Best subset:
fix(train)
ncol(train)
m1 <- regsubsets(x=train[,-1], y=train[,1], nvmax=13)
reg.fit <- summary(m1)
# Test:
fix(test)
# Set p as number of features:
p <- (ncol(newdata)-1)
# Function:
predict.regsubsets <- function(model, newdata)
{
  # Transform the test data into a model matrix:
  test.mat <- model.matrix(crim~., data=newdata)
  
  # Allocate vector for MSEs:
  val.errors <- rep(NA, p)
  
  # Loop thru each model and compute its MSE:
  for (j in 1:p)
  {
    coefi <- coef(model, id=j)
    pred <- test.mat[, names(coefi)] %*% coefi
    val.errors[j] <- mean( (newdata$crim-pred)^2 )
  }
  return(val.errors)
}
test.mse.results <- predict.regsubsets(m1, test)
plot(seq(1,p,by=1), test.mse.results)
# Zoom in on relevant section:
plot(seq(9,13,by=1), test.mse.results[9:13],
     main="Test Set MSE vs. Model Size", xlab="Number of Features",
     ylab="Test Set MSE")
# Clearly, a validation set approach indicates that a 12-variable
# model narrowly beats out a 13-variable model.
# 12-variable model MSE = 48.954.
mse_subset <- min(test.mse.results)

# ii. LASSO
tune_1 <- cv.glmnet(as.matrix(train[,-c(1)]), as.matrix(train[,1]), alpha=1, lambda=seq(0,0.6,by=0.001))
plot(lapply(tune_1$lambda, rev), tune_1$cvm, main="LASSO")
# The above confirms that the min cvm occurs for lambda around 0.18
# Thus the below value of 0.164 is reasonable.
tune_1_min <- tune_1$lambda.min
m2 <- glmnet(as.matrix(train[,-c(1)]), as.matrix(train[,1]), alpha=1, lambda=tune_1_min)
print(m2$a0); print(m2$beta); length(m2$beta)
# 7 feature non-zero est + 1 int = 8 non-zero parms.
# Score the predictions:
head(test)
pred3 <- predict(m2, newx=as.matrix(test[,-c(1)]), s=tune_1)
head(pred3)
mse_lasso <- mean((pred3-test$crim)^2)
print(mse_lasso)
print(mse_lasso/mse_subset-1)
# This shows LASSO produces a 4.5% deterioration in rmse relative
# to best subset regression.

# iii. PCA
set.seed(2)
pcr.fit <- pcr(crim~., data=train, scale=TRUE, validation='CV')
validationplot(pcr.fit, val.type="MSEP")
summary(pcr.fit)
# The minimum is at 13, however it is very close to 8.  
# For simplicity, I will use 8.
pred4 <- predict(pcr.fit, test[,-c(1)], ncomp=8)
head(pred4)
# Compute rmse:
mse_pcr <- mean((pred4-test$crim)^2)
print(mse_pcr); print(mse_pcr/mse_subset-1)
# 6.3% increase.  Let's try 13 components:
pred5 <- predict(pcr.fit, test[,-c(1)], ncomp=13)
# Compute rmse:
mse_pcr2 <- mean((pred5-test$crim)^2)
print(mse_pcr2); print(mse_pcr2/mse_subset-1)
# This is practically indistinguishable from the best subset model
# (0.015% greater).  So I choose best subset, as it has a much better
# validation set MSE than the LASSO, and it achieves a very slightly better
# MSE result than PCA while using 1 fewer feature!

# b.
# I propose the 12 variable best subset model as it has the lowest 
# validation set error, and even uses 1 fewer variable than the 
# best PCA model to achieve this! (i.e. it is parsimonious!)

# c.
summary(m1)
# No, my chosen model does not include the "age" feature.
cor(Boston[,-1])
cor(Boston[,-1])[,6]
# I would imagine that the reason it does not include age is because
# age has high correlation (abs val > 0.6) with indus, nox, dis, and lstat.
# I imagine that this causes age, incremental to these other features,
# to be a somewhat redundant feature.