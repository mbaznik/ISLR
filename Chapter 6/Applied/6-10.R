
rm(list=ls())
library(ISLR)
library(leaps)
library(dplyr)
library(glmnet)
library(pls)
library(stats)

# a. Generate a data set with p=20 features and n=1,000 obs.
# Control:
p <- 20
n <- 1000
# Create data:
x <- c()
for (i in 1:p)
{
  set.seed(i)
  x <- cbind(x, rnorm(n)) 
}
fix(x)
# Create errors:
set.seed(1000)
epsilon <- rnorm(n, mean=0, sd=20)
# Create response:
set.seed(2000)
coef_pool <- c(-2,-1,0,1,2)
beta <- matrix(sample(coef_pool, size=p, replace=TRUE), ncol=1)
y <- ( y<-as.data.frame(y<-(x %*% beta + epsilon)) %>% rename(y=V1) )
fix(y)
data <- cbind(y=y, x)
fix(data)

# b. Split dset into Train/Test
set.seed(100)
test_index <- sample(1:1000, 100, replace=FALSE)
train <- data[-test_index,]
test <- data[test_index,]
print(nrow(train)); print(nrow(test))
fix(train)
fix(test)

# c. Train Set MSE
m1 <- regsubsets(x=train[,-1], y=train[,1], nvmax=20)
reg.fit <- summary(m1)
names(reg.fit)
plot(1:20, reg.fit$rss, main="Train Set MSE vs. Model Size")

# d.
predict.regsubsets <- function(model, newdata)
{
  # Transform the test data into a model matrix:
  test.mat <- model.matrix(y~., data=newdata)
  
  # Allocate vector for MSEs:
  val.errors <- rep(NA, p)
  
  # Loop thru each model and compute its MSE:
  for (j in 1:p)
  {
    coefi <- coef(model, id=j)
    pred <- test.mat[, names(coefi)] %*% coefi
    val.errors[j] <- mean( (newdata$y-pred)^2 )
  }
  return(val.errors)
}
test.mse.results <- predict.regsubsets(m1, test)
plot(seq(1,p,by=1), test.mse.results)
# Zoom in on relevant section:
plot(seq(8,13,by=1), test.mse.results[8:13], main="Test Set MSE vs. Model Size")

# e.
# The 12 variable model has the lowest test set MSE.
# I had to play around with the size of the sd of the epsilon relative
# to that of the feature vars, as initially the 20 variable model
# was indicated as optimal.  After increasing the sd of the epsilon
# significantly, this increased noise caused overfitting in models
# that used most/all of the features, and thus the 12 variable 
# model was indicated as optimal, and the 9 variable model
# indicated as a close second. 

# f.
# The fact that the indicated optimal model uses 12 instead of 20
# features indicates that we needed to increase bias to reduce volatility
# and only fit 12 variables to achieve optimal results.  
print(t(beta)); print(coef(m1, id=12))
# I did not include an intercept, which changes the result. 
# For those variables included in both models, the coefficients
# are generally close:
# 
x <- substr(rownames(as.matrix(coef(m1, id=12))), start=4, stop=5)[-c(1)]
x2 <- (as.numeric(noquote(x))+0)
coef_est <- coef(m1, id=12)[-1]
coef_act <- t(beta)[,x2]
plot(coef_act, coef_est, main="Plot of Est vs. Act Coef")

# g.
# Here I focus on feature coefficient estimates.
coef_rmse_storage <- c()
for (k in 8:13)
{
  x <- substr(rownames(as.matrix(coef(m1, id=k))), start=4, stop=5)[-c(1)]
  x2 <- (as.numeric(noquote(x))+0)
  coef_est <- coef(m1, id=k)[-1]
  coef_act <- t(beta)[,x2]
  # Compute RMSE and append:
  temp <- sqrt(sum((coef_act-coef_est)^2))
  coef_rmse_storage <- rbind(coef_rmse_storage, temp)
}
plot(seq(8,13,by=1), coef_rmse_storage, main="Plot of Coef RMSE vs. Num Feat")
# Not surprisingly, this drifts/trends up, as more coefficients 
# included increases the RMSE of its estimated coefficients against actual.
# While the 12 feature model had the lowest test MSE, it actually has the
# highest coefficient RMSE of the subset of models 8:13 examined here.
# Clearly this suggests that lowest test RMSE does not imply 
# lowest coefficient RMSE!
