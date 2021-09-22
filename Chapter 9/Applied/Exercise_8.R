
# Chapter 9, Exercise 8
# Packages/options:
rm(list=ls())
# For svm() function
library(e1071)
# For OJ data:
library(ISLR)

# Parms:
nrow_tot <- nrow(OJ)
print(nrow_tot) # 1070 rows
nrow_train <- 800 # Per problem

# Part a: Create a training set containing 800 obs
# (~75% of data), and a test set with the remainder.
head(OJ)
# For reproducibility:
set.seed(1)
OJ_MAB <- OJ
# For random sort:
OJ_MAB$rand_sort <- runif(nrow_tot, 0,1)
# head(OJ_MAB)
OJ_MAB <- OJ_MAB[order(OJ_MAB$rand_sort),]
head(OJ_MAB)
# Train and test:
train <- OJ_MAB[1:nrow_train,]
test  <- OJ_MAB[(nrow_train+1):nrow_tot,]
print(nrow(train)); print(nrow(test)); print(nrow_tot)
# Remove rand_sort
train <- subset(train, select=-c(rand_sort))
test  <- subset(test, select=-c(rand_sort))
print(head(train)); print(head(test))

# Part b: Train a SVC to training data with cost=0.01
head(train)
svc_fit <- svm(Purchase~., data=train, kernel="linear", cost=0.01)
summary(svc_fit)
# This SVC has 438 support vectors, which seems like a lot.
# This is b/c the cost is very small, so the optimal solution
# here will be a very flexible SVC, which may likely overfit
# the data from having too much flexibility.

# Part c: Determine train and test error rates:
train$svc_pred <- predict(svc_fit, train)
test$svc_pred  <- predict(svc_fit, test)
# Train:
train_table <- table(train$svc_pred, train$Purchase)
train_error_rate <- (train_table[1,2]+train_table[2,1])/sum(train_table)
print(train_error_rate)
# Train error rate is 16.6%
# Test:
test_table <- table(test$svc_pred, test$Purchase)
test_error_rate <- (test_table[1,2]+test_table[2,1])/sum(test_table)
print(test_error_rate)
# Test error rate is 17.8%

# Part d: Use the tune function to select optimal value of cost:
set.seed(1)
tune.out <- tune(svm, Purchase~., data=train, kernel="linear", 
                 ranges=list(cost=c(0.01, 0.1, 1, 5, 10)))
summary(tune.out)
# Interestingly enough, all values in this range indicate an equal
# CV error: the 16.6% seen above.  So would we be indifferent to 
# any of these values of cost, or would we prefer 10 since highest
# cost corresponds to the simplest model?  Yes, I select 10.

# Part e: Compute training and test error rates using my new
# value of cost:
set.seed(1)
svc_fit_opt <- svm(Purchase~., data=train, kernel="linear", cost=10)
summary(svc_fit_opt)
# Predict:
train$svc_pred_opt <- predict(svc_fit_opt, train)
test$svc_pred_opt  <- predict(svc_fit_opt, test)
# Train:
train_table2 <- table(train$svc_pred_opt, train$Purchase)
train_error_rate2 <- (train_table2[1,2]+train_table2[2,1])/sum(train_table2)
print(train_error_rate2)
# Train error rate is 16.6%
# Test:
test_table2 <- table(test$svc_pred_opt, test$Purchase)
test_error_rate2 <- (test_table2[1,2]+test_table2[2,1])/sum(test_table2)
print(test_error_rate2)
# Test error rate is 17.8%

# Part f: Repeat Parts b-e using an SVM with the RBF and 
# default value of gamma:
svm_fit_rad <- svm(Purchase~., data=train, kernel="radial", cost=0.01)
summary(svm_fit_rad)
# This has 607 sv's, which indicates a more complex SVM than the
# one trained by the SVC with the same cost function.  Is it correctly
# picking up on nonlinear functions in the data?
# Determine train and test error rates:
train$svm_rad_pred <- predict(svm_fit_rad, train)
test$svm_rad_pred  <- predict(svm_fit_rad, test)
# Train:
train_table_rad <- table(train$svm_rad_pred, train$Purchase)
train_err_rt_rad <- (train_table_rad[1,2]+train_table_rad[2,1])/sum(train_table_rad)
print(train_err_rt_rad)
# Train error rate is 37.8%
# Test:
test_table_rad <- table(test$svm_rad_pred, test$Purchase)
test_err_rt_rad <- (test_table_rad[1,2]+test_table_rad[2,1])/sum(test_table_rad)
print(test_err_rt_rad)
# Test error rate is 42.6%
# These train and test error rates using 0.01 for cost of SVM with RBF
# are really bad: much worse than SVC.
# Try other values of cost:
set.seed(1)
tune.out_rad <- tune(svm, Purchase~., data=train, kernel="radial", 
                 ranges=list(cost=c(0.01, 0.1, 1, 5, 10)))
summary(tune.out_rad)
# We see large improvement, and an optimal value of cost of 0.1,
# which yields the same 16.6% CV error as opt SVC did.
# Lastly, compute training and test error rates:
set.seed(1)
svm_fit_opt_r <- svm(Purchase~., data=train, kernel="radial", cost=0.1)
summary(svm_fit_opt_r)
# Predict:
train$svm_pred_opt_r <- predict(svm_fit_opt_r, train)
test$svm_pred_opt_r  <- predict(svm_fit_opt_r, test)
# Train:
train_table3 <- table(train$svm_pred_opt_r, train$Purchase)
train_error_rate3 <- (train_table3[1,2]+train_table3[2,1])/sum(train_table3)
print(train_error_rate3)
# Train error rate is 16.5%
# Test:
test_table3 <- table(test$svm_pred_opt_r, test$Purchase)
test_error_rate3 <- (test_table3[1,2]+test_table3[2,1])/sum(test_table3)
print(test_error_rate3)
# Test error rate is 17.8%, same as SVC.

# Part g: Repeat Parts b-e using an SVM with the polynomial kernel and 
# degree = 2:
svm_fit_poly <- svm(Purchase~., data=train, kernel="polynomial", degree=2, cost=0.01)
summary(svm_fit_poly)
# This has 610 sv's, which indicates a more complex SVM than the
# one trained by the SVC with the same cost function.  Is it correctly
# picking up on nonlinear functions in the data?
# Determine train and test error rates:
train$svm_poly_pred <- predict(svm_fit_poly, train)
test$svm_poly_pred  <- predict(svm_fit_poly, test)
# Train:
train_table_poly <- table(train$svm_poly_pred, train$Purchase)
train_err_rt_poly <- (train_table_poly[1,2]+train_table_poly[2,1])/sum(train_table_poly)
print(train_err_rt_poly)
# Train error rate is 37.8%
# Test:
test_table_poly <- table(test$svm_poly_pred, test$Purchase)
test_err_rt_poly <- (test_table_poly[1,2]+test_table_poly[2,1])/sum(test_table_poly)
print(test_err_rt_poly)
# Test error rate is 42.6%, same as RBF.
# These train and test error rates using 0.01 for cost of SVM with poly
# are really bad: much worse than SVC.
# Try other values of cost:
set.seed(1)
tune.out_poly <- tune(svm, Purchase~., data=train, kernel="polynomial", 
                     degree=2, ranges=list(cost=c(0.01, 0.1, 1, 5, 10)))
summary(tune.out_poly)
# We see large improvement, and an optimal value of cost of 1,
# which yields a 16.3% CV error, slightly better than opt SVC did.
# Lastly, compute training and test error rates:
set.seed(1)
svm_fit_opt_p <- svm(Purchase~., data=train, kernel="polynomial", degree=2, cost=1)
summary(svm_fit_opt_p)
# Predict:
train$svm_pred_opt_p <- predict(svm_fit_opt_p, train)
test$svm_pred_opt_p  <- predict(svm_fit_opt_p, test)
# Train:
train_table4 <- table(train$svm_pred_opt_p, train$Purchase)
train_error_rate4 <- (train_table4[1,2]+train_table4[2,1])/sum(train_table4)
print(train_error_rate4)
# Train error rate is 15.8%
# Test:
test_table4 <- table(test$svm_pred_opt_p, test$Purchase)
test_error_rate4 <- (test_table4[1,2]+test_table4[2,1])/sum(test_table4)
print(test_error_rate4)
# Test error rate is 17.8%, same as SVC.

# Part h: Overall, which approach seems to give the best results
# on this data?
# All 3 approaches (SVC, RBF, and polynomial of degree=2) give the
# same test error, which we are most concerned about.  Thus by this
# criterion, we would say they are all equal.  However, keeping in mind
# the principle of parsimony, I would say the SVC (which is the simplest
# model here) gives the best results, as it gives test accuracy which
# is equal to the other two approaches, but with a simpler model.
