
# Chapter 9, Exercise 7
# Packages/options:
rm(list=ls())
# For svm() function
library(e1071)
# For Auto data:
library(ISLR)

# Part a: Create a binary variable that is 1/0 for 
# obs whose gas mileage is above/below the median.
# Data:
head(Auto)
summary(Auto)
# Median for target var:
mpg_median <- median(Auto$mpg)
print(mpg_median)
# Create a copy of the dataset, which I will munge:
Auto_MAB <- Auto
Auto_MAB$high_mpg_ind <- as.factor(ifelse(Auto_MAB$mpg>mpg_median, 1, 0))
# Remove mpg feature:
Auto_MAB_xmpg <- subset(Auto_MAB, select= -c(mpg) )
print(head(Auto_MAB_xmpg)); print(tail(Auto_MAB_xmpg))
# Perfect.

# Part b. Train a SVC to data with various values of cost:
set.seed(1)
tune.out <- tune(svm, high_mpg_ind~., data=Auto_MAB_xmpg, kernel="linear", 
                 ranges=list(cost=c(0.001, 0.01, 0.1, 1, 10, 100)))
summary(tune.out)
# Output:
#   cost      error dispersion
# 1 1e-03 0.13525641 0.05661708
# 2 1e-02 0.08923077 0.04698309
# 3 1e-01 0.08673077 0.04040897 <- 0.1 is lowest CV error
# 4 1e+00 0.09961538 0.04923181
# 5 1e+01 0.11237179 0.05701890
# 6 1e+02 0.11750000 0.06208951
# Store best SVC:
best_svc <- tune.out$best.model
# Predict:  
Auto_MAB_xmpg$best_SVC_pred <- predict(best_svc, newdata=Auto_MAB_xmpg)
# Among the values of the cost hyperparameter that I tried,
# the value of 0.1 yields the lowest CV error of 0.0867  This suggests that a 
# moderate amount of complexity is optimal, which aligns with law of 
# diminishing marginal returns.

# Part c: Repeat Part b., this time with each of:
# RBF kernel with different values of gamma:
set.seed(1)
tune.out_rbf <- tune(svm, high_mpg_ind~., data=Auto_MAB_xmpg, kernel="radial", 
                 ranges=list(
                    cost=c(0.001, 0.01, 0.1, 1, 10, 100),
                    gamma=c(0.5, 1, 2, 3)
                    ))
summary(tune.out_rbf)
# Output:
#     cost gamma      error dispersion
# 1  1e-03   0.5 0.55115385 0.04366593
# 2  1e-02   0.5 0.55115385 0.04366593
# 3  1e-01   0.5 0.08666667 0.04193895
# 4  1e+00   0.5 0.08416667 0.04359608
# 5  1e+01   0.5 0.08923077 0.03843042
# 6  1e+02   0.5 0.09173077 0.03821436
# 7  1e-03   1.0 0.55115385 0.04366593
# 8  1e-02   1.0 0.55115385 0.04366593
# 9  1e-01   1.0 0.55115385 0.04366593
# 10 1e+00   1.0 0.07647436 0.04820737 <- optimal
# 11 1e+01   1.0 0.08153846 0.04773889
# 12 1e+02   1.0 0.08153846 0.04773889
# 13 1e-03   2.0 0.55115385 0.04366593
# 14 1e-02   2.0 0.55115385 0.04366593
# 15 1e-01   2.0 0.55115385 0.04366593
# 16 1e+00   2.0 0.13256410 0.07390123
# 17 1e+01   2.0 0.12743590 0.06892992
# 18 1e+02   2.0 0.12743590 0.06892992
# 19 1e-03   3.0 0.55115385 0.04366593
# 20 1e-02   3.0 0.55115385 0.04366593
# 21 1e-01   3.0 0.55115385 0.04366593
# 22 1e+00   3.0 0.36756410 0.14692411
# 23 1e+01   3.0 0.32685897 0.14742241
# 24 1e+02   3.0 0.32685897 0.14742241
# Store best SVM with RBF Kernel:
best_svm_rbf <- tune.out_rbf$best.model
# Predict:
Auto_MAB_xmpg$best_RBF_pred <- predict(best_svm_rbf, newdata=Auto_MAB_xmpg)
# Here, the optimal values of hyperparameters are cost=1 and gamma=1,
# which correspond to a minimum value of 0.0765, which outperforms the
# SVC optimal of 0.0867 (reduces CV error by about 12%)
# The Polynomial Basis Kernel with different values of degree:
set.seed(1)
tune.out_poly <- tune(svm, high_mpg_ind~., data=Auto_MAB_xmpg, kernel="polynomial", 
                     ranges=list(
                       cost=c(0.001, 0.01, 0.1, 1, 10, 100),
                       degree=c(1, 2, 3, 4)
                     ))
summary(tune.out_poly)
# Output:
#     cost degree      error dispersion
# 1  1e-03      1 0.55115385 0.04366593
# 2  1e-02      1 0.55115385 0.04366593
# 3  1e-01      1 0.24750000 0.10618796
# 4  1e+00      1 0.09179487 0.04693642
# 5  1e+01      1 0.07903846 0.03497759 <- optimal
# 6  1e+02      1 0.07903846 0.03497759
# 7  1e-03      2 0.55115385 0.04366593
# 8  1e-02      2 0.55115385 0.04366593
# 9  1e-01      2 0.55115385 0.04366593
# 10 1e+00      2 0.55115385 0.04366593
# 11 1e+01      2 0.48230769 0.11988539
# 12 1e+02      2 0.23756410 0.11699228
# 13 1e-03      3 0.55115385 0.04366593
# 14 1e-02      3 0.55115385 0.04366593
# 15 1e-01      3 0.55115385 0.04366593
# 16 1e+00      3 0.55115385 0.04366593
# 17 1e+01      3 0.55115385 0.04366593
# 18 1e+02      3 0.40326923 0.10793388
# 19 1e-03      4 0.55115385 0.04366593
# 20 1e-02      4 0.55115385 0.04366593
# 21 1e-01      4 0.55115385 0.04366593
# 22 1e+00      4 0.55115385 0.04366593
# 23 1e+01      4 0.55115385 0.04366593
# 24 1e+02      4 0.55115385 0.04366593
# Store best SVM with Polynomial Kernel:
best_svm_poly <- tune.out_poly$best.model
# Here, the optimal values of hyperparameters are cost=10 and degree=1,
# which correspond to a minimum value of 0.079, which outperforms the 
# SVC but not the SVM with RBF.
# This tells us 2 things:
# 1. The additional flexibility wrt degree of polynomial is not very useful,
#     as the CV error indicates a degree of 1 (like the SVC) is optimal.
# 2. This suggests the relationship between features and target is closer
#     to linear than it is quadratic, etc.
# 3. The fact that the RBF Kernel performs best suggests that it best picks up on
#     minor nonlinearities in the data.

# Part d: Make plots to back up my assertions in b and c:
# In Part b, I asserted that when we restrict the kernel to be
# a linear one, a moderate value of cost is optimal.  I substatiate
# that with the below graph:
plot(best_svc, Auto_MAB_xmpg, weight~horsepower)
# This shows that while the red and black are roughly linearly 
# seperable, there is still some noise/misclassification.
# Thus, a moderate value of cost (which permits some, but not
# extreme levels of misclassification) is optimal here.  A similar
# phenomenon is visible in other cuts.
# In Part c, I asserted that the seperation is close to linear, 
# but not fully linear, which explains why RBF and SVM with 
# polynomial kernel outperform SVC. I substantiate that with the
# below plot:
plot(best_svm_poly, Auto_MAB_xmpg, acceleration~displacement)
# This shows that while a vertical line around displacement=200
# does a good job of separating the two classes, it is not perfect
# as there is nonlinearity between displacement between 200-300.
# Thus SVM with RBF kernel outperforms SVC.

# Appendix 1: Compare training with and without the mpg variable.
# Goal is to understand if excluding it was implied, since it should 
# be able to explain perfectly the mpg binary variable...
# Without:
set.seed(1)
tune.without <- tune(svm, high_mpg_ind~., data=Auto_MAB_xmpg, kernel="linear", 
                 ranges=list(cost=c(0.01, 0.1, 1, 10, 100)))
summary(tune.without)
# With:
set.seed(1)
tune.with <- tune(svm, high_mpg_ind~., data=Auto_MAB, kernel="linear", 
                 ranges=list(cost=c(0.01, 0.1, 1, 10, 100)))
summary(tune.with)
# Yes, presumably we were intented to exclude mpg.

# Appendix 2: Understand the viz:
# Plot svm poly:
plot(best_svm_poly, Auto_MAB_xmpg, weight~horsepower, fill=TRUE)
one <- Auto_MAB_xmpg[Auto_MAB_xmpg$weight<3500 & Auto_MAB_xmpg$horsepower>200,]
print(one)
# actual = 0
# This is a black cross.
second <- Auto_MAB_xmpg[Auto_MAB_xmpg$weight<2400 & Auto_MAB_xmpg$horsepower>110,]
print(second)
# actual = 1
# This is a red cross.
three <- Auto_MAB_xmpg[Auto_MAB_xmpg$weight<2500 & Auto_MAB_xmpg$horsepower>100,]
print(three)
# Thus, color (red/black) symbolizes actual value (1/0), and 
# shape (X/circle) symbolizes if it was a support vector/not a sv.