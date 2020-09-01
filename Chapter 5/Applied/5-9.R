
rm(list=ls())
library(MASS)
library(boot)

# a.
fix(Boston)
attach(Boston)
mu_hat <- mean(medv)
print(mu_hat)

# b.
mu_hat_se <- sd(medv)/sqrt(nrow(Boston))
print(mu_hat_se)

# c.
boot.fn <- function(data, index) return(mean(data[index]))
set.seed(1)
mean_storage <- c()
final <- boot(medv, boot.fn, R=100000)
print(final)
0.4096424/0.4081785-1
# Bootstrapped Std Error is 0.36% larger than ideal.

# d. CI
ci <- rep(mu_hat,2)+c(-2*0.4096424, 2*0.4096424)
# 21.71352 23.35209 vs.
t.test(medv)
# 21.72953 23.33608
# Clearly bootstrapped CI is slightly larger.

# e.
median(final$t)
# 22.5332

# f.
boot.median.fn <- function(data, index) return(median(data[index]))
set.seed(1)
final <- boot(medv, boot.median.fn, R=100000)
print(final)
# 0.3774977; thus std error of median smaller than of the mean.
# Not surprising, since median is less sensitive to outliers than mean,
# so median's std error will be smaller.

# g.
quantile(medv, probs=c(0.1))
# 12.75

# h.
boot.tenth.fn <- function(data, index) return(quantile(data[index],0.1))
final <- boot(medv, boot.tenth.fn, R=100000)
print(final)
# 0.5025573, which is larger than the std error of each of the mean and 
# and the median.  This makes sense, as depending on how many obs are in
# each replicate, this can make the 10th percentile swing more wildly than
# either the mean or median.

