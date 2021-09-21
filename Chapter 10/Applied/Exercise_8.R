
# Exercise 8

# Options/Packages:
rm(list=ls())

# Data:
print(head(USArrests))
USArrests_scale <- scale(USArrests)
print(head(USArrests_scale))
# Trust, but verify:
murder_mean <- mean(USArrests_scale[,1])
murder_sd <- sd(USArrests_scale[,1])
print(murder_mean); print(murder_sd)
# Perfect.

# Part a: Use sdev output of prcomp() function:
# Turns out I didn't have to scale after all...
pca_out <- prcomp(USArrests, scale=TRUE)
ls(pca_out)
vars <- (pca_out$sdev)^2
print(vars)
# Derive cumsum:
var_cumsum <- cumsum(vars)
print(var_cumsum)
len <- length(var_cumsum)
print(len)
cum_pve_part_a <- var_cumsum/sum(var_cumsum[len])
print(cum_pve_part_a)
# Cum PVE:
# 0.6200604 0.8675017 0.9566425 1.0000000
# PVE:
two_thru_four <- cum_pve_part_a[2:4]-cum_pve_part_a[1:3]
pve <- c(cum_pve_part_a[1], two_thru_four)
print(pve); print(sum(pve))
# PVE:
# 0.62006039 0.24744129 0.08914080 0.04335752
# Verify I get same answer with homebrewed scaling:
pca_out2 <- prcomp(USArrests_scale, scale=FALSE)
vars2 <- (pca_out2$sdev)^2 
print(vars2)
# I get the same answer.

# Part b:
# Get denominator:
denom_1 <- USArrests_scale^2
print(head(denom_1))
denom_2 <- sum(denom_1) 
print(denom_2)
# Get loadings:
pca_load <- pca_out$rotation
print(pca_load)
# Matrix multiply scaled data times loadings:
foo <- USArrests_scale %*% pca_load
print(head(foo))
# Sum each row:
bar <- apply(foo, 1, sum)
print(head(bar))
# Square:
bar2 <- bar^2
print(head(bar2))
# Sum over all rows to compute numerator:
numer <- sum(bar2)
print(numer)

# Homebrewed function to compute PVE:
hb_pve <- function(x, loadings, index)
{
  step1 <- USArrests_scale %*% pca_load[,index]
  step2 <- apply(step1, 1, sum)
  step3 <- step2^2
  step4 <- sum(step3)
  return(step4)
}
# For each of the 4 PC:
pve1 <- hb_pve(USArrests_scale, pca_load, 1)
pve2 <- hb_pve(USArrests_scale, pca_load, 2)
pve3 <- hb_pve(USArrests_scale, pca_load, 3)
pve4 <- hb_pve(USArrests_scale, pca_load, 4)
pves <- c(pve1, pve2, pve3, pve4)/denom_2
print(pves)
# 0.62006039 0.24744129 0.08914080 0.04335752
# Same as above in a.
