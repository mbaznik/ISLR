
# Exercise 7

# Options/Packages:
rm(list=ls())
library(ISLR)

# Data
summary(USArrests)
print(head(USArrests))
class(USArrests)
dim(USArrests)

###########################################################
# Part 1: Correlation Applied to Scaled Obs               #
###########################################################
# Data Preprocessing:
# Transpose (as we want each obs to be mean=0, std=1):
USArrests_transpose <- t(USArrests)
print(head(USArrests_transpose))
# And scale:
USArrests_transpose_sc <- scale(USArrests_transpose)
# Trust, but verify (one):
# Use helper variables:
wy_mean <- mean(USArrests_transpose_sc[,'Wyoming'])
wy_sd <- sd(USArrests_transpose_sc[,'Wyoming'])
print(wy_mean); print(wy_sd) 
# Good.
# Lastly, take the transpose again:
USArrests_cor_final <- t(USArrests_transpose_sc)
print(head(USArrests_cor_final))

# Data Processing:
# Take the pairwise correlation between all pairs of obs.
part1_storage <- c(0, 0, 0)
part1_nrow <- nrow(USArrests_cor_final)
for (i in 1:(part1_nrow-1))
{
  for (j in (i+1):part1_nrow)
  {
    first <- USArrests_cor_final[i,]
    second <- USArrests_cor_final[j,]
    temp_val <- cor(first, second)
    temp_vec <- c(i,j,temp_val)    
    part1_storage <- rbind(part1_storage, temp_vec)
  }
}
# Delete dummy row:
part1_storage <- part1_storage[-1,]
print(head(part1_storage))
# Good.
# And create a col for (1-cor):
part1_storage <- cbind(part1_storage, 1)
print(head(part1_storage))
temp_col <- (part1_storage[,4]-part1_storage[,3]) 
part1_storage <- cbind(part1_storage, temp_col)
print(head(part1_storage))
# Number of rows should be 50choose2, so 50*49/2:
print(nrow(part1_storage))
baz_val <- 50*49/2
print(baz_val)
# Perfect.

###########################################################
# Part 2: Euclidean Distance Between Obs                  #
###########################################################
# Data Processing:
# Take the pairwise Euclidean Distance between all pairs of obs:
part2_storage <- c(0, 0, 0)
part2_nrow <- nrow(USArrests_cor_final)
# Compute distance matrix:
part2_dist <- dist(USArrests_cor_final)
print(part2_dist)
# The dist function outputs a 2x2 object
# (which is not quite a matrix), but is indexed
# like a vector.  Here I grab those vals:
baz_part2_vals <- part2_dist
length(baz_part2_vals)
# 1225, perfect.
# Lastly, I need to squre these:
baz_part2_final <- baz_part2_vals^2
# Print the first few:
print(baz_part2_final[1:5])
# Good.

###########################################################
# Part 3: Demo Proportionality of Part 1 and Part 2       #
###########################################################
plot(part1_storage[,5], baz_part2_final, 
     xlab='(1-Cor)', ylab='Square of Euclidean Dist')
# The fact that the (1-correlations between scaled obs) (horiz axis)
# lie perfectly on the line when plotted against the squared 
# Euclidean distances (vertical axis) demonstrates the proportionality
# between these two quantities.

