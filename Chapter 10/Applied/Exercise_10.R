
# Exercise 10

# Packages/Options:
rm(list=ls())

# Parms:
# 1. Input:
#   a. Data structure:
num_feat   <- 50
num_class  <- 3
num_ineach <- 20
#   b. Data distribution:
class_dif <- 5
error_sd  <- 1
# 2. Derived:
num_obs <- num_class*num_ineach
num_err <- num_obs*num_feat

# Part a: 
# Generate simulated data of 60 obs (20 in each of 3 classes),
# with 50 features:
# Generate random errors:
set.seed(1)
# Allow each variable to have its own sd:
sd_pool     <- seq(1, 10, by=1)
feature_sds <- sample(sd_pool, size=num_feat, replace=TRUE) 
# Create matrix to house errors:
error_mat <- matrix(0, nrow=num_obs, ncol=num_feat)
set.seed(1)
for (j in 1:num_feat)
{
  error_mat[,j] <- rnorm(num_obs, mean=0, sd=feature_sds[j])
}
print(error_mat[1:5, 1:5])
# Check one or two cols:
print(sd(error_mat[,2])); print(sd(error_mat[,4])) 
# Good.
# And shift classes 2 and 3, relative to 1:
cl_zero_shift <- matrix(0, nrow=num_ineach, ncol=num_feat)
cl_one_shift  <- matrix(1, nrow=num_ineach, ncol=num_feat)
cl_two_shift  <- matrix(2, nrow=num_ineach, ncol=num_feat)
shift_matrix  <- rbind(cl_zero_shift, cl_one_shift, cl_two_shift)
dim(shift_matrix)
scaled_sm     <- class_dif*shift_matrix
summary(scaled_sm)
print(scaled_sm[1:5,1:5])
# Shift + Error:
final_data_mat <- error_mat+scaled_sm
dim(final_data_mat)
print(final_data_mat[,1:5])
# Validate 2 cols:
print(mean(final_data_mat[,2])); print(mean(final_data_mat[,4]))
print(sd(final_data_mat[,2])); print(sd(final_data_mat[,4]))
# Good.

# Part b:
# Perform PCA on the 60 obs:
pc_take1 <- prcomp(x=final_data_mat, center=FALSE, scale=FALSE)
# And plot the first 2 PC score vectors:
col_vec <- c(rep(1, times=20), rep(2, times=20), rep(3, times=20))
plot(pc_take1$x[,1], pc_take1$x[,2], col=col_vec, xlab="PC1", ylab="PC2")

# Part c:
# Perform K-Means clustering on the data with K=3:
kmeans_partc <- kmeans(final_data_mat, centers=3, nstart=50)
ls(kmeans_partc)
kmeans_partc$cluster
table(col_vec, kmeans_partc$cluster)
# Once I consider the fact that kmeans() function
# indexes the clusters differently, these cluster 
# assignments align reasonably well with their true values,
# namely, (57/60) obs classified correctly.

# Part d:
# Perform K-Means clustering on the data with K=2:
kmeans_partd <- kmeans(final_data_mat, centers=2, nstart=50)
table(col_vec, kmeans_partd$cluster)
# These results show this cluster assignment aligns loosely with
# that from Part c.  Namely, all of the true Cluster 1 obs
# are in the 1st estimated cluster, and all the true Cluster 3
# obs are in the 2nd estimated cluster (2nd of 2, that is.)
# 16/20 of the true Cluster 2 obs are assigned to estimated
# cluster 1, and the remainder are assigned to the 2nd
# estimated cluster.
# In short, the true middle cluster is split between the other
# two, while the other two true clusters are assigned all their obs.

# Part e:
# Perform K-Means clustering on the data with K=4:
kmeans_parte <- kmeans(final_data_mat, centers=4, nstart=50)
table(col_vec, kmeans_parte$cluster)
# (19/20) of the true class 1 are assigned to estimated class 1
# (12/20) of the true class 2 are assigned to estimated class 2
# True class 3 are assigned to estimated class 3, and
# the balance assigned to estimated classes 4 and 1.

# Part f:
# Perform K-Means clustering with K=3 on the first 2 PCs of the data:
kmeans_partf <- kmeans(pc_take1$x[,1:2], centers=3, nstart=50)
table(col_vec, kmeans_partf$cluster)
# This approach perfectly classifies all 60 obs into
# the correct clusters.  This is not surprising, as I designed
# the simulated data so this would happen.  Also, PCA's ability
# to distill much of the signal and little of the noise into 
# the first 2 PC's is causing this superior performance.

# Part g:
# Scale the data so each variable has sd=1:
data_scaled <- scale(final_data_mat, center=FALSE, scale=TRUE)
# Perform K-Means clustering with K=3:
kmeans_partg <- kmeans(data_scaled, centers=3, nstart=50)
# Compare results to Part b:
table(col_vec, kmeans_partg$cluster)
# This assigns all but 1 of the obs correctly.
# When comparing to Part b, these results are not surprising.
# The data, by design, is somewhat volatile.  Therefore in Part g,
# while scaling the data does put each of the features on the
# same footing, it uses all 50 features for the clustering, which
# does nothing to protect against overfitting to noise when performing
# the k-means clustering, which is why 1 obs is misclassified.
# In Part b. however, when we use PCA to distill as much of the signal
# as possible into the first 2 PC's, we significantly reduce the
# amount of noise as well as the potential to overfit by using
# only the first 2 PC's.  This is why Part b. correctly
# assigns all the obs.
