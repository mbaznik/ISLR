
# Exercise 11

# Packages/Options:
rm(list=ls())
library(ISLR)

# Part a:
# Ingest data:
setwd('/Users/a085278/Desktop')
data <- read.csv('Ch10Ex11.csv', header=F)
print(head(data))
dim(data)
# Transpose so that each sample is staged in a row:
data_t <- t(data)
print(data_t[1:5, 1:5])
print(dim(data_t))
# Good.

# Part b: 
# Apply hierarchichal clustering using correlation-based distance:
# Let's see the sd of each gene:
sds <- apply(data_t, 2, sd)
summary(sds)
# The differences in sd are only modest, as min=0.65 and 
# max=1.78.  I will scale nonetheless.
# First scale data to put features on same footing:
data_t_scale <- scale(data_t)
# Compute the correlation between obs:
data_t_scale_t <- t(data_t_scale)
dim(data_t_scale_t)
# 1000 by 40.  40 cols = 40 samples, so cor() will provide
# 40 by 40 matrix of correlations between the samples=obs.
data.dist <- cor(data_t_scale_t)
dim(data.dist)
print(data.dist[1:5,1:5])
# Utilize helper variable to get 1-cor:
help_the_cor <- as.dist(1-data.dist)
dim(help_the_cor)
# Good.
# Cluster using correlation-based distance and complete linkage:
clust_complete <- hclust(help_the_cor)
# And plot the dendogram:
plot(clust_complete, main="HClust: Complete Linkage")
# Yes, the genes do seperate the genes into the two groups,
# at least for complete linkage.
# Does the answer depend on type of linkage used?
# Let's try Single Linkage:
clust_single <- hclust(help_the_cor, method='single')
# Plot corresponding dendogram:
plot(clust_single, main="HClust: Single Linkage")
# Single linkage gives same outcome as complete.
# Try Average Linkage:
clust_average <- hclust(help_the_cor, method='average')
plot(clust_average, main="HClust: Average Linkage")
# Average Linkage still gives same outcome.
# Thus among the 3 types I linkage with which I experimented,
# the answer does not change when different types of linkage used.

# Part c:
# Goal: Find out which genes differ the most across the 2 groups
# Suggestion: 
# 1. First standardize the genes, so that each has mean 0 and
#   sd=1. (DONE=data_t_scale)
# 2. Second, for each gene, take its sample average
#   in each of the 2 groups.
# 3. Lastly, rank the differences between the 2 groups' means to see
#   which genes differ the most across the 2 groups.
dim(data_t_scale)
is.matrix(data_t_scale)
healthy <- data_t_scale[1:20,]
diseased <- data_t_scale[21:40,]
print(nrow(data_t_scale))
# For each group, take average of each gene:
healthy_avg <- apply(healthy, 2, mean)
length(healthy_avg)
diseased_avg <- apply(diseased, 2, mean)
length(diseased_avg)
# Plot:
plot(healthy_avg, diseased_avg)
# ID ones which are most different:
diff <- as.matrix(abs(healthy_avg-diseased_avg), ncol=1)
head(diff)
length(diff)
# Index:
ind <- seq(1, 1000, by=1)
w_ind <- as.data.frame(cbind(diff, ind))
print(head(w_ind))
# Sort:
sorted <- w_ind[order(w_ind$V1),]
print(tail(sorted))
# If we assume a difference metric of the absolute value between
# the sample average of each genes between the 2 groups,
# then the following 5 genes differ the most between the 2 groups:
# c(502, 589, 600, 590, 565)
