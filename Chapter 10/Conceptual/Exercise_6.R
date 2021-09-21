
# Exercise 6

# Part a:
# Saying that the first Principal Component (PC) "explains 10% of 
# the variation" means that the variance of the first PC divided 
# by the total variance of the dataset is 10%.
# Said using mathematical notation, this means that when m=1,
# the value of Equation (10.8) on page page 383 of ISLR (1st Ed)
# is 10% (namely, the PVE=0.1).

# Part b:
# I critique the researcher's idea as follows:
# By taking the data and subtracting off a scaled version of the first principal
# component (pc), the researcher is assuming that:
# (i) The use of machine A or B really does impact the measured gene expression
# (ii) The first pc picks up this impact of machine A vs. B
# (iii) The ONLY THING that the first pc picks up is the believed machine A vs. B impact.
# The 3rd assumption is the most troubling one: how can the researcher know that
# the first pc ONLY picks up the impact of machine A vs. B, as opposed to some linear
# combo of the machine effect and the combined impacts of many other factors?
# The answer is: the researcher can't be sure of this!  And if the researcher is wrong,
# by backing out the first pc, they will also be accidentally backing out the
# impacts of other very important factors!  This may bias the experimental data
# and cause the results of the researcher's 2-sample t-tests to be biased/invalid.
# I suggest a better approach: since we have gene expression measurements whose
# values are believed to be impacted by two factors (machine a/b and treatment/control)
# for which we have the data for each sample, for each gene, I will run a regression
# with the values of the gene expression as the response variable and two binary variables
# as the predictor variables: one for treatment/control and one for machine a/b.
# Then, for each gene, to assess the partial effect of treatment/control, I will look 
# at the p-value on that binary variable to assess statistical significance.

# Part c:
# Simulation Strategy to Demonstrate Superiority of my Idea:
# * I will create data with 1000 genes and 100 samples.
# * The data will be composed of 3 components: a random component, a machine A/B component,
#   and a component for treatment/control.
# * By design, the machine a/b will exhibit a trend with systematically more A's earlier
#   and more B's later.
# * Simply by chance, the treatment/control will exhibit a small linear trend. (in the
#   below example, where t is coded as 1 and control as 0, we will see slighly more
#   control earlier and more treatment later).
# * Thus, there will be a slight correlation between treatment=t and machine=b,
#   and corresponding trends in the data.
# * The first PC will pick up the linear trend in both treatment and machine.
# * Subtracting off a scaled version of the 1st PC will remove most of the machine
#   impact as well as some of the incidental treatment control impact.
# * The treatment impact that remains in the data will be smaller, and thus will 
#   not be detected as strongly/as often as it would be in my idea.
# * Thus, given that I will parameterize this simulation with significant machine and
#   treatment impacts, the researcher's idea will not detect (at the 5% level) a significant
#   difference between the treatment and control groups as often as my approach will.
#   Thus my approach has greater statistical power here.
# * Note: As I note below, since there will only be 99 informative PCs (since number of
#   PCs = min(n-1,p) = 99, I will only do t-tests (for the researcher's idea) or 
#   regressions (for my idea) on the first 99 genes, since the researcher's idea
#   can only be applied to modify the first 99 rows of the matrix.  While my approach
#   can be used to modify all 1000 rows/genes (which is another advantage of my approach),
#   here I only look at the first 99 genes to put these 2 approaches on level footing.
rm(list=ls())
# Here, I create an 100 by 1000 matrix, assuming:
# 1. Each col = a gene, each row = a sample.
# 2. The random part of each measurement is distributed normal(mean=0, sd=random_sd).
# 3. Machine B makes all measurements higher by an additive amount mach_b_imp
# 4. Treatment makes all measurements higher by an additive amount treat_imp
# I then take the transpose of this matrix.
# Parms:
# Data matrix dim:
n_gene <- 1000
n_samp <- 100
num_entries <- n_gene*n_samp
# Data properties:
random_sd <- 1.5
mach_b_imp <- 1
treat_imp <- 1

# Step 1: Generate Random Component of Data Matrix:
set.seed(1)
random_vals <- rnorm(num_entries, mean=0, sd=sqrt(random_sd))
print(mean(random_vals)); print(sd(random_vals))
# Good.
noise_mat <- matrix(random_vals, nrow=n_samp, ncol=n_gene)
print(noise_mat[1:5,1:5])
print(dim(noise_mat))
# Good.

# Step 2: Generate a matrix for A/B impacts:
# Determine which of the 100 samples are Machine A/B:
# Helper vec:
ab_vec <- c('a', 'b')
# Need to make a more likely early, and b more likely later:
base_seq <- seq(1, n_samp, by=1)
b_prb <- 0.1+(base_seq)/(n_samp)*0.8
# Plot as sanity check:
plot(base_seq, b_prb)
# Good.
ab_ind <- data.frame(matrix('x', nrow=1, ncol=n_samp))
print(ab_ind)
# Populate:
set.seed(4)
for (j in 1:n_samp)
{
  konst <- b_prb[j]
  baz_probs <- c((1-konst), konst)
  ab_ind[1,j] <- sample(ab_vec, 1, replace=TRUE, prob=baz_probs)
}
# Sanity check:
ab_ind_num <- ifelse(ab_ind=='b', 1, 0)
plot(base_seq, ab_ind_num)
ab_ind_num[1:10]
length(ab_ind_num)    
# Put into a matrix:
ab_matrix <- matrix(ab_ind_num, nrow=n_samp, ncol=1)
ab_matrix_full <- matrix(0, nrow=n_samp, ncol=n_gene)
dim(ab_matrix_full)  
ab_matrix_full[,1:n_gene] <- ab_matrix 
print(ab_matrix_full[1:10,1:10])
# And multiply by machine B impact:
ab_matrix_full <- ab_matrix_full*mach_b_imp
print(ab_matrix_full[1:10,1:10])

# Step 3: Generate a matrix for T/C impacts:
# Helper vec:
tc_vec <- c('c', 't')
set.seed(1)
tc_ind <- sample(tc_vec, n_samp, replace=TRUE)
length(tc_ind)
# Sanity check:
t_ind_num <- ifelse(tc_ind=='t', 1, 0)
length(t_ind_num)
t_ind_num[1:10]
print(mean(t_ind_num))
# Put into a matrix:
tc_matrix <- matrix(t_ind_num, nrow=n_samp, ncol=1)
tc_matrix_full <- matrix(0, nrow=n_samp, ncol=n_gene)
dim(tc_matrix_full)  
tc_matrix_full[,1:n_gene] <- tc_matrix 
print(tc_matrix_full[1:10,1:10])
# And multiply by machine B impact:
tc_matrix_full <- tc_matrix_full*treat_imp
print(tc_matrix_full[1:10,1:10])
plot(1:n_samp, t_ind_num)
# Run a regression to assess a trend, if any:
reg_helper <- treat_imp*t_ind_num
print(reg_helper[1:5])
print(length(reg_helper))
reg_helper_feat <- 1:n_samp
print(length(reg_helper_feat))
m1 <- lm(reg_helper~reg_helper_feat)
summary(m1)
ls(m1)
# Plot the data and trendline:
dim(tc_matrix_full)
helper_seq <- 1:100
plot(helper_seq, tc_matrix_full[,1])
lines(m1$fitted.values)
# Print range of fitted values:
print(summary(m1$fitted.values))
# This goes from 0.42-0.60.  This increase of 0.18 will certainly
# "muddy the waters" from the machine impact.

# Step 4: Combine the 3 Component Matrices
data_comb <- (noise_mat + ab_matrix_full + tc_matrix_full)
print(data_comb[1:10,1:10])
print(dim(data_comb))

# Step 5: First, implement the researcher's idea:
# Apply PCA:
r_pc <- prcomp(data_comb, center=TRUE, scale=TRUE)
ls(r_pc)
r_pc_var <- (r_pc$sdev)^2
r_pc_var_vec <- r_pc_var/sum(r_pc_var)
plot(r_pc_var_vec)
r_pc_var_vec[1:5]
# The first PC is much larger than the others.  This is what I 
# was hoping for.  Namely, it appears the random TC impact and 
# the systematic A/B impact are picked up in 1 PC.
# Set up replacement matrix:
pre_repl <- t(data_comb)
print(dim(pre_repl))
# Here I implement the researcher's idea via matrix math,
# instead of scalar math:
# Dimensions:
dim(data_comb)      # Data     = 100 by 1000
dim(r_pc$rotation)  # Loadings = 1000 by 100
dim(r_pc$x)         # PCs      = 100 by 100
# Row Vector of loading vals:
load_mat <- t(as.matrix(r_pc$rotation[1,]))
dim(load_mat)
# Col Vector of first PC:
pc1_mat <- as.matrix(r_pc$x[,1])
dim(pc1_mat)
# Here we compute approx of data using only 1st PC:
pc1_approx <- pc1_mat %*% load_mat
dim(pc1_approx)
# As expected, this has dimensions 100 by 100.
# Note that (as ISLR Chapter 10 mentions), the number of PCs is
# min(n-1,p), so here we only have 99 real PCs, not 1000.
# Thus the loadings matrix is only 1000 by 100, not 1000 by 1000.
# Thus the PC matrix as well is only 100 by 100, instead of 100 by 1000.
# Replace the entries to subtract off the first PC:
# Create a matrix repl.  Initialize it with pre_repl.
repl <- pre_repl
dim(repl)
# Both repl and pre_repl are 1000 by 100.
# Create a helper that resolves to 99, not 1000.
n_gene_helper <- (n_samp-1)
# Modify the first 99 rows:
repl[1:n_gene_helper,] <- pre_repl[1:n_gene_helper,] - pc1_approx[1:n_gene_helper,]
# Inspect:
print(pre_repl[99:100,1:5]); print(repl[99:100,1:5])
# Good.
# Perform PCA on the data which has been adjusted for the 
# researcher's idea:
r_pc2 <- prcomp(t(repl[1:n_gene_helper,]), center=TRUE, scale=TRUE)
ls(r_pc2)
r_pc_var2 <- (r_pc2$sdev)^2
r_pc_var_vec2 <- r_pc_var2/sum(r_pc_var2)
plot(r_pc_var_vec2)
r_pc_var_vec2[1:5]

# Step 6: Next, test the researcher's idea.
# For each gene, let's see how many (of the 99 genes whose data
# we adjusted using the researcher's idea) indicate a stat sig difference
# between T and C at the 5% level:
# Augment data with T or C ind:
data_aug <- as.data.frame(rbind(repl, t_ind_num))
dim(data_aug)
data_aug[998:1001, 1:5]
# Loop:
counter_res <- 0
for (k in 1:n_gene_helper)
{
  # For each gene:
  # Get T:
  temp_t <- as.numeric(c(data_aug[k, data_aug[(n_gene+1),]==1])) 
  # Get C:
  temp_c <- as.numeric(c(data_aug[k, data_aug[(n_gene+1),]==0]))
  # T-test:
  baz_ttest <- t.test(temp_t, temp_c)
  if(baz_ttest$p.value<0.05)
  {
    counter_res <- counter_res+1
  }
}
print(counter_res)
# Indicates 85/99 are statistically significantly different
# at the 5% level.

# Step 7: Lastly, test my idea:
dim(data_comb)
baz_pre <- t(data_comb)
# Combine gene measurements with indicators for ab_machine and tc_ind:
baz_idea_aug <- as.data.frame(rbind(baz_pre, t_ind_num, ab_ind_num))
dim(baz_idea_aug)
baz_idea_aug_t <- as.data.frame(t(baz_idea_aug))
# Create vectors with the two indicators, to be used as features:
x_tc_ind     <- as.factor(c(baz_idea_aug_t[,1001]))
table(x_tc_ind)
x_ab_ind     <- c(baz_idea_aug_t[,1002])
table(x_ab_ind)
# Loop thru to tabulate # of times tc_ind is stat sig:
counter_baz <- 0
for (i in 1:n_gene_helper)
{
  # Grab appropriate column:
  y <- c(baz_idea_aug_t[,i])
  # And wrap up with features as df:
  temp_data    <- cbind(y, x_tc_ind, x_ab_ind)
  temp_data_df <- as.data.frame(temp_data) 
  # Run reg:
  baz_reg1 <- lm(y~x_tc_ind+x_ab_ind, data=temp_data_df)
  # Extract p-value for tc term:
  pval_temp <- summary(baz_reg1)$coefficients[2,4]
  # Increment counter_baz if less than 0.05
  baz_incr    <- ifelse(pval_temp<0.05, 1, 0)
  counter_baz <- (counter_baz + baz_incr)
}
print(counter_baz)
# 93/99 times.
print(counter_res/n_gene_helper); print(counter_baz/n_gene_helper)
# Thus my approach correctly rejects the null of no treatment/control impact
# (and thus arrives at the conclusion of a difference between treatment/control groups)
# about 94% of the time, instead of the researchers' approach's of about 86%.
# Thus this indicates the superiority of my approach.
