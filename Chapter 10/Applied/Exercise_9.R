
# Exercise 9

# Options/Packages:
rm(list=ls())
library(ISLR)

# Data
# Unscaled:
print(head(USArrests))
# Scaled:
USArrests_scale <- scale(USArrests)
print(head(USArrests_scale))
# Perfect.

# Part a: HClust using unscaled data.
# Data distances:
data.dist <- dist(USArrests)
# Cluster using Euclidean distance and complete linkage:
hc_un_compl <- hclust(data.dist, method="complete")
# Plot:
state_names <- row.names(USArrests) 
plot(hc_un_compl, labels=state_names)

# Part b: Cut dendogram at height to obtain 3 clusters.
num_clus <- 3
hc_un_compl_clusters <- cutree(hc_un_compl, num_clus)
# Which states in each?
part_b <- table(state_names, hc_un_compl_clusters)
temp <- data.frame(part_b)
temp2 <- temp[temp$Freq>0,]
print(temp2)
# States in CL1:
cl1 <- temp2[temp2$hc_un_compl_clusters==1,"state_names"]
print(cl1)
# Alabama        Alaska         Arizona        California    
# Delaware       Florida        Illinois       Louisiana     
# Maryland       Michigan       Mississippi    Nevada        
# New Mexico     New York       North Carolina South Carolina
# States in CL2:
cl2 <- temp2[temp2$hc_un_compl_clusters==2,"state_names"]
print(cl2)
# Arkansas      Colorado      Georgia       Massachusetts Missouri     
# New Jersey    Oklahoma      Oregon        Rhode Island  Tennessee    
# Texas         Virginia      Washington    Wyoming
# States in CL3:
cl3 <- temp2[temp2$hc_un_compl_clusters==3,"state_names"]
print(cl3)
# Connecticut   Hawaii        Idaho         Indiana       Iowa         
# Kansas        Kentucky      Maine         Minnesota     Montana      
# Nebraska      New Hampshire North Dakota  Ohio          Pennsylvania 
# South Dakota  Utah          Vermont       West Virginia Wisconsin

# Part c: HClust using scaled data.
data.dist_sc <- dist(USArrests_scale)
# Cluster this scaled data using Euclidean distance and complete linkage:
hc_sc_compl <- hclust(data.dist_sc, method="complete")
plot(hc_sc_compl, labels=state_names)
# Examine clusters:
hc_sc_compl_clusters <- cutree(hc_sc_compl, num_clus)
# Which states in each?
part_c <- table(state_names, hc_sc_compl_clusters)
temp3 <- data.frame(part_c)
temp4 <- temp3[temp3$Freq>0,]
print(temp4)
# States in CL1:
cl1_ <- temp4[temp4$hc_sc_compl_clusters==1,"state_names"]
print(cl1_)
# Alabama  Alaska  Georgia  Louisiana  Mississippi  North Carolina
# South Carolina  Tennessee
# States in CL2:
cl2_ <- temp4[temp4$hc_sc_compl_clusters==2,"state_names"]
print(cl2_)
# Arizona    California Colorado   Florida    Illinois  
# Maryland   Michigan   Nevada     New Mexico New York  Texas
# States in CL3:
cl3_ <- temp4[temp4$hc_sc_compl_clusters==3,"state_names"]
print(cl3_)
# Arkansas      Connecticut   Delaware      Hawaii       
# Idaho         Indiana       Iowa          Kansas       
# Kentucky      Maine         Massachusetts Minnesota    
# Missouri      Montana       Nebraska      New Hampshire
# New Jersey    North Dakota  Ohio          Oklahoma     
# Oregon        Pennsylvania  Rhode Island  South Dakota 
# Utah          Vermont       Virginia      Washington   
# West Virginia Wisconsin     Wyoming

# Part d
# Scaling the variables causes a less even distribution of the number
# of states in each cluster, relative to unscaled.
# Which is better?  Let's look at the clusters:
USArrests[cl1,]
USArrests[cl1_,]
# Clearly the unscaled clustering cl1 groups states together based mainly
# on similarity of their "Assault" values.  This may be undesirable, since the
# values of "Murder" and other features are quite variable/heterogenous.
# The scaled clustering cl1_ groups states together based on a more holistic/even
# consideration of all features, and thus while Assault values are more variable,
# values of Murder, etc. are less variable.
# In my opinion, the values should be scaled before performing this clustering.
# This is because clustering scaled variables allows the clustering to develop
# a more holistic perspective of the relationships in the data based on more/all
# features, rather than allowing just one or two features with high variances
# to dominate the clustering outcome.

