
college <- read.csv("U:/My Documents/Desktop/5. Modelling/26. Intro to Stat Learning/Chapter 2/College.csv")
head(college)
fix(college)
rownames(college) <- college[,1]
college <- college[,-1]

summary(college)
pairs(college[,1:10])
pairs(college[,2:4])
plot(college$Private, college$Outstate)

attach(college)
college$Elite <- rep("No", nrow(college))
college$Elite[Top10perc>50]="Yes"
head(college, 20)
Elite <- as.factor(Elite)
summary(Elite)
plot(Elite, Outstate)

par(mfrow=c(2,2))
hist(Outstate)
hist(Apps)
hist(Accept)
hist(Enroll)
