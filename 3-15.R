
rm(list=ls())
library(MASS)
head(Boston)
attach(Boston)

# a.
# Parms
len <- (ncol(Boston))
storage <- c()

for (j in 2:len)
{
  m1 <- lm(crim~data.frame(Boston)[,j] )
  storage <- rbind(storage,m1$coef[2])
}

# For 
m1 <- lm(crim~data.frame(Boston)[,5] )
summary(m1)
# Yes, there is positive stat sig relationship between zn and crim @ 1% level.
plot(nox,crim)
abline(m1)
# There appears to be a positive sloping line between nox and crime.
cor(nox,crim)
# Pearson's correlation is 0.42
cor.test(nox, crim, method="spearman")
# Spearman's correlation is 0.82.

# b. 
m2 <- lm(crim~., data=Boston)
summary(m2)
# Results are a multiple regression with some variables having postive 
# and others having negative estimated effects.
# At the 5% level, we can reject the null for intercept, zn, dis, rad,
# black and medv. 
# Nox was individually stat sig, but now is no longer stat sig with 
# the inclusion of the other variables.

# c.
plot(storage, m2$coef[2:14])

# d.
# For simplicity, I just do this for nox:
m3 <- lm(crim~nox+I(nox^2)+I(nox^3))
summary(m3)
# Yes, at the 1% level, each of the intercept, the nox and nox^2 and
# nox^3 terms are all stat sig, so this is strong evidence of a 
# nonlinear relationship.