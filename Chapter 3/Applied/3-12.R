
# a.
# Reg of y on x without an intercept, equation for beta hat is:
# b = sum(x_i * y_i)/sum(x_i ^2)
# Reg of x on y without an intercept, equation for beta hat 2 is:
# b2 = sum(x_i * y_i)/sum(y_i ^2)
# Clearly when sum(x_i ^2)=sum(y_i ^2), these two estimators will be equal.

# b.
rm(list=ls())
y <- seq(1,100,by=1)
x <- seq(101,200,by=1)
lm1 <- lm(y~0+x)
summary(lm1)
lm2 <- lm(x~0+y)
summary(lm2)
# Clearly 0.35912 != 2.49254, so two estimates are different.

# c.
rm(list=ls())
y <- seq(1,100,by=1)
x <- seq(100,1,by=-1)
lm1 <- lm(y~0+x)
summary(lm1)
lm2 <- lm(x~0+y)
summary(lm2)
# Clearly 0.5075=0.5075, so two estimates are the same.
sum(x^2)==sum(y^2)
# Two sum of squares are equal.