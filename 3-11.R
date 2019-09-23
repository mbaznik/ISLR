
rm(list=ls())

set.seed(1)
x<-rnorm(100)
y<-2*x+rnorm(100)

#a
m1 <- lm(y~0+x)
summary(m1)
# est=1.99, se=.1065, t-stat=18.7, p-value=<2e-16.
# This appears as if the coef is very stat sig at even the 1% leve.

#b
m2 <- lm(x~y+0)
summary(m2)
# est=.3911, se=.0289, t-stat=18.7, p-value=<2e-16.
# This also appears very stat sig, even at the 1% level.

#c
# Same t value and stat sig regressing y on x as x on y.

#d - paper
p1 <- sqrt(length(y)-1)*sum(x*y)
p2 <- sqrt( sum(x^2)*sum(y^2)-(sum(x*y))^2 )
p1/p2
# It is same as above output from lm function.
           
#e - clearly the two t-stats have the same formula.

#f
m3 <- lm(y~x)
m4 <- lm(x~y)
summary(m3)
summary(m4)
# Clearly the two t-stats are the same.