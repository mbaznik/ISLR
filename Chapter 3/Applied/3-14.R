
rm(list=ls())
set.seed(1)

x1 <- runif(100)
x2 <- 0.5*x1+rnorm(100)/10
y <- 2+2*x1+0.3*x2+rnorm(100) 

# a. 
# y_i = b_0 + b_1*x_1 + b_2*x_2 + epsilon_i
# b_0=2, b_1=2, b_2=0.3

# b.
cor(x1,x2)
# Correlation is 0.835
cor.test(x1,x2,method="spearman")
plot(x1,x2)
# Scatterplot shows moderate/strong positive linear relationship
# between the two variables.

# c.
m1 <- lm(y~x1+x2)
summary(m1)
# b_0_hat=2.13 slightly larger than actual 2
# b_1_hat=1.44 noticeably smaller than actual value of 2
# b_2_hat=1.01 much larger than actual value of 0.3
# At 5% level, yes can reject null that b_1=0 (p-value = 0.0487)
# At 5% level, no can not reject the null hypothesis that b_2=0

# d.
m2 <- lm(y~x1)
summary(m2)
# Can reject null of b_1=0 even at 1% level, since p-val=2.66*10^-6

# e.
m3 <- lm(y~x2)
summary(m3)
# Can reject null of b_2=0 even at 1% level, since p-val=1.37*10^-5

# f.
# No, results do not contradict each other.  While each x1 and x2 is 
# marginally stat sig, because there is material dependency between the
# two variables, x2 becomes not stat sig when the two are included,
# since estimating the two precisely becomes more difficult when
# the two correlated variables are included together.

# g.
x1 <- c(x1, 0.1)
x2 <- c(x2, 0.8)
y  <- c(y,6)
# Yes, this is a high leverage point, since it is both the highest value of x2
# as well as unusualy that such a low value of x1 would correspond to 
# a high value of x2.
plot(y)
zscore <- (6-mean(y))/sd(y)
# No, using a threshold of 3 std dev, since this y value is only 2.36 sd
# above its mean, it is not an outlier.
m1 <- lm(y~x1+x2)
summary(m1)
# This makes x2 stat sig and x1 not stat sig now.  
m2 <- lm(y~x1)
summary(m2)
# Yet x1 is marginally very stat sig.
m3 <- lm(y~x2)
summary(m3)
# And x2 is also very marginally stat sig.
# I think the point here is that since x1 and x2 were both stat sig yet
# highly correlated, the estimates are imprecise and volatile.  Thus,
# the model guessed that x1 was stat sig and x2 was not, yet even a 
# small perturbation shifted the estimates so that now x2 looked stat sig
# and x1 did not.  This also shows how high leverage points can affect
# the solution.
par(mfrow=c(2,2))
plot(m1)
