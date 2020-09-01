
rm(list=ls())
library(ISLR)
library(stats)
library(boot)

# a. 
set.seed(1)
x <- rnorm(100)
y <- (x-2*x^2+rnorm(100))
# n=100 and p=2
# Model used to generate the data in equation form:
# y_i = b_0*x_i + b_1*(x_i)^2 + epsilon_i, where epsilon_i is the error.

# b. 
plot(x,y)
data <- data.frame(y,x)
# Clearly a very nonlinear relationship between the two vars, looks
# quite quadratic actually. 

# c.
# i.
set.seed(1)
results <- c()
for (i in 1:nrow(data))
{
  model <- glm(y~x, data=data[-i,])
  pred <- predict(model, newdata=data[i,])
  temp <- (pred-data[i,1])^2
  results <- rbind(results, temp)
}
fix(results)
mse_i <- mean(results)
# ii.
set.seed(1)
results <- c()
for (i in 1:nrow(data))
{
  model <- glm(y~x+I(x^2), data=data[-i,])
  pred <- predict(model, newdata=data[i,])
  temp <- (pred-data[i,1])^2
  results <- rbind(results, temp)
}
fix(results)
mse_ii <- mean(results)
# iii.
set.seed(1)
results <- c()
for (i in 1:nrow(data))
{
  model <- glm(y~x+I(x^2)+I(x^3), data=data[-i,])
  pred <- predict(model, newdata=data[i,])
  temp <- (pred-data[i,1])^2
  results <- rbind(results, temp)
}
fix(results)
mse_iii <- mean(results)
# iv.
set.seed(1)
results <- c()
for (i in 1:nrow(data))
{
  model <- glm(y~x+I(x^2)+I(x^3)+I(x^4), data=data[-i,])
  pred <- predict(model, newdata=data[i,])
  temp <- (pred-data[i,1])^2
  results <- rbind(results, temp)
}
fix(results)
mse_iv <- mean(results)

# d.
# i.
set.seed(2)
results <- c()
for (i in 1:nrow(data))
{
  model <- glm(y~x, data=data[-i,])
  pred <- predict(model, newdata=data[i,])
  temp <- (pred-data[i,1])^2
  results <- rbind(results, temp)
}
fix(results)
mse_i <- mean(results)
# ii.
set.seed(2)
results <- c()
for (i in 1:nrow(data))
{
  model <- glm(y~x+I(x^2), data=data[-i,])
  pred <- predict(model, newdata=data[i,])
  temp <- (pred-data[i,1])^2
  results <- rbind(results, temp)
}
fix(results)
mse_ii <- mean(results)
# iii.
set.seed(2)
results <- c()
for (i in 1:nrow(data))
{
  model <- glm(y~x+I(x^2)+I(x^3), data=data[-i,])
  pred <- predict(model, newdata=data[i,])
  temp <- (pred-data[i,1])^2
  results <- rbind(results, temp)
}
fix(results)
mse_iii <- mean(results)
# iv.
set.seed(2)
results <- c()
for (i in 1:nrow(data))
{
  model <- glm(y~x+I(x^2)+I(x^3)+I(x^4), data=data[-i,])
  pred <- predict(model, newdata=data[i,])
  temp <- (pred-data[i,1])^2
  results <- rbind(results, temp)
}
fix(results)
mse_iv <- mean(results)
# Yes, results are exactly the same.  This is so because there is
# nothing random about this process; all we are doing is deterministically
# leaving out 1 record at a time and fitting the model on the 
# other obs.  No randomness here!

# e.
# Model ii. had the smallest LOOCV error.  Yes this is what I 
# expected, as the true data generating process was a quadratic
# function (plus or minus some noise), thus I would expect 
# the quadratic fit to perform best.  I was surprised that the
# third degree and fourth degree fits were as close as they were
# to the second degree fit.

# f.
# Here I use the whole dataset to train the models:
model1 <- glm(y~x, data=data)
model2 <- glm(y~x+I(x^2), data=data)
model3 <- glm(y~x+I(x^2)+I(x^3), data=data)
model4 <- glm(y~x+I(x^2)+I(x^3)+I(x^4), data=data)
summary(model4)
# Adding each of the squared, cubic, and fourth degree term yielded
# a statistically significant value (at the 10% level) for the 
# leading term, which is a suggestion of statistical significance
# of the model.  Given this result, one may have expected the 
# cross-validated MSE to decrease in each case, but it did not.
