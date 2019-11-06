
rm(list=ls())
library(ISLR)
library(stats)
library(boot)

# a. 
fix(Weekly)
m1 <- glm(Direction~Lag1+Lag2, data=Weekly, family="binomial")

# b. 
m2 <- glm(Direction~Lag1+Lag2, data=Weekly, subset=2:nrow(Weekly), family="binomial")

# c.
pred2 <- predict(m2, newdata=Weekly[1,2:3], type="response")
# Since pred2>0.5, this model predicts Up, which is incorrect since the stock market
# actually went down.

# d.
results <- c()
for (i in 1:nrow(Weekly))
{
  model <- glm(Direction~Lag1+Lag2, data=Weekly[-i,], family="binomial")
  pred <- predict(model, newdata=Weekly[i,2:3], type="response")
  temp <- ifelse(pred>=0.5, 1, 0)
  results <- rbind(results, temp)
}
fix(results)
Weekly$results_ud <- ifelse(results==1, "Up", "Down")
cm <- table(Weekly$results_ud, Weekly$Direction)
error_rate <- (cm[1,2]+cm[2,1])/sum(cm)
print(error_rate)
# 45% error rate.  This is not a great result (or surprising, as the stock market
# is hard to predict). 