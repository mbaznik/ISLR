
rm(list=ls())
data()
library(ISLR)
library(stats)

# a-b. 
fix(Default)
set.seed(1)
train_ind <- rep(0, nrow(Default))
train_ind <- ifelse(runif(nrow(Default))>0.7, 0, 1)
train <- Default[train_ind==1,]
test <- Default[train_ind==0,]
m1 <- glm(default~income+balance, data=train, family="binomial")
pred1 <- predict(m1, newdata=test, type="response")
pred1_binary <- rep("No", length(pred1))
pred1_binary[pred1>0.5] <- "Yes"
t1 <- table(pred1_binary,test$default)
val_error <- (t1[1,2]+t1[2,1])/sum(t1)
print(val_error)
# 2.5%


# c.
train_perc <- c(0.5,0.6,0.7,0.8)
val_error_vec <- c()
for (i in 1:length(train_perc))
{
  set.seed(i)
  train_ind <- rep(0, nrow(Default))
  train_ind <- ifelse(runif(nrow(Default))>train_perc[i], 0, 1)
  train <- Default[train_ind==1,]
  test <- Default[train_ind==0,]
  m1 <- glm(default~income+balance, data=train, family="binomial")
  pred1 <- predict(m1, newdata=test, type="response")
  pred1_binary <- rep("No", length(pred1))
  pred1_binary[pred1>0.5] <- "Yes"
  t1 <- table(pred1_binary,test$default)
  val_error <- (t1[1,2]+t1[2,1])/sum(t1)
  val_error_vec <- rbind(val_error_vec, val_error)  
}
print(val_error_vec)
# clearly using 70% as train percentage appears to give lowest 
# test error rate.

# d.
fix(Default)
Default$student_dummy <- ifelse(Default$student=="No",0,1)
set.seed(1)
train_ind <- rep(0, nrow(Default))
train_ind <- ifelse(runif(nrow(Default))>0.7, 0, 1)
train <- Default[train_ind==1,]
test <- Default[train_ind==0,]
m1 <- glm(default~income+balance+student_dummy, data=train, family="binomial")
pred1 <- predict(m1, newdata=test, type="response")
pred1_binary <- rep("No", length(pred1))
pred1_binary[pred1>0.5] <- "Yes"
t1 <- table(pred1_binary,test$default)
val_error <- (t1[1,2]+t1[2,1])/sum(t1)
print(val_error)
# Test error is now 2.8%, higher than the prior 2.5% without this dummy variable.
# Since no, this does not lead to a reduction in the test error rate,
# recommend not including this variable.
