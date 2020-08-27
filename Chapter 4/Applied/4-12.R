
rm(list=ls())

# a. and b.
Power <- function(base, power)
{
  result <- base^power
  print(result)
}
Power(2,3)
Power(3,8)

# c.
Power(10,3)
Power(8,17)
Power(131,3)

# d.
Power2 <- function(base, power)
{
  result <- base^power
  return(result)
}
x <- Power2(2,3)

# e.
storage <- c()
for (i in 1:10)
{
  temp <- Power2(i,2)
  storage <- rbind(storage, temp)
}
plot(1:10, log(storage), xlim=c(1,10), xlab="x", ylab="ln(x^2)",
     main="Plot of ln(x^2) vs. x")

# f.
PlotPower <- function(base_range, power)
{
  result <- (base_range)^power
  plot(base_range, result)
}
PlotPower(1:10,2)
PlotPower(1:10,0.5)
PlotPower(1:10,5)
# Success!
