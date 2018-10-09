set.seed(123456)
data10000=rnorm(10000,0,1)
data1000=rnorm(1000,0,1)
data100=rnorm(100,0,1)
data10=rnorm(10,0,1)
data5=rnorm(5,0,1)

library(MASS)
library(boot)
#sample mean
mean(data10000, na.rm=TRUE)
mean(data1000, na.rm=TRUE)
mean(data100, na.rm=TRUE)
mean(data10, na.rm=TRUE)
mean(data5, na.rm=TRUE)

#standard deviation of the sample

sqrt(sum((data5 - mean(data5))^2) / (length(data5) - 1))
sd(data5)


#standard error of the mean
sd(data10000)/sqrt(length(data10000))
sd(data1000)/sqrt(length(data1000))
sd(data100)/sqrt(length(data100))
sd(data10)/sqrt(length(data10))
sd(data5)/sqrt(length(data5))


results <- boot(data=data10, statistic=rsq, 
                R=1000)

