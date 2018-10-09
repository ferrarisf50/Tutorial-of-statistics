seeding=1234567
set.seed(seeding)

data10000 <- rnorm(10000,0,1)
data1000 <- rnorm(1000,0,1)
data100 <- rnorm(100,0,1)
data10 <- rnorm(10,0,1)
data5 <- rnorm(5,0,1)

datalist <- list(data10000,data1000,data100,data10,data5)

library(MASS)
library(boot)
#sample mean

sampling_mean <- numeric(5)
for (i in 1:5) {
  sampling_mean[i] <- mean(datalist[[i]], na.rm=TRUE)
  }

sampling_mean

#standard deviation of the population mean

mean_se <- numeric(5)
for (i in 1:5) {
  mean_se[i] <- 1/sqrt(length(datalist[[i]]))
}

mean_se

#standard error of the mean

sampling_se <- numeric(5)
for (i in 1:5) {
  
  sampling_se[i] <- sd(datalist[[i]])/sqrt(length(datalist[[i]]))
}

sampling_se

# resampling mean



resampling_mean <- numeric(5)
for (i in 1:5) {
  set.seed(seeding)
  dim <- length(datalist[[i]])*10
  resampling_mean[i] <- mean(sample(datalist[[i]],dim,replace=T), na.rm=TRUE)
}

resampling_mean

resampling_se <- numeric(5)
for (i in 1:5) {
  set.seed(seeding)
  dim <- length(datalist[[i]])*10
  data_re=sample(datalist[[i]],dim,replace=T)
  resampling_se[i] <- sd(data_re)/sqrt(length(data_re))
}

resampling_se

#bootstrap

boot10000 <-numeric(1000)
boot1000 <-numeric(1000)
boot100 <-numeric(1000)
boot10 <-numeric(1000)
boot5 <-numeric(1000)

for (i in 1:1000) boot10000[i] <- mean(sample(data10000,replace=T))
for (i in 1:1000) boot1000[i] <- mean(sample(data1000,replace=T))
for (i in 1:1000) boot100[i] <- mean(sample(data100,replace=T))
for (i in 1:1000) boot10[i] <- mean(sample(data10,replace=T))
for (i in 1:1000) boot5[i] <- mean(sample(data5,replace=T))

mean(boot10000)

results <- boot(data=data10, statistic=rsq, 
                R=1000)


