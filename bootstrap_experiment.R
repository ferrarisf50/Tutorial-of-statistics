<<<<<<< HEAD
# Estimate of MSE of mean estimator


library(MASS)
library(boot)

sampling_mean_sum <- numeric(5)
resampling_mean_sum <- numeric(5)


for (i in 1:100) {
  
  set.seed(i)
  data10000 <- rnorm(10000,0,1)
  data1000 <- rnorm(1000,0,1)
  data100 <- rnorm(100,0,1)
  data10 <- rnorm(10,0,1)
  data5 <- rnorm(5,0,1)
  
  datalist <- list(data10000,data1000,data100,data10,data5)
  
  sampling_mean <- numeric(5)
  for (k in 1:5) {
    sampling_mean[k] <- mean(datalist[[k]], na.rm=TRUE)
  }
  sampling_mean_sum=sampling_mean_sum+sampling_mean^2

  
  resampling_mean <- numeric(5)
  for (j in 1:5) {
    set.seed(i)
    dim <- length(datalist[[j]])*10
    resampling_mean[j] <- mean(sample(datalist[[j]],dim,replace=T), na.rm=TRUE)
  }
  
  resampling_mean_sum=resampling_mean_sum+resampling_mean^2 
  
}

sampling_mean_sum
resampling_mean_sum



# exponential distribution

sampling_mean_sum <- numeric(5)
resampling_mean_sum <- numeric(5)

rate=2
for (i in 1:100) {
  
  set.seed(i)
  data10000 <- rexp(10000,rate)
  data1000 <- rexp(1000,rate)
  data100 <- rexp(100,rate)
  data10 <- rexp(10,rate)
  data5 <- rexp(5,rate)
  
  datalist <- list(data10000,data1000,data100,data10,data5)
  
  sampling_mean <- numeric(5)
  for (k in 1:5) {
    sampling_mean[k] <- 1/mean(datalist[[k]], na.rm=TRUE)
  }
  sampling_mean_sum=sampling_mean_sum+(sampling_mean-rate)^2
  
  
  resampling_mean <- numeric(5)
  for (j in 1:5) {
    set.seed(i)
    dim <- length(datalist[[j]])*10
    resampling_mean[j] <- 1/mean(sample(datalist[[j]],dim,replace=T), na.rm=TRUE)
  }
  
  resampling_mean_sum=resampling_mean_sum+(resampling_mean-rate)^2 
  
}


sampling_mean_sum 
resampling_mean_sum 

# Estimate the MSE of the standard error estimator of the mean

sampling_mean_sum <- numeric(5)
resampling_mean_sum <- numeric(5)


data <- runif(100)

fc <- function(x,i){
  
  return(mean(x[i]))
}


set.seed(1)
bootmean <- boot(data,statistic = fc,1)
bootmean
set.seed(1)
mean(sample(data,100,replace=T))
mean(data)


for (i in 1:100) {
  
  set.seed(i)
  data10000 <- rnorm(10000,0,1)
  data1000 <- rnorm(1000,0,1)
  data100 <- rnorm(100,0,1)
  data10 <- rnorm(10,0,1)
  data5 <- rnorm(5,0,1)
  
  datalist <- list(data10000,data1000,data100,data10,data5)
  
  sampling_mean <- numeric(5)
  for (k in 1:5) {
    sampling_mean[k] <- mean(datalist[[k]], na.rm=TRUE)
  }
  sampling_mean_sum=sampling_mean_sum+sampling_mean^2
  
  
  resampling_mean <- numeric(5)
  for (j in 1:5) {
    set.seed(i)
    dim <- length(datalist[[j]])*10
    resampling_mean[j] <- mean(sample(datalist[[j]],dim,replace=T), na.rm=TRUE)
  }
  
  resampling_mean_sum=resampling_mean_sum+resampling_mean^2 
  
}

sampling_mean_sum
resampling_mean_sum


=======
# Estimate of MSE of mean estimator


library(MASS)
library(boot)

sampling_mean_sum <- numeric(5)
resampling_mean_sum <- numeric(5)


for (i in 1:100) {
  
  set.seed(i)
  data10000 <- rnorm(10000,0,1)
  data1000 <- rnorm(1000,0,1)
  data100 <- rnorm(100,0,1)
  data10 <- rnorm(10,0,1)
  data5 <- rnorm(5,0,1)
  
  datalist <- list(data10000,data1000,data100,data10,data5)
  
  sampling_mean <- numeric(5)
  for (k in 1:5) {
    sampling_mean[k] <- mean(datalist[[k]], na.rm=TRUE)
  }
  sampling_mean_sum=sampling_mean_sum+sampling_mean^2

  
  resampling_mean <- numeric(5)
  for (j in 1:5) {
    set.seed(i)
    dim <- length(datalist[[j]])*10
    resampling_mean[j] <- mean(sample(datalist[[j]],dim,replace=T), na.rm=TRUE)
  }
  
  resampling_mean_sum=resampling_mean_sum+resampling_mean^2 
  
}

sampling_mean_sum
resampling_mean_sum



# exponential distribution

sampling_mean_sum <- numeric(5)
resampling_mean_sum <- numeric(5)

rate=2
for (i in 1:100) {
  
  set.seed(i)
  data10000 <- rexp(10000,rate)
  data1000 <- rexp(1000,rate)
  data100 <- rexp(100,rate)
  data10 <- rexp(10,rate)
  data5 <- rexp(5,rate)
  
  datalist <- list(data10000,data1000,data100,data10,data5)
  
  sampling_mean <- numeric(5)
  for (k in 1:5) {
    sampling_mean[k] <- 1/mean(datalist[[k]], na.rm=TRUE)
  }
  sampling_mean_sum=sampling_mean_sum+(sampling_mean-rate)^2
  
  
  resampling_mean <- numeric(5)
  for (j in 1:5) {
    set.seed(i)
    dim <- length(datalist[[j]])*10
    resampling_mean[j] <- 1/mean(sample(datalist[[j]],dim,replace=T), na.rm=TRUE)
  }
  
  resampling_mean_sum=resampling_mean_sum+(resampling_mean-rate)^2 
  
}


sampling_mean_sum 
resampling_mean_sum 

# Estimate the MSE of the standard error estimator of the mean

sampling_mean_sum <- numeric(5)
resampling_mean_sum <- numeric(5)

fc <- function(x,i=1){
  
  return(mean(x))
}
set.seed(1)
bootmean <- boot(data100,statistic = fc,1)
bootmean
set.seed(1)
mean(sample(data100,100,replace=T))

for (i in 1:100) {
  
  set.seed(i)
  data10000 <- rnorm(10000,0,1)
  data1000 <- rnorm(1000,0,1)
  data100 <- rnorm(100,0,1)
  data10 <- rnorm(10,0,1)
  data5 <- rnorm(5,0,1)
  
  datalist <- list(data10000,data1000,data100,data10,data5)
  
  sampling_mean <- numeric(5)
  for (k in 1:5) {
    sampling_mean[k] <- mean(datalist[[k]], na.rm=TRUE)
  }
  sampling_mean_sum=sampling_mean_sum+sampling_mean^2
  
  
  resampling_mean <- numeric(5)
  for (j in 1:5) {
    set.seed(i)
    dim <- length(datalist[[j]])*10
    resampling_mean[j] <- mean(sample(datalist[[j]],dim,replace=T), na.rm=TRUE)
  }
  
  resampling_mean_sum=resampling_mean_sum+resampling_mean^2 
  
}

sampling_mean_sum
resampling_mean_sum


>>>>>>> fd4f451f465d4bdabdb1468a10c88e3d0e137db1
