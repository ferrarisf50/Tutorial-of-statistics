<<<<<<< HEAD
# Estimate of MSE of mean estimator


library(MASS)
library(boot)


# Estimate the standard error of the mean
# Compare bootstrap method and standard error estimator

B=500
loop=100

fc <- function(x,i){
  
  return(mean(x[i]))
}

se_sse <- numeric(5)
se_boot_sse <- numeric(5)
se_mle_sse <- numeric(5)

for (i in 1:loop) {
  
  set.seed(i)
  data10000 <- rnorm(20,0,1)
  data1000 <- rnorm(15,0,1)
  data100 <- rnorm(12,0,1)
  data10 <- rnorm(8,0,1)
  data5 <- rnorm(5,0,1)
  
  datalist <- list(data10000,data1000,data100,data10,data5)
  
  se_theory <- numeric(5)
  for (k in 1:5) {
    se_theory[k] <- 1/sqrt(length(datalist[[k]]))
  }  
  
  se_mle <- numeric(5)
  for (k in 1:5) {
    dim <- length(datalist[[k]])
    se_mle[k] <- sd(datalist[[k]])*sqrt(dim-1)/dim
  }  
  se_mle_sse=se_mle_sse+(se_mle-se_theory)^2
  
  se <- numeric(5)
  for (k in 1:5) {
    dim <- length(datalist[[k]])
    se[k] <- sd(datalist[[k]])/sqrt(dim)
  }
  se_sse=se_sse+(se-se_theory)^2
  
  
  se_boot <- numeric(5)
  for (j in 1:5) {
    set.seed(i)
    bootmean <- boot(datalist[[j]],statistic = fc,B)
    se_boot[j] <- sd(bootmean$t)
  }
  
  se_boot_sse=se_boot_sse+(se_boot-se_theory)^2
  
}
se_theory
se
se_boot
se_mle

se_mle_sse
se_sse
se_boot_sse


se_sse-se_mle_sse
se_boot_sse-se_sse
se_boot_sse-se_mle_sse

>>>>>>> fd4f451f465d4bdabdb1468a10c88e3d0e137db1
