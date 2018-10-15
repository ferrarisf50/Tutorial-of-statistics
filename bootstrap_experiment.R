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
se_boot_para_sse <- numeric(5)

for (i in 1:loop) {
  
  set.seed(i)
  

  
  data10000 <- rnorm(10000,0,1)
  data1000 <- rnorm(1000,0,1)
  data100 <- rnorm(100,0,1)
  data10 <- rnorm(10,0,1)
  data5 <- rnorm(5,0,1)
  
  datalist <- list(data10000,data1000,data100,data10,data5)
  
  # standard error in theory
  se_theory <- numeric(5)
  for (k in 1:5) {
    se_theory[k] <- 1/sqrt(length(datalist[[k]]))
  }  
  
  #standard error estimate using MLE
  se_mle <- numeric(5)
  for (k in 1:5) {
    dim <- length(datalist[[k]])
    se_mle[k] <- sd(datalist[[k]])*sqrt(dim-1)/dim
  }  
  se_mle_sse=se_mle_sse+(se_mle-se_theory)^2
  
  #standard error estimate
  se <- numeric(5)
  for (k in 1:5) {
    dim <- length(datalist[[k]])
    se[k] <- sd(datalist[[k]])/sqrt(dim)
  }
  se_sse=se_sse+(se-se_theory)^2
  
  #standard error estimate using nonparametric bootstrap
  se_boot <- numeric(5)
  for (j in 1:5) {
    set.seed(i)
    bootmean <- boot(datalist[[j]],statistic = fc,B)
    se_boot[j] <- sd(bootmean$t)
  }
  
  se_boot_sse=se_boot_sse+(se_boot-se_theory)^2
  
  #standard error estimate using parametric bootstrap
  se_boot_para <- numeric(5)
  
  
  for (j in 1:5) {
    set.seed(i)
    dim <- length(datalist[[j]])
    mle_u <- mean(datalist[[j]])
    mle_sigma <- var(datalist[[j]])*((dim-1)/dim)
    fit <- list(mle_u,mle_sigma)

    
    gen_normal <- function(data,params) {
      rnorm(length(data), params[[1]],params[[2]])
    }
 
    bootmean <- boot(datalist[[j]],statistic = fc,R=B,
                     sim="parametric",
                     ran.gen=gen_normal,
                     mle=fit)
    se_boot_para[j] <- sd(bootmean$t)
    
    
  }
  se_boot_para_sse=se_boot_para_sse+(se_boot_para-se_theory)^2
  
  
  
}

se_theory
se
se_boot
se_mle
se_boot_para

se_mle_sse
se_sse
se_boot_sse
se_boot_para_sse

se_sse-se_mle_sse
se_boot_sse-se_sse
se_boot_sse-se_mle_sse


datalist[[j]]
mle_sigma <- sd(datalist[[j]])^2
var(datalist[[j]])


# exponetial distribution

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
  
  rate=2
  data10000 <- rexp(10000,rate)
  data1000 <- rexp(1000,rate)
  data100 <- rexp(100,rate)
  data10 <- rexp(10,rate)
  data5 <- rexp(5,rate)  
  
  

  datalist <- list(data10000,data1000,data100,data10,data5)
  
  se_theory <- numeric(5)
  for (k in 1:5) {
    se_theory[k] <- 1/rate/sqrt(length(datalist[[k]]))
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

se_mle_sse-se_sse
se_sse-se_boot_sse
se_boot_sse-se_boot_para_sse
se_boot_para_sse


df <- data.frame(se_mle_sse,se_sse,se_boot_sse,se_boot_para_sse)

df3 <- normalize(df)
df4 <- data.frame(x=c(1:5),df)

library(reshape)
library(ggplot2)
library(gridExtra)
df2 <- melt(df4, id=c("x"))

p1 <- ggplot(data = df2[df2$x==1,], aes(x=x,y=value)) + geom_point(aes(colour=variable,))
p2 <- ggplot(data = df2[df2$x==2,], aes(x=x,y=value)) + geom_point(aes(colour=variable,))
p3 <- ggplot(data = df2[df2$x==3,], aes(x=x,y=value)) + geom_point(aes(colour=variable,))
p4 <- ggplot(data = df2[df2$x==4,], aes(x=x,y=value)) + geom_point(aes(colour=variable,))
p5 <- ggplot(data = df2[df2$x==5,], aes(x=x,y=value)) + geom_point(aes(colour=variable,))

grid.arrange(p1, p2,p3,p4,p5,ncol=2)

??normalize
