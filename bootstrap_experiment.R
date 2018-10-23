<<<<<<< HEAD
# Estimate of MSE of mean estimator


library(MASS)
library(boot)


# Estimate the standard error of the mean
# Compare bootstrap method and standard error estimator

B=200
loop=100

fc <- function(x,i){
  
  return(mean(x[i]))
}



se_sse <- numeric(5)
se_boot_sse <- numeric(5)
se_boot_para_sse <- numeric(5)

for (i in 1:loop) {
  
  set.seed(i)
  

  
  data10000 <- rnorm(100000,0,1)
  data1000 <- rnorm(5,0,1)
  data100 <- rnorm(5,0,1)
  data10 <- rnorm(5,0,1)
  data5 <- rnorm(5,0,1)
  
  datalist <- list(data10000,data1000,data100,data10,data5)
  
  # standard error in theory
  se_theory <- numeric(5)
  for (k in 1:5) {
    se_theory[k] <- 1/sqrt(length(datalist[[k]]))
  }  
  
  
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
    
    dim <- length(datalist[[j]])
    mle_u <- mean(datalist[[j]])
    mle_sigma <- var(datalist[[j]])*((dim-1)/dim)
    #mle_u <-0
    #mle_sigma <-1
    fit <- list(mle_u,mle_sigma)

    set.seed(i+10)
    
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
se_boot_para






se_sse-se_boot_sse
se_boot_sse-se_boot_para_sse
se_sse-se_boot_para_sse
##################### verify
data10


minusL <- function(params, data) {
    -sum(log(dnorm(data, params[1], params[2])))
} 
fit1 <- nlm(minusL, c(1,1), data=data1000)
fit1


mle_sigma
mean(data1000)
var(data1000)*999/1000
bootmean

#######################

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


############## exponetial distribution
###############################################

B=500
loop=100

fc <- function(x,i){
  
  return(mean(x[i]))
}

se_sse <- numeric(5)
se_boot_sse <- numeric(5)
se_boot_para_sse <- numeric(5)

for (i in 1:loop) {
  
  set.seed(i)
  
  rate=0.1
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
  
  ########### parametric bootstrap
  
  se_boot_para <- numeric(5)
  
  
  for (j in 1:5) {
    
    dim <- length(datalist[[j]])
    u <-dim/mean(datalist[[j]])
    
    
    set.seed(i+10)
    
    gen_exp <- function(data,params) {
      rexp(length(data), params)
    }
    
    bootmean <- boot(datalist[[j]],statistic = fc,R=B,
                     sim="parametric",
                     ran.gen=gen_exp,
                     mle=u)
    se_boot_para[j] <- sd(bootmean$t)
    
    
  }
  se_boot_para_sse=se_boot_para_sse+(se_boot_para-se_theory)^2
  
  
}
se_theory
se
se_boot


se_sse-se_boot_sse
se_boot_sse-se_boot_para_sse



#### lognormal ################
######################################

B=500
loop=1000

fc <- function(x,i){
  
  return(mean(x[i]))
}



se_sse <- numeric(5)
se_boot_sse <- numeric(5)
se_boot_para_sse <- numeric(5)

mu=0
sigma=1

for (i in 1:loop) {
  
  set.seed(i)
  
  
  
  data10000 <- rlnorm(10000,mu,sigma)
  data1000 <- rlnorm(1000,mu,sigma)
  data100 <- rlnorm(100,mu,sigma)
  data10 <- rlnorm(10,mu,sigma)
  data5 <- rlnorm(5,mu,sigma)
  
  datalist <- list(data10000,data1000,data100,data10,data5)
  
  # standard error in theory
  se_theory <- numeric(5)
  for (k in 1:5) {
    se_theory[k] <- 1/sqrt(length(datalist[[k]]))
  }  
  
  
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
  
  ########### parametric bootstrap
  
  se_boot_para <- numeric(5)
  
  
  for (j in 1:5) {
    
    dim <- length(datalist[[j]])
    mu <-mean(log(datalist[[j]]))
    var <- mean((log(datalist[[j]])-mu)^2)
    mle_list <- list(mu,var)
    
    set.seed(i+10)
    
    gen_lognormal <- function(data,params) {
      rlnorm(length(data), params[[1]],params[[2]])
    }
    
    bootmean <- boot(datalist[[j]],statistic = fc,R=B,
                     sim="parametric",
                     ran.gen=gen_lognormal,
                     mle=mle_list)
    se_boot_para[j] <- sd(bootmean$t)
    
    
  }
  se_boot_para_sse=se_boot_para_sse+(se_boot_para-se_theory)^2
  
  
}


se_theory
se
se_boot
se_boot_para


se_sse-se_boot_para_sse
se_sse-se_boot_sse
se_boot_sse-se_boot_para_sse