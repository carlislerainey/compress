
# Make sure that working directory is set properly, e.g.,
# setwd("~/Dropbox/projects/compress/")

# Clear workspace
rm(list = ls())

library(arm)
library(MASS)

# simulation paramaters
n.sims1 <- 10
n.sims2 <- 10


genBeta <- function() {
  p1 <-  c(.3, .5)
  p2 <- -.3
  beta <- NULL
  beta[1] <- p1[1]
  beta[2] <- p1[2] - p1[1]
  beta[3] <- p2
  return(beta)
}

## Setup Simulate Function
simulate.p <- function(n.sims1, n.sims2) {
  res <- matrix(NA, nrow = n.sims1, ncol = 2)      
  for (i in 1:n.sims1) {
    beta <- genBeta()
    p <- beta[1] + beta[2]*x + beta[3]*z
    res[i, ] <- compute.p(n.sims2)
  }
  return(t(res))
}

## Compute p
compute.p <- function(unused) {
  beta <- genBeta()
  p <- beta[1] + beta[2]*x + beta[3]*z
  y <- rbinom(n, 1, p)
  m1 <- bayesglm(y ~ x + z, family = binomial)
  m2 <- bayesglm(y ~ x*z, family = binomial)
  sim1 <- mvrnorm(n.sims2, coef(m1), Sigma = vcov(m1))
  sim2 <- mvrnorm(n.sims2, coef(m2), Sigma = vcov(m2))
  d1 <- (plogis(sim1%*%x1.hi.hi) - plogis(sim1%*%x1.hi.lo)) - 
    (plogis(sim1%*%x1.lo.hi) - plogis(sim1%*%x1.lo.lo))
  d2 <- (plogis(sim2%*%x2.hi.hi) - plogis(sim2%*%x2.hi.lo)) - 
    (plogis(sim2%*%x2.lo.hi) - plogis(sim2%*%x2.lo.lo))  
  q1 <- quantile(d1, c(0.025, .5, 0.975))
  q2 <- quantile(d2, c(0.025, .5, 0.975))  
  q1sig <- 1*!(q1[1] < 0 & q1[3] > 0)
  q2sig <- 1*!(q2[1] < 0 & q2[3] > 0) 
  return(c(q1sig, q2sig))
}

## Setup Simulate Function
simulate.p <- function() {
  res <- matrix(NA, nrow = n.sims1, ncol = 2)    
  res <- apply(res, 1, compute.p)
  return(t(res))
}


## Setup Loop
dist.x.names <- c("X ~ Beta(1, 1)", "X ~ Beta(2, 2)", "X ~ Beta(0.1, 1)")
dist.x <- list(c(1,1), c(2,2), c(.1,1))
change.x.names <- c("25th to 75th", "min to max", "5th to 15th\npercentile")
change.x <- list(c(.25, .75), c(0,1), c(.05, .15))
sample.size <- round(exp(seq(log(100), log(100000), length.out = 10)))

prod.res <- array(NA, 
  dim = c(length(sample.size), length(dist.x.names), length(change.x.names)))
noprod.res <- array(NA, 
  dim = c(length(sample.size), length(dist.x.names), length(change.x.names)))


## Run Loop

for (ss in 1:length(sample.size)) {
  cat(paste(sample.size[ss], "\n", sep = ""))
  for (dist in 1:length(dist.x.names)) {
    cat(paste("-- ", dist.x.names[dist], "\n", sep = ""))
    for (change in 1:length(change.x.names)) { 
      cat(paste("------ ", change.x.names[change], "\n", sep = ""))
      
      n <- sample.size[ss]
      x <- rbeta(n, dist.x[[dist]][1], dist.x[[dist]][2])
      z <- rbinom(n, 1, .5)      
      lo <- quantile(x, change.x[[change]][1])
      hi <- quantile(x, change.x[[change]][2])
      x1.lo.lo <- c(1, lo, 0)
      x2.lo.lo <- c(x1.lo.lo, x1.lo.lo[2]*x1.lo.lo[3])
      x1.hi.lo <- c(1, hi, 0)
      x2.hi.lo <- c(x1.hi.lo, x1.hi.lo[2]*x1.hi.lo[3])
      x1.lo.hi <- c(1, lo, 1)
      x2.lo.hi <- c(x1.lo.hi, x1.lo.hi[2]*x1.lo.hi[3])
      x1.hi.hi <- c(1, hi, 1)
      x2.hi.hi <- c(x1.hi.hi, x1.hi.hi[2]*x1.hi.hi[3])
      #sfExportAll()
      res <- simulate.p()
      noprod.res[ss, dist, change] <- sum(res[,1])/n.sims1
      prod.res[ss, dist, change] <- sum(res[,2])/n.sims1
    }
  }
}

save(noprod.res, prod.res, file = "output/sims-fixed.RData")
