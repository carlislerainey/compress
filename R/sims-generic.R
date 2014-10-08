
# Make sure that working directory is set properly, e.g.,
# setwd("~/Dropbox/projects/compress/")

# Clear workspace
rm(list = ls())

# Set simulation paramters
n.iter <- 1000
n.sims1 <- 2000
n.sims2 <- 2000

# Load packages
library(arm)
library(MASS)
library(compactr)
format(Sys.time(), "%H:%M:%S")

# a function to randomly create a relationship
genBeta <- function() {
  a <- runif(1, .3, .7)
  b <- runif(1, .3, .7)
  c <- runif(1, min(a,b), max(a,b))
  d <- runif(1, -.3, .3)
  e <- runif(1, min(0, d), max(0, d))
  beta <- NULL
  beta[1] <- a
  beta[2] <- -3*a - b + 4*c
  beta[3] <- 4*e - d
  beta[4] <- 2*a + 2*b - 4*c
  beta[5] <- 4*((1/2)*d - e)
  return(beta)
}

# check that the function meets basic requirements
#   1.) 0 < Pr(Y) < 1
#   2.) Monotonic change in the effect of X and Z
checkBeta <- function() {
  pass <- T
  if (1 <= beta[1] + beta[2]*1 + beta[3]*1 + beta[4]*1^2 + beta[5]*1^2) pass <- F
  if (1 <= beta[1] + beta[2]*0 + beta[3]*1 + beta[4]*0^2 + beta[5]*1^2) pass <- F
  if (1 <= beta[1] + beta[2]*1 + beta[3]*0 + beta[4]*1^2 + beta[5]*0^2) pass <- F
  if (1 <= beta[1] + beta[2]*0 + beta[3]*0 + beta[4]*0^2 + beta[5]*0^2) pass <- F
  if (0 >= beta[1] + beta[2]*1 + beta[3]*1 + beta[4]*1^2 + beta[5]*1^2) pass <- F
  if (0 >= beta[1] + beta[2]*0 + beta[3]*1 + beta[4]*0^2 + beta[5]*1^2) pass <- F
  if (0 >= beta[1] + beta[2]*1 + beta[3]*0 + beta[4]*1^2 + beta[5]*0^2) pass <- F
  if (0 >= beta[1] + beta[2]*0 + beta[3]*0 + beta[4]*0^2 + beta[5]*0^2) pass <- F
  if (sign(beta[2] + 2*beta[4]*0) != sign(beta[2] + 2*beta[4]*1)) pass <- F
  if (sign(beta[3] + 2*beta[5]*0) != sign(beta[3] + 2*beta[5]*1)) pass <- F
  return(pass)
}
  

# Compute the p-value for a single simulated data set
compute.p <- function(unused) {
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

res <- NULL
## Run Loop
for (iter in 1:n.iter) {
  # generate and check beta vectors
  pass <- F
  while (pass == F) {
    beta <- genBeta()
    pass <- checkBeta()
  }
  # generate the sample size
  n <- round(abs(1000*rt(1, 1)))
  while (n > 100000 | n < 500) {
    cat(paste("----Sample Outside Bound (n = ", n,"), Redraw\n", sep = ""))
    n <- round(abs(1000*rt(1, 1)))  
  }
  cat(paste("-- Iteration ", iter, " (N = ", n, ")\n", sep = ""))
  # Use a binary X or Z?
  binary.x <- rbinom(1, 1, .5)
  binary.z <- rbinom(1, 1, .5)
  # distribution parameters of X and Z (if continuous)
  alpha.x <- runif(1, .7, 3)
  beta.x <- runif(1, .7, 3)
  alpha.z <- runif(1, .7, 3)
  beta.z <- runif(1, .7, 3)
  # distribution parameters of X and Z (if binary)
  prob.x <- runif(1, .1, .9)
  prob.z <- runif(1, .1, .9)
  # the second difference used to test for interaction
  change.x <- runif(2, 0, 1)
  change.z <- runif(2, 0, 1)      
  lo.x <- min(change.x)
  hi.x <- max(change.x)
  lo.z <- min(change.z)
  hi.z <- max(change.z)      
  if (binary.x == 1) {
    lo.x <- 0
    hi.x <- 1
  }
  if (binary.z == 1) {
    lo.z <- 0
    hi.z <- 1
  }      
  x <- rbeta(n, alpha.x, beta.x)
  z <- rbeta(n, alpha.z, beta.z)
  if (binary.x == 1) {
    x <- rbinom(n, 1, prob.x)
  }
  if (binary.z == 1) {
    z <- rbinom(n, 1, prob.z)
  }      
  x1.lo.lo <- c(1, lo.x, lo.z)
  x2.lo.lo <- c(x1.lo.lo, x1.lo.lo[2]*x1.lo.lo[3])
  x1.hi.lo <- c(1, hi.x, lo.z)
  x2.hi.lo <- c(x1.hi.lo, x1.hi.lo[2]*x1.hi.lo[3])
  x1.lo.hi <- c(1, lo.x, hi.z)
  x2.lo.hi <- c(x1.lo.hi, x1.lo.hi[2]*x1.lo.hi[3])
  x1.hi.hi <- c(1, hi.x, hi.z)
  x2.hi.hi <- c(x1.hi.hi, x1.hi.hi[2]*x1.hi.hi[3])
  p <- beta[1] + beta[2]*x + beta[3]*z + beta[4]*x^2 + beta[5]*z^2
  #print(iter)
  if (sum(is.na(p)) > 0) {
    cat(print("WARNING"))
    cat(print(beta))
  }
  # simulate the size
  res1 <- simulate.p()
  noprod <- sum(res[,1])/n.sims1
  prod <- sum(res[,2])/n.sims1
  # write a row to the data set  
  res2 <- c(n, binary.x, binary.z, alpha.x, beta.x, alpha.z,
                  beta.z, prob.x, prob.z, lo.x, hi.x, lo.z, hi.z, 
                  noprod, prod, n.sims1, n.sims2)

  res <- rbind(res, res2)
 }
colnames(res) <- c('n', 'binary.x', 'binary.z', 'alpha.x', 'beta.x',
                   'alpha.z', 'beta.z', 'prob.x', 'prob.z', 'lo.x', 
                   'hi.x', 'lo.z', 'hi.z', 'noprod', 'prod', 'n.sims1',
                   'n.sims2')
save(res, file = "output/sims-generic.RData")


