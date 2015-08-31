
# Make sure that working directory is set properly, e.g.,
# setwd("~/Dropbox/projects/compress/")

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
  p <- plogis(beta[1] + beta[2]*x + beta[3]*z)
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

beta.x <- c(.75, 1.5, 3)
beta.z <- c(.75, 1.5, 3)
sample.size <- round(exp(seq(log(100), log(100000), length.out = 10)))

prod.res <- array(NA, 
                  dim = c(length(sample.size), length(beta.x), length(beta.z)))
noprod.res <- prod.res

## Run Loop

for (ss in 1:length(sample.size)) {
  cat(paste(sample.size[ss], "\n", sep = ""))
  for (b.x in 1:length(beta.x)) {
    cat(paste("-- beta.x = ", beta.x[b.x], "\n", sep = ""))
    for (b.z in 1:length(beta.z)) { 
      cat(paste("------ beta.z = ", beta.z[b.z], "\n", sep = ""))
      n <- sample.size[ss]
      beta <- c(0, beta.x[b.x], beta.z[b.z])
      x <- runif(n)
      z <- rbinom(n, 1, .5)      
      lo <- .25
      hi <- .75
      x1.lo.lo <- c(1, lo, 0)
      x2.lo.lo <- c(x1.lo.lo, x1.lo.lo[2]*x1.lo.lo[3])
      x1.hi.lo <- c(1, hi, 0)
      x2.hi.lo <- c(x1.hi.lo, x1.hi.lo[2]*x1.hi.lo[3])
      x1.lo.hi <- c(1, lo, 1)
      x2.lo.hi <- c(x1.lo.hi, x1.lo.hi[2]*x1.lo.hi[3])
      x1.hi.hi <- c(1, hi, 1)
      x2.hi.hi <- c(x1.hi.hi, x1.hi.hi[2]*x1.hi.hi[3])
      res <- simulate.p()
      noprod.res[ss, b.x, b.z] <- sum(res[,1])/n.sims1
      prod.res[ss, b.x, b.z] <- sum(res[,2])/n.sims1
    }
  }
}

dir.create(path = "output", showWarnings = FALSE)
save(noprod.res, prod.res, file = "output/sims-pwr.RData")
