# Make sure that working directory is set properly, e.g.,
# setwd("~/Dropbox/projects/compress/")

n <- 1000
n.sims <- 50
x <- rbeta(n, 1, 1)
z <- rbinom(n, 1, .5)
genBeta <- function() {
  p1 <-  c(.3, .5)
  p2 <- -.3
  beta <- NULL
  beta[1] <- p1[1]
  beta[2] <- p1[2] - p1[1]
  beta[3] <- p2
  return(beta)
}

beta <- genBeta()
p <- beta[1] + beta[2]*x + beta[3]*z
y <- rbinom(n, 1, p)
m1 <- glm(y ~ x + z, family = binomial)
m2 <- glm(y ~ x*z, family = binomial)

lo <- 0.25
hi <- 0.75

res <- matrix(NA, nrow = n.sims, ncol = 20)
for (i in 1:n.sims) {
  print(i)
  beta <- genBeta()
  p <- beta[1] + beta[2]*x + beta[3]*z
  y <- rbinom(n, 1, p)
  m1 <- glm(y ~ x + z, family = binomial)
  m2 <- glm(y ~ x*z, family = binomial)
  sim1 <- coef(sim(m1, n = 500))
  sim2 <- coef(sim(m2, n = 500))
  x1.lo.lo <- c(1, lo, 0)
  x2.lo.lo <- c(x1.lo.lo, x1.lo.lo[2]*x1.lo.lo[3])
  x1.hi.lo <- c(1, hi, 0)
  x2.hi.lo <- c(x1.hi.lo, x1.hi.lo[2]*x1.hi.lo[3])
  x1.lo.hi <- c(1, lo, 1)
  x2.lo.hi <- c(x1.lo.hi, x1.lo.hi[2]*x1.lo.hi[3])
  x1.hi.hi <- c(1, hi, 1)
  x2.hi.hi <- c(x1.hi.hi, x1.hi.hi[2]*x1.hi.hi[3])
  d1 <- (plogis(sim1%*%x1.hi.hi) - plogis(sim1%*%x1.hi.lo)) - 
    (plogis(sim1%*%x1.lo.hi) - plogis(sim1%*%x1.lo.lo))
  d2 <- (plogis(sim2%*%x2.hi.hi) - plogis(sim2%*%x2.hi.lo)) - 
    (plogis(sim2%*%x2.lo.hi) - plogis(sim2%*%x2.lo.lo))  
  q1 <- quantile(d1, c(0.025, .5, 0.975))
  q2 <- quantile(d2, c(0.025, .5, 0.975))  
  qint <- quantile(sim2[, "x:z"], c(0.025, .5, 0.975))
  q1sig <- 1*!(q1[1] < 0 & q1[3] > 0)
  q2sig <- 1*!(q2[1] < 0 & q2[3] > 0) 
  intsig <- 1*!(qint[1] < 0 & qint[3] > 0)
  d1.p <- min(2*sum(d1 > 0)/length(d1), 2*sum(d1 < 0)/length(d1))
  res[i, 1:8] <- c(q1sig, q2sig, q1, q2)
}
sum(res[,1])/n.sims
sum(res[,2])/n.sims

pdf("doc/fig/fig-plotted-cis.pdf", height = 4, width = 9, family = "serif")
par(mfrow = c(1,2), family = "serif")
ci1 <- res[order(res[, 4]), 3:5]
ci2 <- res[order(res[, 7]), 6:8]
xlim.set <- c(0, ci1, ci2, -ci1, - ci2)
main <- paste("Excluding Product Term\n(",sum(res[,1])/n.sims
              *100, "% statistically significant)", sep = "")
eplot(xlim = mm(xlim.set), ylim = c(1, nrow(ci1)),
      anny = FALSE, xlab = "Second Difference",
      main = main)
for (i in 1:nrow(ci1)) {
  lines(c(ci1[i,1], ci1[i,3]), c(i, i), col = "grey")
}
points(ci1[,2], 1:nrow(ci1), pch = 19, cex = .5)
abline(v = 0, lty = 2)
main <- paste("Including Product Term\n(",sum(res[,2])/n.sims
              *100, "% statistically significant)", sep = "")
aplot(main)
for (i in 1:nrow(ci1)) {
  lines(c(ci2[i,1], ci2[i,3]), c(i, i), col = "grey")
}
points(ci2[,2], 1:nrow(ci1), pch = 19, col = "red", cex = .5)
abline(v = 0, lty = 2)
dev.off()
