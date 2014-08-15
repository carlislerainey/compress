
# Make sure that working directory is set properly, e.g.,
# setwd("~/Dropbox/projects/compress/")

# Clear workspace
rm(list = ls())

library(compactr)

n <- 1000
b.w <- seq(-1, 1, length.out = 5)
mu.fd1 <- mu.fd2 <- true.fd <- numeric(length(b.w))
mu.est1 <- mu.est2 <- true.est <- numeric(length(b.w))

x <- runif(n)
z <- runif(n)
w <- runif(n)
X <- cbind(1, x, z, w)

for (j in 1:length(b.w)) {
  beta <- c(0, 1, 1, b.w[j])
  
p <- plogis(X%*%beta)

X.pred2 <- cbind(1, 0, 0, c(.25, .75))
true.pr <- plogis(X.pred2%*%beta)
true.fd[j] <- true.pr[2, ] - true.pr[1, ]

X.pred <- cbind(0, 0, c(.25, .75))
colnames(X.pred) <- c("x", "z", "w")
X.pred <- data.frame(X.pred)

n.sims <- 100000
fd1 <- fd2 <- numeric(n.sims)
est1 <- est2 <- numeric(n.sims)
pb <- txtProgressBar(min = 0, max = n.sims, style = 3)
for (i in 1:n.sims) {
  y <- rbinom(n, 1, p)
  m1 <- glm(y ~ x + z + w, family = binomial)
  m2 <- glm(y ~ x*z + w, family = binomial)
  pr1 <- predict(m1, newdata = X.pred, type = "response")
  fd1[i] <- pr1[2] - pr1[1]
  est1[i] <- coef(m1)[4]
  pr2 <- predict(m2, newdata = X.pred, type = "response")
  fd2[i] <- pr2[2] - pr2[1]
  est2[i] <- coef(m2)[4]
  setTxtProgressBar(pb, i)
}

mu.fd1[j] <- mean(fd1)
mu.fd2[j] <- mean(fd2)
mu.est1[j] <- mean(est1)
mu.est2[j] <- mean(est2)
true.est[j] <- beta[4]
}

par(mfcol = c(1, 1), family = "serif")
pdf("doc/fig/fig-w-est.pdf", height = 4, width = 5, family = "serif")
eplot(xlim = mm(c(mu.est1, mu.est2)), ylim = mm(c(mu.est1, mu.est2)),
      xlab = "True Coefficient for W",
      ylab = "Estimated Coefficient for W", 
      ylabpos = 2.1)
abline(a = 0, b = 1, col = "grey")
lines(b.w, mu.est1, col = "red", lty = 3)
lines(b.w, mu.est2, col = "black", lty = 1)
text(.5, -.5, "Estimated coefficients for W for models\nwith and without the term XZ overlap.", cex = .7)
dev.off()

pdf("doc/fig/fig-w-fd.pdf", height = 3, width = 4)
eplot(xlim = mm(c(mu.fd1, mu.fd2)), ylim = mm(c(mu.fd1, mu.fd2)),
      xlab = "True First-Difference for W",
      ylab = "Estimated First-Difference for W", 
      ylabpos = 2.1)
abline(a = 0, b = 1, col = "grey")
lines(true.fd, mu.fd1, col = "red", lty = 3)
lines(true.fd, mu.fd2, col = "black", lty = 1)
text(.05, -.05, "Estimated first-differences for W for models\nwith and without the term XZ overlap.", cex = .7)

dev.off()