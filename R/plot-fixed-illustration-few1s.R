
# Make sure that working directory is set properly, e.g.,
# setwd("~/Dropbox/projects/compress/")

# Clear workspace
rm(list = ls())

set.seed(4709901)
n <- 1000000
x <- runif(n)
z <- rbinom(n, 1, .5)
genBeta <- function() {
  p1 <-  c(.04, .08)
  p2 <- -.02
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
beta1 <- coef(m1)
beta2 <- coef(m2)

library(compactr)

f <- function(x, z) {
  beta[1] + beta[2]*x + beta[3]*z
} 

f1 <- function(x, z) {
  plogis(beta1[1] + beta1[2]*x + beta1[3]*z)
} 

f2 <- function(x, z) {
  plogis(beta2[1] + beta2[2]*x + beta2[3]*z + beta2[4]*x*z)
} 

pdf("doc/fig/fig-example-few1s.pdf", 
           height = 3, width = 9, family = "serif")

par(mfrow = c(1,3), family = "serif",
    mar = c(.75,.75,.75,.75), oma = c(4,4,4,1))
eplot(xlim = c(0,1), ylim = c(0,.08),
      xlab = "X", ylab = "Pr(Y)",
      main = "True Relationship", ylabpos = 2)
curve(beta[1] + beta[2]*x, lty = 3, add = TRUE)
curve(beta[1] + beta[2]*x + beta[3], lty = 3, add = TRUE)
text(.2, .4, "Z = 0")
text(.2, .1, "Z = 1")
# lines(c(.25, .75), c(f(.25, 1), f(.25, 1)))
# lines(c(.75, .75), c(f(.25, 1), f(.75, 1)))
# text(.89, mean(c(f(.75, 1), f(.25, 1))), 
#      paste("Change in\nPr(Y) =", f(.75, 1) - f(.25, 1)), 
#      cex = .95)
# lines(c(.25, .75), c(f(.25, 0), f(.25, 0)))
# lines(c(.75, .75), c(f(.25, 0), f(.75, 0)))
# text(.89, mean(c(f(.75, 0), f(.25, 0))), 
#      paste("Change in\nPr(Y) =", f(.75, 0) - f(.25, 0)), 
#      cex = .95)



aplot("Logistic Regression Fit\n with Product Term Excluded")
curve(beta[1] + beta[2]*x, lty = 3, add = TRUE)
curve(beta[1] + beta[2]*x + beta[3], lty = 3, add = TRUE)
curve(plogis(beta1[1] + beta1[2]*x), lty = 1, lwd = 2, add = TRUE)
curve(plogis(beta1[1] + beta1[2]*x + beta1[3]), lty = 1, lwd = 2, add = TRUE)
# lines(c(.25, .75), c(f1(.25, 1), f1(.25, 1)))
# lines(c(.75, .75), c(f1(.25, 1), f1(.75, 1)))
# text(.89, mean(c(f1(.75, 1), f1(.25, 1))), 
#      paste("Change in\nPr(Y) =", 
#            round(f1(.75, 1) - f1(.25, 1), 2)), 
#      cex = .95)
# lines(c(.25, .75), c(f1(.25, 0), f1(.25, 0)))
# lines(c(.75, .75), c(f1(.25, 0), f1(.75, 0)))
# text(.89, mean(c(f1(.75, 0), f1(.25, 0))), 
#      paste("Change in\nPr(Y) =", 
#            round(f1(.75, 0) - f1(.25, 0), 2)), 
#      cex = .95)

aplot("Logistic Regression Fit\n with Product Term Included")
curve(beta[1] + beta[2]*x, lty = 3, add = TRUE)
curve(beta[1] + beta[2]*x + beta[3], lty = 3, add = TRUE)
curve(plogis(beta2[1] + beta2[2]*x), lty = 1, lwd = 2, add = TRUE)
curve(plogis(beta2[1] + beta2[2]*x + beta2[3] + beta2[4]*x), lty = 1, lwd = 2, add = TRUE)
# lines(c(.25, .75), c(f2(.25, 1), f2(.25, 1)))
# lines(c(.75, .75), c(f2(.25, 1), f2(.75, 1)))
# text(.89, mean(c(f2(.75, 1), f2(.25, 1))), 
#      paste("Change in\nPr(Y) =", 
#            round(f2(.75, 1) - f2(.25, 1), 2)), 
#      cex = .95)
# lines(c(.25, .75), c(f2(.25, 0), f2(.25, 0)))
# lines(c(.75, .75), c(f2(.25, 0), f2(.75, 0)))
# text(.89, mean(c(f2(.75, 0), f2(.25, 0))), 
#      paste("Change in\nPr(Y) =", 
#            round(f2(.75, 0) - f2(.25, 0), 2)), 
#      cex = .95)
dev.off()