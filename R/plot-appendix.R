
# Make sure that working directory is set properly, e.g.,
# setwd("~/Dropbox/projects/compress/")

# Clear workspace
rm(list = ls())

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


## explain the algorithm used to generate analyses.

a <- .6
b <- .35
c <- .4
d <- .25
e <- .1

beta <- NULL
beta[1] <- a
beta[2] <- -3*a - b + 4*c
beta[3] <- 4*e - d
beta[4] <- 2*a + 2*b - 4*c
beta[5] <- 4*((1/2)*d - e)
pt <- c(21, 1)

pdf("doc/fig/fig-choose-relationship.pdf", height = 5, width = 8)
par(mfrow = c(2,3), family = "serif",
    mar = c(.75,.75,.75,.75), oma = c(4,4,4,1))
eplot(xlim = c(-0.05, 1.05), ylim = c(0, 1),
      xlab = "X", ylab = "Pr(Y)", ylabpos = 2,
      main = "Step 1: Choose A and B from U(0.3, 0.7)")
lines(c(0, 0), c(.3, .7), lty = 1)
lines(c(1, 1), c(.3, .7), lty = 1)
points(0, a, pch = pt[1], cex = pt[2], bg = "black")
text(0, a, "A", pos = 2)
points(1, b, pch = pt[1], cex = pt[2], bg = "black")
text(1, b, "B", pos = 4)

aplot("Step 2: Choose C from U(A, B) or U(B, A)")
lines(c(.5, .5), c(a, b), lty = 1)
lines(c(0, .5), c(a, a), lty = 3)
lines(c(.5, 1), c(b, b), lty = 3)
points(0, a, pch = pt[1], cex = pt[2], bg = "white")
text(0, a, "A", pos = 2)
points(1, b, pch = pt[1], cex = pt[2], bg = "white")
text(1, b, "B", pos = 4)
points(.5, c, pch = pt[1], cex = pt[2], bg = "black")
text(.5, c, "C", pos = 4)

aplot("Step 3: Choose D from U(A - 0.3, A + 0.3)")
lines(c(0, 0), c(a - .3, a + .3))
points(0, a, pch = pt[1], cex = pt[2], bg = "white")
text(0, a, "A", pos = 2)
points(1, b, pch = pt[1], cex = pt[2], bg = "white")
text(1, b, "B", pos = 4)
points(.5, c, pch = pt[1], cex = pt[2], bg = "white")
text(.5, c, "C", pos = 4)
points(0, d + a, pch = pt[1], cex = pt[2], bg = "black")
text(0, d + a, "D", pos = 2)

aplot("Step 4: Choose E from U(A, D) or U(D, A)")
lines(c(0, 0), c(a, a + d))
points(0, a, pch = pt[1], cex = pt[2], bg = "white")
text(0, a, "A", pos = 2)
points(1, b, pch = pt[1], cex = pt[2], bg = "white")
text(1, b, "B", pos = 4)
points(.5, c, pch = pt[1], cex = pt[2], bg = "white")
text(.5, c, "C", pos = 4)
points(0, d + a, pch = pt[1], cex = pt[2], bg = "white")
text(0, d + a, "D", pos = 2)
points(0, e + a, pch = pt[1], cex = pt[2], bg = "black")
text(0, e + a, "E", pos = 2)

aplot("Step 5: Connect and Solve")
z <- 0
curve(beta[1] + beta[2]*x + beta[3]*z + beta[4]*x^2 + beta[5]*z^2, 
      add = TRUE, xlim = c(0, 1))
z <- 1
curve(beta[1] + beta[2]*x + beta[3]*z + beta[4]*x^2 + beta[5]*z^2, 
      add = TRUE, xlim = c(0, 1))
z <- .5
curve(beta[1] + beta[2]*x + beta[3]*z + beta[4]*x^2 + beta[5]*z^2, 
      add = TRUE, xlim = c(0, 1))

points(0, a, pch = pt[1], cex = pt[2], bg = "white")
text(0, a, "A", pos = 2)
points(1, b, pch = pt[1], cex = pt[2], bg = "white")
text(1, b, "B", pos = 1)
points(.5, c, pch = pt[1], cex = pt[2], bg = "white")
text(.5, c, "C", pos = 1)
points(0, d + a, pch = pt[1], cex = pt[2], bg = "white")
text(0, d + a, "D", pos = 2)
points(0, e + a, pch = pt[1], cex = pt[2], bg = "white")
text(0, e + a, "E", pos = 2)
text(.9, -.03 + b, "Z = 0", pos = 3)
text(.9, -.03 + b + e, "Z = 0.5", pos = 3)
text(.9, -.03 + b + d, "Z = 1", pos = 3)
text(.5, .15, expression(Pr(Y) == 0.6 - 0.55*X + 0.15*Z + 0.3*X^2+ 0.1*Z^2))
dev.off()

pdf("doc/fig/fig-relationship-sample.pdf", height = 7, width = 8)
par(mfrow = c(5, 5), family = "serif",
    mar = c(.5,.5,.5,.5), oma = c(4,4,4,1))
beta0 <- NULL
for (iter in 1:25) {
  print(iter)
  # generate and check beta vectors
  pass <- F
  while (pass == F) {
    beta <- genBeta()
    pass <- checkBeta()
  }
  beta0 <- cbind(beta0, beta)
  eplot(xlim = c(-0.05, 1.05), ylim = c(0, 1),
        xlab = "X", ylab = "Pr(Y)", ylabpos = 2)
  z <- 0
  curve(beta[1] + beta[2]*x + beta[3]*z + beta[4]*x^2 + beta[5]*z^2, 
        add = TRUE, xlim = c(0, 1))
  z <- 1
  curve(beta[1] + beta[2]*x + beta[3]*z + beta[4]*x^2 + beta[5]*z^2, 
        add = TRUE, xlim = c(0, 1))
  z <- .5
  curve(beta[1] + beta[2]*x + beta[3]*z + beta[4]*x^2 + beta[5]*z^2, 
        add = TRUE, xlim = c(0, 1))
}
dev.off()

n0 <- numeric(1000000)
for (iter in 1:1000000) {
  # generate the sample size
  n <- round(abs(1000*rt(1, 1)))
  while (n > 1000000 | n < 500) {
    #cat(paste("----Sample Outside Bound (n = ", n,"), Redraw\n", sep = ""))
    n <- round(abs(1000*rt(1, 1)))  
  }
  n0[iter] <- n
}

quantile(n0, 0:10/10)


## Plot 

pdf("doc/fig/fig-distribution-sample.pdf", height = 7, width = 8)
set.seed(45872057)
par(mfrow = c(5, 5), family = "serif",
    mar = c(.5,.5,.5,.5), oma = c(4,4,4,1))
for (i in 1:25) {
  p <- runif(2, .7, 3)
  eplot(xlim = c(0, 1), ylim = c(0, 2.2), 
        xlab = "X", ylab = "Density",
        ylabpos = 2)
  curve(dbeta(x, p[1], p[2]), add = TRUE, lwd = 2)
}
dev.off()