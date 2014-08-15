rm(list = ls())
setwd("~/Dropbox/Projects/Compress")

library(arm)
library(compactr)
library(foreign)
library(texreg)

# Data available at 
#   http://pantheon.yale.edu/~brusset/io_dta.zip
or <- read.dta("or-replication/data/or.dta")

# compute lower democracy score
or$dem.lo <- apply(cbind(or$demauta, or$demautb), 1, min)

# glm version of their gee on pp. 314 with no product term
m.noprod <- glm(dispute1 ~ allies + lcaprat2 + noncontg + dem.lo + logdstab + minrpwrs,
          family = "binomial", data = or)

# glm version of their gee on pp. 314 with no product terms
m.prod <- glm(dispute1 ~ allies + lcaprat2 + noncontg + dem.lo*logdstab + minrpwrs,
          family = "binomial", data = or)

display(m.noprod, detail = TRUE)
display(m.prod, detail = TRUE)
BIC(m.noprod, m.prod)

###############################################################
## Simulate Pr(Conflict) as Democracy Varies
###############################################################

## No Product Term
dist <- seq(min(or$logdstab), max(or$logdstab), length.out = 100)
xlab <- c(10, 100, 1000, 10000) 
xat <- log(xlab)
x.lo <- x.hi <- cbind(1, 
                      median(or$allies),
                      median(or$lcaprat2),
                      0, #noncontg, coded -1, 0
                      -10,
                      dist, # log(dist)
                      median(or$minrpwrs))
x.hi[, 5] <- 10 # dem
sim.noprod <- coef(sim(m.noprod, n = 1000))
pred.lo.noprod <- plogis(x.lo%*%t(sim.noprod))
pred.hi.noprod <- plogis(x.hi%*%t(sim.noprod))

## Product Term
x.lo <- cbind(x.lo, x.lo[, 5]*x.lo[, 6])
x.hi <- cbind(x.hi, x.hi[, 5]*x.hi[, 6])
sim.prod <- coef(sim(m.prod, n = 1000))
pred.lo.prod <- plogis(x.lo%*%t(sim.prod))
pred.hi.prod <- plogis(x.hi%*%t(sim.prod))

###############################################################
## Predicted Probabilities Plot as Distance Varies
###############################################################

## Graphical Parameters
pdf("doc/fig/fig-pr-distance.pdf", height = 4, width = 7, family = "serif")
par(mfrow = c(2,2), mar = c(.75,.75,.75,.75), oma = c(3,9,1,1), family = "serif")
ylim0 <- mm(c(0, pred.lo.noprod, pred.hi.noprod, pred.lo.prod, pred.hi.prod))
            
## Product Term Excluded
hist(or$logdstab, main = NA, axes = FALSE, col = "grey80", border = NA)
par(new = TRUE)
eplot(xlim = mm(dist), ylim = ylim0,
      xlab = "Distance (in Miles)",
      ylab = "Pr(Conflict)",
      ylabpos = 1.9,
      main = paste("Democracies"),
      xat = xat,
      xticklab = xlab)
lines(dist, apply(pred.hi.noprod, 1, quantile, .05), lty = 2)
lines(dist, apply(pred.hi.noprod, 1, quantile, .5), lwd = 3)
lines(dist, apply(pred.hi.noprod, 1, quantile, .95), lty = 2)
text(-1.8, sum(ylim0)/2, "Product Term\nExcluded", xpd = NA)

hist(or$logdstab, main = NA, axes = FALSE, col = "grey80", border = NA)
par(new = TRUE)
eplot(xlim = mm(dist), ylim = ylim0,
      xlab = "Distance (in Miles)",
      ylab = "Pr(Conflict)",
      ylabpos = 1.9,
      main = paste("Non-Democracies"),
      xat = xat,
      xticklab = xlab)
lines(dist, apply(pred.lo.noprod, 1, quantile, .05), lty = 2)
lines(dist, apply(pred.lo.noprod, 1, quantile, .5), lwd = 3)
lines(dist, apply(pred.lo.noprod, 1, quantile, .95), lty = 2)

## Product Term Included
hist(or$logdstab, main = NA, axes = FALSE, col = "grey80", border = NA)
par(new = TRUE)
eplot(xlim = mm(dist), ylim = ylim0,
      xlab = "Distance (in Miles)",
      ylab = "Pr(Conflict)",
      ylabpos = 1.9,
      xat = xat,
      xticklab = xlab)
lines(dist, apply(pred.hi.prod, 1, quantile, .05), lty = 2)
lines(dist, apply(pred.hi.prod, 1, quantile, .5), lwd = 3)
lines(dist, apply(pred.hi.prod, 1, quantile, .95), lty = 2)
text(-1.8, sum(ylim0)/2, "Product Term\nIncluded", xpd = NA)

hist(or$logdstab, main = NA, axes = FALSE, col = "grey80", border = NA)
par(new = TRUE)
eplot(xlim = mm(dist), ylim = ylim0,
      xlab = "Distance (in Miles)",
      ylab = "Pr(Conflict)",
      ylabpos = 1.9,
      xat = xat,
      xticklab = xlab)
lines(dist, apply(pred.lo.prod, 1, quantile, .05), lty = 2)
lines(dist, apply(pred.lo.prod, 1, quantile, .5), lwd = 3)
lines(dist, apply(pred.lo.prod, 1, quantile, .95), lty = 2)
dev.off()

###############################################################
## First Difference as Distance Varies Plot (No Product Term)
###############################################################

## Graphical Parameters
pdf("doc/fig/fig-fd-distance.pdf", height = 2.5, width = 6, family = "serif")
par(mfrow = c(1,2), mar = c(.75,.75,.75,.75), oma = c(3,4,1,1), family = "serif")
ylim0 <- mm(c(0, pred.hi.noprod - pred.lo.noprod, pred.hi.noprod - pred.lo.noprod))

## No Product Term
hist(or$logdstab, main = NA, axes = FALSE, col = "grey80", border = NA)
par(new = TRUE)
eplot(xlim = mm(dist), ylim = ylim0,
      xlab = "Distance (in Miles)",
      ylab = "Pr(Conflict)",
      ylabpos = 2.3,
      xat = xat,
      xticklab = xlab,
      main = "Product Term Excluded")
lines(dist, apply(pred.hi.noprod - pred.lo.noprod, 1, quantile, .05), lty = 2)
lines(dist, apply(pred.hi.noprod - pred.lo.noprod, 1, quantile, .5), lwd = 3)
lines(dist, apply(pred.hi.noprod - pred.lo.noprod, 1, quantile, .95), lty = 2)

## Product Term
hist(or$logdstab, main = NA, axes = FALSE, col = "grey80", border = NA)
par(new = TRUE)
eplot(xlim = mm(dist), ylim = ylim0,
      xlab = "Distance (in Miles)",
      ylab = "Pr(Conflict)",
      ylabpos = 2.3,
      xat = xat,
      xticklab = xlab,
      main = "Product Term Included")
lines(dist, apply(pred.hi.prod - pred.lo.prod, 1, quantile, .05), lty = 2)
lines(dist, apply(pred.hi.prod - pred.lo.prod, 1, quantile, .5), lwd = 3)
lines(dist, apply(pred.hi.prod - pred.lo.prod, 1, quantile, .95), lty = 2)
dev.off()

###############################################################
## Calculate Second-Difference and Plot CIs
###############################################################

## Product Term Excluded
fd.noprod <- pred.lo.noprod - pred.hi.noprod
sd.noprod <- fd.noprod[nrow(fd.noprod), ] - fd.noprod[1, ] # min-max second difference
q.sd.noprod <- quantile(sd.noprod, c(.05, .5, .95))

## Product Term Included
fd.prod <- pred.lo.prod - pred.hi.prod
sd.prod <- fd.prod[nrow(fd.prod), ] - fd.prod[1, ] # min-max second difference
q.sd.prod <- quantile(sd.prod, c(.05, .5, .95))

## Graphical Parameters
pdf("doc/fig/fig-sd-distance.pdf", height = 2.5, width = 4.5, family = "serif")
par(mfrow = c(1,1), mar = c(.75,.75,.75,.75), oma = c(3,1,1,1), family = "serif")
xlim0 <- mm(c(0, q.sd.noprod, q.sd.prod))

## Plot CIs
eplot(xlim = xlim0, ylim =  c(.5, 2.5),
      xlab = "Second-Difference",
      anny = FALSE)
# Product Term Excluded
points(q.sd.noprod[2], 1, pch = 19, cex = .7)
text(q.sd.noprod[2], 1, "Product Term Excluded", pos = 3, cex = .7)
lines(q.sd.noprod[c(1, 3)], c(1, 1), lwd = 2)
# Product Term Excluded
points(q.sd.prod[2], 2, pch = 19, cex = .7)
text(q.sd.prod[2], 2, "Product Term Included", pos = 3, cex = .7)
lines(q.sd.prod[c(1, 3)], c(2, 2), lwd = 2)
abline(v = 0, lty = 2)
dev.off()

###############################################################
## Print Regression Tables
###############################################################

texreg(list(m.noprod, m.prod),
          custom.model.names = c("No Product Term", "Product Term"),
          custom.coef.names = c("Constant",
                                "Allies",
                                "Power Ratio",
                                "Noncontiguity",
                                "Lower Democracy",
                                "Log(Distance)",
                                "Only Minor Powers",
                                "Lower Democracy $\\times$ Log(Distance)"),
          digits = 3, reorder.coef = c(5, 6, 8, 2, 3, 4, 7, 1),
          include.loglik = FALSE, include.deviance = FALSE)


###############################################################
## Simulate Pr(Conflict) as Distance Varies
###############################################################

## No Product Term
dem.lo <- seq(min(or$dem.lo), max(or$dem.lo), length.out = 100)
xlab <- c(-10, -5, 0, 5, 10)
xat <- xlab
x.lo <- x.hi <- cbind(1, 
                      median(or$allies),
                      median(or$lcaprat2),
                      0, #noncontg, coded -1, 0
                      dem.lo,
                      quantile(or$logdstab, .25), # log(dist)
                      median(or$minrpwrs))
x.hi[, 6] <- quantile(or$logdstab, .75) # dem
sim.noprod <- coef(sim(m.noprod, n = 1000))
pred.lo.noprod <- plogis(x.lo%*%t(sim.noprod))
pred.hi.noprod <- plogis(x.hi%*%t(sim.noprod))

## Product Term
x.lo <- cbind(x.lo, x.lo[, 5]*x.lo[, 6])
x.hi <- cbind(x.hi, x.hi[, 5]*x.hi[, 6])
sim.prod <- coef(sim(m.prod, n = 1000))
pred.lo.prod <- plogis(x.lo%*%t(sim.prod))
pred.hi.prod <- plogis(x.hi%*%t(sim.prod))

# Clean up
rm(x.lo, x.hi)

###############################################################
## First Difference as Democracy Varies Plot (No Product Term)
###############################################################

## Graphical Parameters
pdf("doc/fig/fig-fd-democracy.pdf", height = 2.5, width = 6, family = "serif")
par(mfrow = c(1,2), mar = c(.75,.75,.75,.75), oma = c(3,4,1,1), family = "serif")
ylim0 <- mm(c(0, pred.hi.noprod - pred.lo.noprod, pred.hi.noprod - pred.lo.noprod))

## No Product Term
hist(or$dem.lo, main = NA, axes = FALSE, col = "grey80", border = NA)
par(new = TRUE)
eplot(xlim = mm(dem.lo), ylim = ylim0,
      xlab = "Smaller Democracy Score",
      ylab = "Pr(Conflict)",
      ylabpos = 2.5,
      xat = xat,
      xticklab = xlab,
      main = "Product Term Excluded")
lines(dem.lo, apply(pred.hi.noprod - pred.lo.noprod, 1, quantile, .05), lty = 2)
lines(dem.lo, apply(pred.hi.noprod - pred.lo.noprod, 1, quantile, .5), lwd = 3)
lines(dem.lo, apply(pred.hi.noprod - pred.lo.noprod, 1, quantile, .95), lty = 2)

## Product Term
hist(or$dem.lo, main = NA, axes = FALSE, col = "grey80", border = NA)
par(new = TRUE)
eplot(xlim = mm(dem.lo), ylim = ylim0,
      xlab = "Smaller Democracy Score",
      ylab = "Pr(Conflict)",
      ylabpos = 2.5,
      xat = xat,
      xticklab = xlab,
      main = "Product Term Included")
lines(dem.lo, apply(pred.hi.prod - pred.lo.prod, 1, quantile, .05), lty = 2)
lines(dem.lo, apply(pred.hi.prod - pred.lo.prod, 1, quantile, .5), lwd = 3)
lines(dem.lo, apply(pred.hi.prod - pred.lo.prod, 1, quantile, .95), lty = 2)
dev.off()