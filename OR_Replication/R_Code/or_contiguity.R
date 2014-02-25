rm(list = ls())
setwd("~/Dropbox/Projects/Compress")

library(arm)
library(compactr)
library(foreign)

# Data available at 
#   http://pantheon.yale.edu/~brusset/io_dta.zip
or <- read.dta("OR_Replication/Data/or.dta")

# compute lower democracy score
or$dem.lo <- apply(cbind(or$demauta, or$demautb), 1, min)

# glm version of their gee on pp. 314 with no product term
m.noprod <- glm(dispute1 ~ allies + lcaprat2 + dem.lo + noncontg + logdstab + minrpwrs,
          family = "binomial", data = or)

# glm version of their gee on pp. 314 with no product terms
m.prod <- glm(dispute1 ~ allies + lcaprat2 + dem.lo*noncontg + dem.lo*logdstab + minrpwrs,
          family = "binomial", data = or)

display(m.noprod, detail = TRUE)
display(m.prod, detail = TRUE)
BIC(m.noprod, m.prod)


###############################################################
## Simulate Quantities of Interest
###############################################################

## No Product Term
dem.lo <- seq(-10, 10, length.out = 100)
x.lo <- x.hi <- cbind(1, 
                      median(or$allies),
                      median(or$lcaprat2),
                      dem.lo, 
                      -1, #noncontg, coded -1, 0 # -1 = contig
                      median(or$logdstab[or$noncontg == -1]),
                      median(or$minrpwrs))
x.hi[, 5] <- 0 # again, the var is coded -1, 0 # 0 = noncontig
sim.noprod <- coef(sim(m.noprod, n = 1000))
pred.lo.noprod <- plogis(x.lo%*%t(sim.noprod))
pred.hi.noprod <- plogis(x.hi%*%t(sim.noprod))

## No Product Term
dem.lo <- seq(-10, 10, length.out = 100)
x.lo <- cbind(x.lo, x.lo[, 4]*x.lo[, 5], x.lo[, 4]*x.lo[, 6])
x.hi <- cbind(x.hi, x.hi[, 4]*x.hi[, 5], x.hi[, 4]*x.lo[, 6])
sim.prod <- coef(sim(m.prod, n = 1000))
pred.lo.prod <- plogis(x.lo%*%t(sim.prod))
pred.hi.prod <- plogis(x.hi%*%t(sim.prod))

# Clean up
rm(x.lo, x.hi)

###############################################################
## Predicted Probabilities Plot
###############################################################

## Graphical Parameters
par(mfrow = c(2,2), mar = c(.75,.75,.75,.75), oma = c(3,9,1,1))
ylim0 <- mm(c(pred.lo.noprod, pred.hi.noprod, pred.lo.prod, pred.hi.prod))
            
## Product Term Excluded
hist(or$dem.lo[or$noncontg == 0], main = NA, axes = FALSE, col = "grey80", border = NA)
par(new = TRUE)
eplot(xlim = mm(dem.lo), ylim = ylim0,
      xlab = "Lowest Democracy Score",
      ylab = "Pr(Conflict)",
      ylabpos = 2.5,
      main = paste("Non-contiguous States (N = ", sum(or$noncontg == 0), ")", sep = ""))
lines(dem.lo, apply(pred.hi.noprod, 1, quantile, .05), lty = 2)
lines(dem.lo, apply(pred.hi.noprod, 1, quantile, .5), lwd = 3)
lines(dem.lo, apply(pred.hi.noprod, 1, quantile, .95), lty = 2)
text(-22, sum(ylim0)/2, "Product Term\nExcluded", xpd = NA)

hist(or$dem.lo[or$noncontg == -1], main = NA, axes = FALSE, col = "grey80", border = NA)
par(new = TRUE)
eplot(xlim = mm(dem.lo), ylim = ylim0,
      xlab = "Lowest Democracy Score",
      ylab = "Pr(Conflict)",
      ylabpos = 2.5,
      main = paste("Contiguous States (N = ", sum(or$noncontg == -1), ")", sep = ""))
lines(dem.lo, apply(pred.lo.noprod, 1, quantile, .05), lty = 2)
lines(dem.lo, apply(pred.lo.noprod, 1, quantile, .5), lwd = 3)
lines(dem.lo, apply(pred.lo.noprod, 1, quantile, .95), lty = 2)

## Product Term Included
hist(or$dem.lo[or$noncontg == 0], main = NA, axes = FALSE, col = "grey80", border = NA)
par(new = TRUE)
eplot(xlim = mm(dem.lo), ylim = ylim0,
      xlab = "Lowest Democracy Score",
      ylab = "Pr(Conflict)",
      ylabpos = 2.5)
lines(dem.lo, apply(pred.hi.prod, 1, quantile, .05), lty = 2)
lines(dem.lo, apply(pred.hi.prod, 1, quantile, .5), lwd = 3)
lines(dem.lo, apply(pred.hi.prod, 1, quantile, .95), lty = 2)
text(-22, sum(ylim0)/2, "Product Term\nIncluded", xpd = NA)

hist(or$dem.lo[or$noncontg == -1], main = NA, axes = FALSE, col = "grey80", border = NA)
par(new = TRUE)
eplot(xlim = mm(dem.lo), ylim = ylim0,
      xlab = "Lowest Democracy Score",
      ylab = "Pr(Conflict)",
      ylabpos = 2.5)
lines(dem.lo, apply(pred.lo.prod, 1, quantile, .05), lty = 2)
lines(dem.lo, apply(pred.lo.prod, 1, quantile, .5), lwd = 3)
lines(dem.lo, apply(pred.lo.prod, 1, quantile, .95), lty = 2)

###############################################################
## First Difference as Democracy Varies Plot (No Product Term)
###############################################################

## Graphical Parameters
par(mfrow = c(1,2), mar = c(.75,.75,.75,.75), oma = c(3,4,1,1))
ylim0 <- mm(c(pred.lo.noprod - pred.hi.noprod, pred.lo.noprod - pred.hi.noprod))

## No Product Term
hist(or$dem.lo, main = NA, axes = FALSE, col = "grey80", border = NA)
par(new = TRUE)
eplot(xlim = mm(dem.lo), ylim = ylim0,
      xlab = "Lowest Democracy Score",
      ylab = "Effect of Contiguity\non Pr(Conflict)",
      ylabpos = 2.5,
      main = "No Product Term")
lines(dem.lo, apply(pred.lo.noprod - pred.hi.noprod, 1, quantile, .05), lty = 2)
lines(dem.lo, apply(pred.lo.noprod - pred.hi.noprod, 1, quantile, .5), lwd = 3)
lines(dem.lo, apply(pred.lo.noprod - pred.hi.noprod, 1, quantile, .95), lty = 2)

## Product Term
hist(or$dem.lo, main = NA, axes = FALSE, col = "grey80", border = NA)
par(new = TRUE)
eplot(xlim = mm(dem.lo), ylim = ylim0,
      xlab = "Lowest Democracy Score",
      ylab = "Effect of Contiguity\non Pr(Conflict)",
      ylabpos = 2.5,
      main = "Product Term")
lines(dem.lo, apply(pred.lo.prod - pred.hi.prod, 1, quantile, .05), lty = 2)
lines(dem.lo, apply(pred.lo.prod - pred.hi.prod, 1, quantile, .5), lwd = 3)
lines(dem.lo, apply(pred.lo.prod - pred.hi.prod, 1, quantile, .95), lty = 2)

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
par(mfrow = c(1,1), mar = c(.75,.75,.75,.75), oma = c(3,1,1,1))
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

