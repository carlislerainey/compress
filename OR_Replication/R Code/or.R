rm(list = ls())
setwd("~/Dropbox/Projects/Compress")

library(arm)
library(compactr)

# Data available at 
#   http://pantheon.yale.edu/~brusset/io_dta.zip
or <- read.csv("OR_Replication/Data/or.csv")

# compute lower democracy score
or$dem.lo <- apply(cbind(or$demauta, or$demautb), 1, min)

# glm version of their gee on pp. 314 with no product term
m1 <- glm(dispute1 ~ allies + lcaprat2 + dem.lo + noncontg + logdstab + minrpwrs,
          family = "binomial", data = or)

# glm version of their gee on pp. 314 with no product terms
m2 <- glm(dispute1 ~ allies + lcaprat2 + dem.lo*noncontg + dem.lo*logdstab + minrpwrs,
          family = "binomial", data = or)

display(m1); display(m2)
BIC(m1, m2)


## The effect of democracy

# x.lo and x.hi for no product term
dem.lo <- seq(-10, 10, length.out = 100)
x.lo <- x.hi <- cbind(1, 
                      median(or$allies),
                      median(or$lcaprat2),
                      dem.lo, 
                      -1, #noncontg, coded -1, 0
                      median(or$logdstab[or$noncontg == 0]),
                      median(or$minrpwrs))
x.hi[, 5] <- 0 # again, the var is coded -1, 0

sim1 <- coef(sim(m1, n = 1000))

pred.lo <- plogis(x.lo%*%t(sim1))
pred.hi <- plogis(x.hi%*%t(sim1))

par(mfrow = c(1,2), mar = c(.75,.75,.75,.75), oma = c(3,3,1,1))

###############################################################
## Predicted Probabilities (No Product Term)
###############################################################

eplot(xlim = mm(dem.lo), ylim = mm(c(pred.lo, pred.hi)),
      xlab = "Lowest Democracy Score",
      ylab = "Pr(Conflict)",
      ylabpos = 2.5,
      main = "Non-contiguous States")
lines(dem.lo, apply(pred.hi, 1, quantile, .05), lty = 2)
lines(dem.lo, apply(pred.hi, 1, quantile, .5), lwd = 3)
lines(dem.lo, apply(pred.hi, 1, quantile, .95), lty = 2)

eplot(xlim = mm(dem.lo), ylim = mm(c(pred.lo, pred.hi)),
      xlab = "Lowest Democracy Score",
      ylab = "Pr(Conflict)",
      ylabpos = 2.5,
      main = "Contiguous States")
lines(dem.lo, apply(pred.lo, 1, quantile, .05), lty = 2)
lines(dem.lo, apply(pred.lo, 1, quantile, .5), lwd = 3)
lines(dem.lo, apply(pred.lo, 1, quantile, .95), lty = 2)

###############################################################
## First Difference as Democracy Varies (No Product Term)
###############################################################

par(mfrow = c(1,1), mar = c(.75,.75,.75,.75), oma = c(3,4,1,1))

eplot(xlim = mm(dem.lo), ylim = mm(pred.lo - pred.hi),
      xlab = "Lowest Democracy Score",
      ylab = "Effect of Contiguity\non Pr(Conflict)",
      ylabpos = 2.5)
lines(dem.lo, apply(pred.lo - pred.hi, 1, quantile, .05), lty = 2)
lines(dem.lo, apply(pred.lo - pred.hi, 1, quantile, .5), lwd = 3)
lines(dem.lo, apply(pred.lo - pred.hi, 1, quantile, .95), lty = 2)



