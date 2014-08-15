
# Make sure that working directory is set properly, e.g.,
# setwd("~/Dropbox/projects/compress/")

# Clear workspace
rm(list = ls())

# Load the simulations.
load("output/sims-pwr.RData")

# Install the local verson of compactr to allow for log scales on the x-axis
library(devtools)
install_github(repo = "compactr", 
               username = "carlislerainey")
library(compactr)

###############################################################################
## Draw the plot
###############################################################################

# Set the graphical parameters
xat <- c(100, 1000, 10000, 100000)
xticklab = c("100", "1,000", "10,000", "100,000")
col.label.at <- 1:3
plot.counter <- 0
y.axis.at = c(1, 4, 7) 
beta.x <- c(.75, 1.5, 3)
beta.z <- c(.75, 1.5, 3)
sample.size <- round(exp(seq(log(100), log(100000), length.out = 10)))

pdf("doc/fig/pwr.pdf", height = 4, width = 8, family = "serif")
par(mfrow = c(length(beta.z), length(beta.x)), xlog = T,
    oma = c(3,8,4,1), mar = c(.5,1,1,1), family = "serif")

# Create the plots.
for (b.x in 1:length(beta.x)) {
  for (b.z in 1:length(beta.z)) {
    beta <- c(0, beta.x[b.x], beta.z[b.z])
    fd.hi <- round(plogis(sum(beta*c(1, .75, 1))) - plogis(sum(beta*c(1, .25, 1))), 2)
    fd.lo <- round(plogis(sum(beta*c(1, .75, 0))) - plogis(sum(beta*c(1, .25, 0))), 2)
    sd <- fd.hi - fd.lo
    plot.counter <- plot.counter + 1
    p.noprod <- noprod.res[ , b.x, b.z]
    p.prod <- prod.res[ , b.x, b.z]
    n <- sample.size
    eplot(xlim = c(min(n), max(n)), ylim = c(0,1),
          xlab = "Sample Size", ylab = "Pr(Find Interaction)", 
          log = "x", ylabpos = 2, xat = xat, xticklab = xticklab,
          main = paste("Second-Difference = ", 
                       sprintf("%.2f", fd.hi), " - ", 
                       sprintf("%.2f", fd.lo), " = ", 
                       sprintf("%.2f", sd), sep = ""))
    abline(h = 0.05, col = "grey70")
    if (plot.counter %in% y.axis.at) {
      whch <- which(plot.counter == y.axis.at)
      mtext(side = 2, bquote(beta[x] ==  .(beta.x[b.x])), 
            cex = .8, line = 6, adj = 0.5, las = 1)
    }
    if (plot.counter %in% col.label.at) {
      whch <- which(plot.counter == col.label.at)
      mtext(side = 3, bquote(beta[z] ==  .(beta.z[b.z])), 
            line = 2, cex = .8)
    }    
    lines(n, p.noprod, lwd = 2)
    lines(n, p.prod, lwd = 2, lty = 2)
  }
}
dev.off()
