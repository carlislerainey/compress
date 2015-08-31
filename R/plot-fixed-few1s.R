
# Make sure that working directory is set properly, e.g.,
# setwd("~/Dropbox/projects/compress/")

# Load the simulations.
load("output/sims-fixed-few1s.RData")

###############################################################################
## Draw the plot
###############################################################################

# Set the graphical parameters
xat <- c(100, 1000, 10000, 100000)
xticklab = c("100", "1,000", "10,000", "100,000")
col.label.at <- 1:3
plot.counter <- 0
y.axis.at = c(1, 4, 7) 
dist.x.names <- c("X ~ Beta(1, 1)", "X ~ Beta(2, 2)", "X ~ Beta(0.1, 1)")
dist.x <- list(c(1,1), c(2,2), c(.1,1))
change.x.names <- c("25th to 75th", "min to max", "5th to 15th\npercentile")
change.x <- list(c(.25, .75), c(0,1), c(.05, .15))
sample.size <- round(exp(seq(log(100), log(100000), length.out = 10)))

svg("doc/fig/fig-fixed-few1s.svg", height = 4, width = 8, family = "serif")
par(mfrow = c(length(change.x.names), length(dist.x.names)), xlog = T,
    oma = c(3,14,5,1), mar = c(.5,1,.5,1), family = "serif")

# Create the plots.
for (dist in 1:length(dist.x.names)) {
  for (change in 1:length(change.x.names)) {
    plot.counter <- plot.counter + 1
    p.noprod <- noprod.res[ , dist, change]
    p.prod <- prod.res[ , dist, change]
    n <- sample.size
    eplot(xlim = c(min(n), max(n)), ylim = c(0,1),
          xlab = "Sample Size", ylab = "Pr(Find Interaction)", 
          log = "x", ylabpos = 2, xat = xat, xticklab = xticklab)
    abline(h = 0.05, col = "grey70")
    if (plot.counter %in% y.axis.at) {
      whch <- which(plot.counter == y.axis.at)
      mtext(side = 2, paste("Change X from\n", change.x.names[whch], ".", sep = ""), 
            cex = .8, line = 9, adj = 0.5, las = 1)
    }
    if (plot.counter %in% col.label.at) {
      whch <- which(plot.counter == col.label.at)
      mtext(side = 3, dist.x.names[whch], line = 3.5, cex = .8)
    }    
    lines(n, p.noprod, lwd = 2)
    lines(n, p.prod, lwd = 2, lty = 2)
  }
}
dev.off()
