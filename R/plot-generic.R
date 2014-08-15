

# Make sure that working directory is set properly, e.g.,
# setwd("~/Dropbox/projects/compress/")

# Clear workspace
rm(list = ls())

load("output/sims-generic.RData")

prod <- res[, "prod"]
noprod <- res[, "noprod"]


library(compactr)
pdf("doc/fig/fig-scatter.pdf", height = 3.5, width = 5.5)
par(mfrow = c(1,1), mar = c(.75,.75,.75,.75), oma = c(4,4,1,1),
    family = "serif")
eplot(xlim = c(0, 1), ylim = c(0, 1),
      xlab = "Probability of Finding Interaction\nwithout a Product Term",
      ylab = "Probability of Finding Interaction\nwith a Product Term",
      xlabpos = 2.5, ylabpos = 2)
points(noprod, prod, col = rgb(0,0,0,.2))
abline(v = 0.05, lwd = 3)
abline(h = 0.05, lwd = 3)
abline(a = 0, b = 1, lwd = 3)
dev.off()

pdf("doc/fig/fig-hist.pdf", height = 3, width = 8)
par(mfrow = c(1,2), mar = c(.75,.75,.75,.75), oma = c(3,3,1,1), 
    family = "serif")
eplot(xlim = c(0, 1), ylim = c(0, 100),
      xlab = "Probability of Finding Interaction\nwithout a Product Term",
      ylab = "Percent of Simulations",
      xlabpos = 2.5, ylabpos = 2)
h <- hist(noprod, plot = FALSE)
h$counts <- 100*h$counts/nrow(res)
plot(h, add = TRUE)
text(h$mids[2], h$counts[1], 
     paste(round(h$counts[1], 1), "%", sep = ""), cex = .7)
text(h$mids[9], h$counts[10], 
     paste(round(h$counts[10], 1), "%", sep = ""), cex = .7)


eplot(xlim = c(0, 1), ylim = c(0, 100),
      xlab = "Probability of Finding Interaction\nwith a Product Term",
      xlabpos = 2.5)
h <- hist(prod, plot = FALSE)
h$counts <- 100*h$counts/nrow(res)
plot(h, add = TRUE)
text(h$mids[2], h$counts[1], 
     paste(round(h$counts[1], 1), "%", sep = ""), cex = .7)
dev.off()