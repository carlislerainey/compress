
# Make sure that working directory is set properly, e.g.,
# setwd("~/Dropbox/projects/compress/")

# Clear workspace
rm(list = ls())

svg("doc/fig/beta11.svg", height = 1, width = 1.5, family = "serif")
par(mfrow = c(1,1), mar = c(0.1,0.1,0.1,0.1), oma = c(0,0,0,0))
curve(dbeta(x,1,1), axes = FALSE, lwd = TRUE)
box()
dev.off()

svg("doc/fig/beta22.svg", height = 1, width = 1.5, family = "serif")
par(mfrow = c(1,1), mar = c(0.1,0.1,0.1,0.1), oma = c(0,0,0,0))
curve(dbeta(x,2,2), axes = FALSE, lwd = TRUE)
box()
dev.off()

svg("doc/fig/beta011.svg", height = 1, width = 1.5, family = "serif")
par(mfrow = c(1,1), mar = c(0.1,0.1,0.1,0.1), oma = c(0,0,0,0))
curve(dbeta(x,.1,1), axes = FALSE, lwd = TRUE)
box()
dev.off()