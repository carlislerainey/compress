
# Make sure that working directory is set properly, e.g.,
# setwd("~/Dropbox/projects/compress/")

# Clear workspace
rm(list = ls())

# Do each simulation
source("R/plot-fixed-illustration.R", echo = TRUE)
source("R/plot-fixed.R", echo = TRUE)
source("R/plot-fixed-few1s.R", echo = TRUE)
source("R/plot-betas.R", echo = TRUE)
source("R/plot-appendix.R", echo = TRUE)
source("R/plot-generic.R", echo = TRUE)
source("R/plot-example-cis.R", echo = TRUE)

