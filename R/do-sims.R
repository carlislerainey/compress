
# Make sure that working directory is set properly, e.g.,
# setwd("~/Dropbox/projects/compress/")

# Clear workspace
rm(list = ls())

# Set seed
set.seed(1947130)

# Create directory for output
dir.create(path = "output", showWarnings = FALSE)

# Do each simulation
source("R/sims-fixed.R", echo = TRUE)
source("R/sims-fixed-few1s.R", echo = TRUE)
source("R/sims-generic.R", echo = TRUE)
source("R/sims-pwr.R", echo = TRUE)
