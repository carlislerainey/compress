
# Make sure that working directory is set properly, e.g.,
# setwd("~/Dropbox/projects/compress/")

# Clear workspace
rm(list = ls())

# Install the local verson of compactr to allow for log scales on the x-axis
#install.packages("devtools", dependencies = "Depends")
#devtools::install_github("carlislerainey/compactr")
#install.packages("arm")
# install.packages("texreg")

# Load packages
library(arm)
library(MASS)
library(compactr)
library(foreign)
library(texreg)

# Set seed
set.seed(892746)

# Do simulations
n.sims1 <- 2000  # number of simulations used to compute size
n.sims2 <- 2000  # number of Clarify-like simulation used to compute each p-value
n.iter <- 1000  # number of generic DGPs to simulate
source("R/do-sims.R")

# Do plots
source("R/do-plots.R")

# Do replication study
source("or-replication/R/or.R")
