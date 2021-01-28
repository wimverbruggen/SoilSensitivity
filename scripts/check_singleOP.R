rm(list = ls())

library(rhdf5)
library(dplyr)

source("~/Documents/ED2/R-utils/h5read_opt.r")

h5file <- file.path("~/Downloads/",
                    "history-S-2000-01-01-000000-g01.h5")

mymont    = lapply(h5read_opt(h5file),FUN=aperm)
names(mymont) <- gsub(x = names(mymont), pattern = "\\_", replacement = ".")
