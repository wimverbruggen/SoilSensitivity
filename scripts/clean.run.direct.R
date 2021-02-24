rm(list = ls())

library(dplyr)
library(tidyr)
library(PEcAnRTM)
library(purrr)
library(rrtm)
library(ED2scenarios)
library(PEcAn.ED2)
library(purrr)
library(ggplot2)
library(ggridges)
library(cowplot)
library(pracma)
library(BayesianTools)
library(raster)

ED_REG_LATMIN = -19.5
ED_REG_LATMAX =  13.5
ED_REG_LONMIN = -84.5
ED_REG_LONMAX = -30.5

GRID_RES = 1

X = seq(ED_REG_LONMIN,ED_REG_LONMAX,GRID_RES)
Y = seq(ED_REG_LATMIN,ED_REG_LATMAX,GRID_RES)

ref_dir <- "/data/gent/vo/000/gvo00074/pecan/output/other_runs/SoilSensitivity/run"
rundir <- "/data/gent/vo/000/gvo00074/pecan/output/other_runs/SoilSensitivity/run/grid"
ICdir <- "/data/gent/vo/000/gvo00074/pecan/output/other_runs/LSliana/IC/"
outdir <- "/kyukon/scratch/gent/vo/000/gvo00074/felicien/SoilSensitivity/out"

# load landmask here
land <- readRDS(file.path("maps","landmask.RDS"))


scenars <- c("SoilGrids_mean","SoilGrids_min","SoilGrids_max")


for(ix in seq(1,length(X))){
  for(iy in seq(1,length(Y))){
    clat = Y[iy]; clon = X[ix]

    if (land[which(rownames(land)==clon),which(colnames(land)==clat)] != 0){

      for (iscenar in seq(1,length(scenars))){
        run_name <- paste0("SoilSens_Amazon_IC_",scenars[iscenar],"_X_",abs(X[ix]),ifelse(X[ix]<0,"W","E"),
                           "_Y_",abs(Y[iy]),ifelse(Y[iy]<0,"S","N"))

        run_ref <- file.path(rundir,run_name)

        if (!dir.exists(run_ref)) next()

        if (file.exists(file.path(run_ref,"logfile.txt"))) system2("rm",file.path(run_ref,"logfile.txt"))
        if (file.exists(file.path(run_ref,"status.txt"))) system2("rm",file.path(run_ref,"status.txt"))
        if (file.exists(file.path(run_ref,"errorfile.txt"))) system2("rm",file.path(run_ref,"errorfile.txt"))
        if (file.exists(file.path(run_ref,"job.sh"))) system2("rm",file.path(run_ref,"job.sh*"))
      }
    }
  }
}

# scp /home/femeunier/Documents/projects/SoilSensitivity/scripts/clean.run.direct.R hpc:/data/gent/vo/000/gvo00074/felicien/R
