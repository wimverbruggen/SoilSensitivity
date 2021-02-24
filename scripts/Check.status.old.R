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
library(rhdf5)
library(stringr)

source("/data/gent/vo/000/gvo00074/felicien/R/h5read_opt.r")

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

scenars <- c("SoilGrids_mean")

land <- readRDS(file.path("maps","landmask.RDS"))

status.all <- data.frame()

for(ix in seq(1,length(X))){
  for(iy in seq(1,length(Y))){
    clat = Y[iy]; clon = X[ix]

    if (land[which(rownames(land)==clon),which(colnames(land)==clat)] != 0){

      for (iscenar in seq(1,length(scenars))){

        run_name <- paste0("SoilSens_Amazon_",scenars[iscenar],"_X_",abs(X[ix]),ifelse(X[ix]<0,"W","E"),
                           "_Y_",abs(Y[iy]),ifelse(Y[iy]<0,"S","N"))

        run_ref <- file.path(rundir,run_name)
        out_ref <- file.path(outdir,run_name)

        # run_ref <- "/data/gent/vo/000/gvo00074/pecan/output/other_runs/SoilSensitivity/run/grid/SoilSens_Amazon_SoilGrids_mean_X_78.5W_Y_5.5S"

        job.file <- list.files(path = run_ref,pattern = "^job\\.sh$",full.names = TRUE)

        if (length(job.file) >0){
          job.file.lines <- readLines(job.file[1])
          directories <- sapply(strsplit(grep("cd *",job.file.lines,value = TRUE)," "),function(x) x[2])

          lats <- lons <- c()
          for (idir in seq(1,length(directories))){
            ed2in <- read_ed2in(file.path(directories[idir],"ED2IN"))
            lats <- c(lats,ed2in$POI_LAT)
            lons <- c(lons,ed2in$POI_LON)
          }

          job.files.o <- list.files(path = run_ref,pattern = "job.sh.o.*",full.names = TRUE)

          df.status <- data.frame(dir = directories,
                                  status = as.character("Not run"),stringsAsFactors = FALSE,
                                  lat = lats,
                                  lon = lons)

          if (length(job.files.o)>0){

            details <- file.info(job.files.o)
            job.files.o <- details[with(details, order(as.POSIXct(mtime),decreasing = TRUE)), ]
            Filelines <- readLines(job.files.o[1])

            startLines <- c(grep("Copying namelist",Filelines),Inf)

            errors <- grep("ED execution halts",Filelines)
            if (length(errors)>0) names(errors) <- rep("error",length(errors))
            errors <- errors[order(findInterval(errors,startLines))[!duplicated(sort(findInterval(errors,startLines)))]]

            success <- grep("ED-2.2 execution ends",Filelines)
            if (length(success)>0) names(success) <- rep("success",length(success))

            if (max(c(errors,success)) != (length(Filelines) - 1)){
              Endline <- length(Filelines)
              names(Endline) <- as.numeric(strsplit(strsplit(Filelines[length(Filelines)]," ")[[1]][6],"/")[[1]][3])
            } else{
              Endline <- NULL
            }

            all <- sort(c(errors,success,Endline))

            for (idir in seq(1,length(all))){
              df.status[df.status$dir == directories[idir],"status"] <-  names(all)[idir]
            }
          }

          status.all <- bind_rows(list(status.all,
                                       df.status))
        }
      }
    }
  }
}

saveRDS(status.all,"status.all.RDS")

# scp /home/femeunier/Documents/projects/SoilSensitivity/scripts/Check.status.R hpc:/data/gent/vo/000/gvo00074/felicien/R

# ED execution halts (see previous error message)
# ED-2.2 execution ends
