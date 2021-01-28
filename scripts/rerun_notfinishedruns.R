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

ED_REG_LATMIN = -10.5
ED_REG_LATMAX = -0.5
ED_REG_LONMIN = -60.5
ED_REG_LONMAX = -50.5

GRID_RES = 1

X = seq(ED_REG_LONMIN,ED_REG_LONMAX,GRID_RES)
Y = seq(ED_REG_LATMIN,ED_REG_LATMAX,GRID_RES)

ref_dir <- "/data/gent/vo/000/gvo00074/pecan/output/other_runs/SoilSensitivity/run"
rundir <- "/data/gent/vo/000/gvo00074/pecan/output/other_runs/SoilSensitivity/run/grid"
ICdir <- "/data/gent/vo/000/gvo00074/pecan/output/other_runs/LSliana/IC/"
outdir <- "/kyukon/scratch/gent/vo/000/gvo00074/felicien/SoilSensitivity/out"

list_dir <- list()
# load landmask here
land <- readRDS("landmask.RDS")
land_IC <- readRDS("land_IC.RDS")

Nsimuperjob = 4
isimu = 0
scenars <- c("ref","silt","clay")

for(ix in seq(1,length(X))){
  for(iy in seq(1,length(Y))){
    clat = Y[iy]; clon = X[ix]

    if (land[which(rownames(land_IC)==clat),
             which(colnames(land_IC)==clon)]){

      for (iscenar in seq(1,length(scenars))){

        run_name <- paste0("SoilSens_Amazon_",scenars[iscenar],"_X_",abs(X[ix]),ifelse(X[ix]<0,"W","E"),
                           "_Y_",abs(Y[iy]),ifelse(Y[iy]<0,"S","N"))

        run_ref <- file.path(rundir,run_name)
        out_ref <- file.path(outdir,run_name)

        ED2INfile <- file.path(run_ref,"ED2IN")

        if (file.exists(ED2INfile)){
          ED2IN <- read_ed2in(ED2INfile)
          OP_dir <- dirname(ED2IN$SFILOUT)

          h5file <- file.path(OP_dir,"history-S-2005-01-01-000000-g01.h5")

          ED2IN_ctime <- file.info(ED2INfile)$ctime
          h5file_ctime <- file.info(h5file)$ctime

          if (!file.exists(h5file) | h5file_ctime < ED2IN_ctime){

            isimu = isimu + 1

            if (isimu == 1){
              isfirstjob = TRUE
              dir_joblauncher = run_ref
              list_dir[[run_name]] = run_ref
            } else{
              isfirstjob = FALSE
            }

            # job.sh
            write_joblauncher_noR(file =  file.path(dir_joblauncher,"job.sh"),
                                  nodes = 1,ppn = 1,mem = 16,walltime = 12,
                                  prerun = "ml purge ; ml UDUNITS/2.2.26-intel-2018a R/3.4.4-intel-2018a-X11-20180131 HDF5/1.10.1-intel-2018a; ulimit -s unlimited",
                                  CD = run_ref,
                                  ed_exec = "/user/scratchkyukon/gent/gvo000/gvo00074/felicien/ED2/ED/build/ed_2.1-opt-master-2bb6872",
                                  ED2IN = "ED2IN",
                                  firstjob = isfirstjob)

            if (isimu == Nsimuperjob){
              isimu = 0
            }
          }

        }
      }
    }
  }
}

dumb <- write_bash_submission(file = file.path(rundir,"all_jobs.sh"),
                              list_files = list_dir,
                              job_name = "job.sh")

# scp /home/femeunier/Documents/projects/SoilSensitivity/scripts/rerun_notfinishedruns.R hpc:/data/gent/vo/000/gvo00074/felicien/R

