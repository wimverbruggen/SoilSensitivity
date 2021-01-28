rm(list = ls())

library(stringr)
library(PEcAn.ED2)
library(rhdf5)
library(dplyr)

source("/data/gent/vo/000/gvo00074/felicien/R/h5read_opt.r")

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

# load landmask here
land <- readRDS("landmask.RDS")
land_IC <- readRDS("land_IC.RDS")

scenars <- c("ref","silt","clay")
scenars <- c("ref","silt","clay",
             "ref_nohydro","silt_nohydro","clay_nohydro")

scenars <- c("soilgrids_ref","soilgrids","soilgrids_SD")


df <- data.frame()
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
          h5file <- file.path(OP_dir,"history-S-2001-01-01-000000-g01.h5")

          ED2IN_ctime <- file.info(ED2INfile)$ctime

          if (file.exists(h5file) & (ED2IN_ctime < file.info(h5file)$ctime)){

            for (iyear in seq(2000,2001)){
              h5file <- file.path(OP_dir,paste0("history-S-",iyear,"-01-01-000000-g01.h5"))
              mymont    = lapply(h5read_opt(h5file),FUN=aperm)
              names(mymont) <- gsub(x = names(mymont), pattern = "\\_", replacement = ".")

              lat = mymont$LATITUDE
              lon = mymont$LONGITUDE
              AGB = sum(mymont$AGB.PY)
              ntext_soil_bt = mymont$NTEXT.SOIL[1]
              ntext_soil_tp = mymont$NTEXT.SOIL[mymont$NZG]

              df <- bind_rows(list(df,
                                   data.frame(lat,lon,
                                              AGB,
                                              ntext_soil_bt,ntext_soil_tp,
                                              scenario = scenars[iscenar],year = iyear)))
            }
          }
        }
      }
    }
  }
}

saveRDS(df,"df_SoilSens.RDS")

# scp /home/femeunier/Documents/projects/SoilSensitivity/scripts/analyze_SoilSens.R hpc:/data/gent/vo/000/gvo00074/felicien/R

