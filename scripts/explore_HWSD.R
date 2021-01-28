rm(list = ls())

library(Hmisc)
library(raster)
library(rgdal)
library(dplyr)
library(SoilSensitivity)
library(ncdf4)
library(rhdf5)

# ED_REG_LATMIN = -10.5
# ED_REG_LATMAX = -0.5
# ED_REG_LONMIN = -60.5
# ED_REG_LONMAX = -50.5

ED_REG_LATMIN = -20
ED_REG_LATMAX = 15
ED_REG_LONMIN = -85
ED_REG_LONMAX = -35

HWSD <- raster('~/Downloads/hwsd.bil')
e <- extent(ED_REG_LONMIN,ED_REG_LONMAX,ED_REG_LATMIN,ED_REG_LATMAX)
HWSD.cropped <- raster::crop(HWSD, e)
HWSD.cropped.filled <- HWSD.cropped
HWSD.cropped.filled[HWSD.cropped.filled==0] <- NA
values <- unique(raster::extract(HWSD.cropped,e))

plot(HWSD.cropped.filled)

df <- mdb.get('~/Downloads/HWSD.mdb')
# contents(df)
# for(z in df) print(contents(z))
# HWSD.SMU <- mdb.get('~/Downloads/HWSD.mdb', tables='HWSD_SMU')
# mdb.get('~/Downloads/HWSD.mdb', tables='D_SWR')

HWSD.data <- mdb.get('~/Downloads/HWSD.mdb', tables='HWSD_DATA')

textures_HWSD <-
  HWSD.data %>% filter(MU.GLOBAL %in% values) %>% group_by(MU.GLOBAL) %>% summarise(mu = ID[which.max(SHARE)],
                                                                                    t.sand = as.vector(as.numeric(T.SAND[which.max(SHARE)]/100)),
                                                                                    t.silt = as.vector(as.numeric(T.SILT[which.max(SHARE)]/100)),
                                                                                    t.clay = as.vector(as.numeric(T.CLAY[which.max(SHARE)]/100)),
                                                                                    s.sand = as.vector(as.numeric(S.SAND[which.max(SHARE)]/100)),
                                                                                    s.silt = as.vector(as.numeric(S.SILT[which.max(SHARE)]/100)),
                                                                                    s.clay = as.vector(as.numeric(S.CLAY[which.max(SHARE)]/100))) %>% ungroup() %>%
  mutate(ntextsoil.t = components2ntext_soil(t.sand,t.silt,t.clay),
         ntextsoil.s = components2ntext_soil(s.sand,s.silt,s.clay))

all.ids <- HWSD.data %>% filter(MU.GLOBAL %in% values) %>% pull(ID)
selected.ids <- textures_HWSD %>% pull(mu)
which(!(all.ids %in% selected.ids))

rclmat <- matrix(c(textures_HWSD$MU.GLOBAL-0.001,0.001+textures_HWSD$MU.GLOBAL,textures_HWSD$ntextsoil.t),
                   ncol=3, byrow=FALSE)
rc <- reclassify(HWSD.cropped.filled, rclmat)

rclmat_s <- matrix(c(textures_HWSD$MU.GLOBAL-0.001,0.001+textures_HWSD$MU.GLOBAL,textures_HWSD$ntextsoil.s),
                 ncol=3, byrow=FALSE)
rc_s <- reclassify(HWSD.cropped.filled, rclmat_s)

HWSD.all <- stack(rc,rc_s)
names(HWSD.all) <- c('0-30','30-100')
raster.aggregate <- raster::aggregate(HWSD.all,1/c(res(HWSD.all)[1],res(HWSD.all)[2]),fun = get_mode)

rf <- writeRaster(raster.aggregate, filename=file.path(".","maps", "HWSD_all.grd"))

plot(HWSD.all)
plot(raster.aggregate)

filename = "/home/femeunier/Documents/raw_inputED/HWSD/HWSD_20S085W.h5"
data <- SoilSensitivity::rotate(as.matrix(raster.aggregate))

# h5file
system2("rm",filename)
h5createFile(filename)
h5createDataset(filename, "fao",dim(data), storage.mode = "integer", level=0)
h5write(data, file=filename, name="fao")
H5close()

r <- raster(filename)
plot(flip(raster(filename),2))

# fileheader
fileheader <- file.path("/home/femeunier/Documents/raw_inputED/HWSD/","HWSD_HEADER")
writeLines("#!/bin/bash -l",con = fileheader)
writeLines(text = paste(c(as.integer(dim(data)),1/res(raster.aggregate)[1],extent(raster.aggregate)[1],extent(raster.aggregate)[3]),
                        collapse = " "),
           con = fileheader)
write("------------------------------------------------------------------",file=fileheader,append=TRUE)
write("  nio,    njo,   nperdeg,     iwbego,     isbego,",file=fileheader,append=TRUE)
