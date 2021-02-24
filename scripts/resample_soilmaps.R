rm(list = ls())

library(SoilSensitivity)
library(lattice)
library(raster)
library(purrr)
library(tidyr)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(stringr)

files <- c(
  "soilgrid_top.clay_max.grd",
  "soilgrid_top.clay_mean.grd",
  "soilgrid_top.clay_min.grd",
  "soilgrid_top.sand_max.grd",
  "soilgrid_top.sand_mean.grd",
  "soilgrid_top.sand_min.grd",
  "soilgrid_top.soc_mean.grd",
  "soilgrid_top.soc_min.grd",
  "soilgrid_top.soc_min.grd"
)

ED_REG_LATMIN = -19.5
ED_REG_LATMAX =  13.5
ED_REG_LONMIN = -84.5
ED_REG_LONMAX = -30.5

GRID_RES = 0.5

X = seq(ED_REG_LONMIN,ED_REG_LONMAX,GRID_RES)
Y = seq(ED_REG_LATMIN,ED_REG_LATMAX,GRID_RES)

R = raster(nrows=length(Y), ncols=length(X),
           xmn=ED_REG_LONMIN-0.25, xmx=ED_REG_LONMAX-0.25,
           ymn=ED_REG_LATMIN-0.25, ymx=ED_REG_LATMAX + 0.25, vals=NULL)

par(mfrow = c(2,1))

cuts=seq(0,1,length.out = 11) #set breaks
pal <- colorRampPalette(c("white","black"))

for (ifile in seq(1, length(files))) {
  rcfile <- file.path(".", "maps", files[ifile])
  rc <- brick(rcfile, expand = TRUE)
  rcprim <- resample(rc, R)
  plot(rc, breaks = cuts, col = pal(11))
  plot(rcprim, breaks = cuts, col = pal(11))
  writeRaster(rcprim,
              filename = file.path(
                ".",
                "maps",
                paste0(tools::file_path_sans_ext(files[ifile]), "_resampled.grd")
              ),
              overwrite = TRUE)
}
