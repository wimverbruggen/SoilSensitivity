rm(list = ls())

library(SoilSensitivity)

# ncfile <- file.path("/data/gent/vo/000/gvo00074/ED_common_data/met/CRUNCEP/ED2","1901JAN.h5")
ncfile <- file.path("/home/femeunier/Downloads/","1901APR.h5")

lats <- seq(-20,15,0.5)
lons <- seq(-90,-30,0.5)

nc <- ncdf4::nc_open(ncfile)
var <- try(ncdf4::ncvar_get(nc, "tmp"), silent = TRUE)
ncdf4::nc_close(nc)

map.var <- var[20,,]
landmask <- map.var
landmask[!is.na(map.var)] <- TRUE
landmask[is.na(map.var)] <- FALSE
# landmask <- as.logical(landmask)

# image(1:121,1:71,rotate(rotate(apply(landmask,2, rev))))
image(lons,lats,rotate(landmask))

LM <- rotate(landmask)

colnames(LM) <- as.numeric(lats)
rownames(LM) <- as.numeric(lons)

colnames(landmask) <- as.numeric(lons)
rownames(landmask) <- as.numeric(lats)

# saveRDS(object = landmask,
#         file = file.path(".","maps","landmask.RDS"))


