rm(list = ls())

library(raster)
library(lattice)

ED_REG_LATMIN = -20
ED_REG_LATMAX = 15
ED_REG_LONMIN = -85
ED_REG_LONMAX = -30

Delta_X = 5
Delta_Y = 5

X = seq(ED_REG_LONMIN,ED_REG_LONMAX,Delta_X)
Y = seq(ED_REG_LATMIN,ED_REG_LATMAX,Delta_Y)

grid <- expand.grid(lon=X, lat=Y)

files <- list()
compt <- 1
for (i in seq(1,nrow(grid))){
  clat = grid[i,"lat"];  clon = grid[i,"lon"]

  file <- file.path("/home/femeunier/Documents/raw_inputED/FAO",
                    paste0("FAO_",sprintf("%02d",abs(clat)),ifelse(clat < 0,"S","N"),sprintf("%03d",abs(clon)),ifelse(clon <0,"W","E"),".h5"))

  if (file.exists(file)){
    r <- flip(raster(file),2)
    r <- setExtent(r,extent(clon,clon+Delta_X,clat,clat+Delta_Y))
    files[[compt]] <- r
    compt <- compt + 1
  }
}

m <- do.call(merge, files)
plot(m)
raster.aggregate <- raster::aggregate(m,c(1/res(m)[1],1/res(m)[2]),fun = get_mode)
plot(raster.aggregate) #xlim = c(-60.5,-50.5), ylim = c(-10.5,-0.5))

#
# file2 <- "/home/femeunier/Documents/raw_inputED/FAO/FAO_05N055W.h5"
# r2 <- flip(raster(file2),2)
# r2 <- setExtent(r2,extent(-55,-50,5,10))
#
# file3 <- "/home/femeunier/Documents/raw_inputED/FAO/FAO_00N060W.h5"
# r3 <- flip(raster(file3),2)
# r3 <- setExtent(r3,extent(-60,-55,0,5))
#
# file4 <- "/home/femeunier/Documents/raw_inputED/FAO/FAO_00N055W.h5"
# r4 <- flip(raster(file4),2)
# r4 <- setExtent(r4,extent(-55,-50,0,5))
#





