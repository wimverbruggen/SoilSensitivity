download.soilGrid.psims <-  function(var,
                                     depth,
                                     product,
                                     long.min,
                                     long.max,
                                     lat.min,
                                     lat.max,
                                     resam.pix=240) {

  # var <- 'soc'
  # depth <- c('0-5')
  # product <- c ('mean')
  # long.min <- -80
  # long.max <- -79
  # lat.min <- -10
  # lat.max <- -9

  tmp.f <- tempfile(pattern = var, tmpdir = tempdir(), fileext = ".tiff")
  f <- paste0('https://maps.isric.org/mapserv?map=/map/',var,'.map&SERVICE=WCS&VERSION=2.0.1&REQUEST=GetCoverage&COVERAGEID=',
              var,'_',depth,'cm_',product,
              '&FORMAT=GEOTIFF_INT16&',
              'SUBSET=long(',long.min,',',long.max,')&',
              'SUBSET=lat(',lat.min,',',lat.max,')&',
              'SUBSETTINGCRS=http://www.opengis.net/def/crs/EPSG/0/4326&',
              'OUTPUTCRS=http://www.opengis.net/def/crs/EPSG/0/4326'
  )
  #download the map
  download.file(f, tmp.f, mode = 'wb')
  #read it into a raster
  r <- raster(tmp.f)
  #create a new raster for resizing it to psims res
  r2 <- raster(nrow=resam.pix, ncol=resam.pix)
  extent(r2) <- extent(r)
  # resample to resize
  r_resam <- raster::resample(r, r2, method = 'bilinear')
  unlink(tmp.f)
  return(raster::as.matrix(r_resam))
}
