aggregates <- function(raster1,raster2,res1,res2,probs = 0.5){

  nX = nrow(raster1)
  nY = ncol(raster1)
  # whichfun <- match.fun(paste0("which.",FUN))

  X = c(seq(1,nX,res1),nX)
  Y = c(seq(1,nY,res2),nY)

  values.out <- c()

  for(ix in seq(1,length(X)-1)){
    for(iy in seq(1,length(Y)-1)){
      cvalues <- getValuesBlock(raster1,X[ix],res1,Y[iy],res2)
      cvalues.t <-  getValuesBlock(raster2,X[ix],res1,Y[iy],res2)
      target <- quantile(cvalues,probs)
      pos <- which.min(abs(cvalues - as.numeric(target)))
      values.out <- c(values.out,mean(cvalues.t[pos]))
    }
  }

  raster.out <- raster::aggregate(raster1,
                                  res1,res2,
                                  fun = "mean")
  values(raster.out) <- values.out

  return(raster.out)
}
