components2ntext_soil <- function(sand_all,silt_all,clay_all){

  # print(length(sand_all))
  sand_all <- sand_all*100; silt_all <- silt_all*100; clay_all <- clay_all*100

  ntext_soil_all <- NA*sand_all


  N = length(sand_all)

  for (i in seq(1,N)){
    sand <- sand_all[i]; silt <- silt_all[i]; clay <- clay_all[i]

    # print(c(sand,silt,clay))
    if (any(is.na(c(sand,silt,clay)))) {
      ntext_soil <- NA
      next
    } else if (all(c(sand,silt,clay) == 0) | sum(c(sand,silt,clay)) < 99 ){
      ntext_soil = NA
    }

    if (any(c(sand,silt,clay)>100) | any(c(sand,silt,clay)<0) | round(sum(c(sand,silt,clay)))>100){
      stop(paste(c("Error in the soil texture class:",paste(c(sand,silt,clay))),collapse = " "))
    } else if (sand > 85.0 + 0.5 * clay){
      ntext_soil = 1 #Sa Sand
    } else if (sand > 70.0 + clay) {
      ntext_soil = 2 #LSa Loamy sand
    } else if ((clay <= 20.0 & sand > 52.5) | (clay <= 7.5 & silt <= 50.0)){
      ntext_soil = 3 #SaL Sandy loam
    } else if ((clay <= 27.5 & silt > 50.0 & silt <= 80.0) | (silt >  80.0 & clay > 12.5)) {
      ntext_soil = 4 #SiL Silt loam
    } else if (clay > 7.5 & clay <= 27.5 & silt > 27.5 & silt <= 50.0 & sand <= 52.5){
      ntext_soil = 5 #L Loam
    } else if (clay > 20.0 & clay <= 35.0 & silt <= 27.5 & sand > 45.0){
      ntext_soil = 6 #SaCL Sandy clay loam
    } else if (clay > 27.5 & clay <= 40.0 & sand <= 20.0) {
      ntext_soil = 7 #SiCL Silty clay loam
    } else if (clay > 27.5 & clay <= 40.0 & sand > 20.0 & sand <= 45.0){
      ntext_soil = 8 #CL Clayey loam
    } else if (clay > 35.0 & sand > 45.0){
      ntext_soil = 9 #SaC Sandy clay
    } else if (clay > 40.0 & silt > 40.0){
      ntext_soil = 10 #SiC Silty clay
    } else if (clay <= 70.0 & sand <= 30.0 & silt <= 30.0){
      ntext_soil = 11 #C Clay
    } else if (silt > 80.0 & clay <= 12.5){
      ntext_soil = 14 #Si Silt
    } else if (clay > 70.0){
      ntext_soil = 15 #CC Heavy clay
    } else if (clay > 40.0 & sand > 30.0 & sand <= 45.0){
      ntext_soil = 16 #CSa Clayey sand
    } else if ( clay > 40.0 & silt > 30.0 & silt <= 40.0){
      ntext_soil = 17 #CSi Clayey silt
    } else {
      stop("Error in the soil texture class")
    }

    ntext_soil_all[i] <- ntext_soil
  }

  return(ntext_soil_all) # Sand, silt, clay
}












