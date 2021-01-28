ntext_soil2components <- function(ntext_soil_all){


  sand_all <- silt_all <- clay_all <- NA*ntext_soil_all
  N <- length(ntext_soil_all)

  for (i in seq(1,N)){
    ntext_soil <- ntext_soil_all[i]
    if (ntext_soil == 1){ #Sa Sand
      text = c(0.920,0.050,0.030)
    } else if (ntext_soil == 2){ #LSa Loamy sand
      text = c(0.825,0.115,0.060)
    } else if (ntext_soil == 3){ #SaL Sandy loam
      text = c(0.660,0.230,0.110)
    } else if (ntext_soil == 4){ #SiL Silt loam
      text = c(0.200,0.640,0.160)
    } else if (ntext_soil == 5){ #L Loam
      text = c(0.410,0.420,0.170)
    } else if (ntext_soil == 6){ #SaCL Sandy clay loam
      text = c(0.590,0.140,0.270)
    } else if (ntext_soil == 7){ #SiCL Silty clay loam
      text = c(0.100,0.560,0.340)
    } else if (ntext_soil == 8){ #CL Clayey loam
      text = c(0.320,0.340,0.340)
    } else if (ntext_soil == 9){ #SaC Sandy clay
      text = c(0.520,0.060,0.420)
    } else if (ntext_soil == 10){ #SiC Silty clay
      text = c(0.060,0.470,0.470)
    } else if (ntext_soil == 11){ #C Clay
      text = c(0.200,0.200,0.600)
    } else if (ntext_soil == 14){ #Si Silt
      text = c(0.075,0.875,0.050)
    } else if (ntext_soil == 15){ #CC Heavy clay
      text = c(0.100,0.100,0.800)
    } else if (ntext_soil == 16){ #CSa Clayey sand
      text = c(0.375,0.100,0.525)
    } else if (ntext_soil == 17){ #CSi Clayey silt
      text = c(0.125,0.350,0.525)
    } else{
      stop("Unknown ntext_soil")
    }

    sand_all[i] <- text[1]
    silt_all[i] <- text[2]
    clay_all[i] <- text[3]
  }

  return(list(sand_all,silt_all,clay_all)) # Sand, silt, clay
}












