rm(list = ls())

library(SoilSensitivity)
library(lattice)
library(raster)
library(purrr)
library(tidyr)
library(ggplot2)
library(dplyr)

rc <- brick(file.path(".","maps", "soilgrid.grd"),expand = TRUE)

plot(rc[[1]])

top.sand <- raster::aggregate(rc[[1]],
                              1/res(rc)[1]*0.25,
                              1/res(rc)[2]*0.25,
                              fun = mean)

top.clay <- raster::aggregate(rc[[2]],
                              1/res(rc)[1]*0.25,
                              1/res(rc)[2]*0.25,
                              fun = mean)

sub.sand <- raster::aggregate(rc[[3]],
                              1/res(rc)[1]*0.25,
                              1/res(rc)[2]*0.25,
                              fun = mean)

sub.clay <- raster::aggregate(rc[[4]],
                              1/res(rc)[1]*0.25,
                              1/res(rc)[2]*0.25,
                              fun = mean)


writeRaster(top.sand, filename=file.path(".","maps", "soilgrid_top.sand.grd"),overwrite = TRUE)
writeRaster(top.clay, filename=file.path(".","maps", "soilgrid_top.clay.grd"),overwrite = TRUE)
writeRaster(sub.sand, filename=file.path(".","maps", "soilgrid_sub.sand.grd"),overwrite = TRUE)
writeRaster(sub.clay, filename=file.path(".","maps", "soilgrid_sub.clay.grd"),overwrite = TRUE)

ocean.filt <- as.vector(top.sand)==0

df.hydro <- data.frame(top.sand = as.numeric(as.vector(top.sand)),
                       top.clay = as.numeric(as.vector(top.clay)),
                       sub.sand = as.numeric(as.vector(sub.sand)),
                       sub.clay = as.numeric(as.vector(sub.clay))) %>% filter(sub.sand > 0 & sub.clay > 0) %>% mutate(top.ksat = get_soilproperties(top.sand,top.clay)[["k_sat"]],
                                                                                                                      sub.ksat = get_soilproperties(sub.sand,sub.clay)[["k_sat"]],
                                                                                                                      top.b = get_soilproperties(top.sand,top.clay)[["b"]],
                                                                                                                      sub.b = get_soilproperties(sub.sand,sub.clay)[["b"]],
                                                                                                                      top.tsat = get_soilproperties(top.sand,top.clay)[["theta_sat"]],
                                                                                                                      sub.tsat = get_soilproperties(sub.sand,sub.clay)[["theta_sat"]],
                                                                                                                      top.tfc = get_soilproperties(top.sand,top.clay)[["theta_fc"]],
                                                                                                                      sub.tfc = get_soilproperties(sub.sand,sub.clay)[["theta_fc"]],
                                                                                                                      top.twp = get_soilproperties(top.sand,top.clay)[["theta_wp"]],
                                                                                                                      sub.twp = get_soilproperties(sub.sand,sub.clay)[["theta_wp"]])  %>%
  mutate(sand =  (top.sand - sub.sand),
         clay =  (top.clay - sub.clay),
         ksat =  (top.ksat - sub.ksat),
         b =  (top.b - sub.b),
         theta_sat =  (top.tsat - sub.tsat),
         theta_fc =  (top.tfc - sub.tfc),
         theta_wp =  (top.twp - sub.twp)) %>% pivot_longer(cols = c("sand","clay","ksat","b","theta_sat","theta_fc","theta_wp"),
                                                         names_to = "variable", values_to = "diff")

ggplot(data = df.hydro) +
  geom_histogram(aes(x = diff),alpha = 0.5) +
  geom_vline(xintercept = 0) +
  facet_wrap(~variable,scales = "free") +
  theme_bw()
