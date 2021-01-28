rm(list = ls())

library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggplot2)
library(dplyr)
library(viridis)
library(tidyr)
library(SoilSensitivity)

system2("rsync",paste("-avz",
                      "hpc:/data/gent/vo/000/gvo00074/felicien/R/df_SoilSens.RDS",
                      "/home/femeunier/Documents/projects/SoilSensitivity/outputs/"))

df <- readRDS(file = "/home/femeunier/Documents/projects/SoilSensitivity/outputs/df_SoilSens.RDS") %>%
  group_by(lat,lon,scenario) %>% mutate(group.id = group_indices())

ED_REG_LATMIN = -10.5
ED_REG_LATMAX = -0.5
ED_REG_LONMIN = -60.5
ED_REG_LONMAX = -50.5

world <- ne_countries(scale = "medium", returnclass = "sf")

df <- df %>% group_by(group.id) %>% mutate(sand_top = ntext_soil2components(ntext_soil_bt)[[1]],
                                           sand_bottom = ntext_soil2components(ntext_soil_tp)[[1]])

ggplot(data = df  %>% filter(scenario == 'soilgrids',
                             year == 2000)) +
  geom_raster(aes(x=lon, y = lat, fill = as.factor(ntext_soil_tp)),alpha = 0.3) +
  geom_sf(data = world,fill = NA) +
  coord_sf(xlim = c(-81.5, -34.5), ylim = c(-17.5, 12.5), expand = FALSE) +
  labs(x = "",y = "") +
  theme_bw()

ggplot(data = df  %>% filter(scenario == 'soilgrids_ref',
                             year == 2000)) +
  geom_raster(aes(x=lon, y = lat, fill = sand_top),alpha = 0.3) +
  geom_sf(data = world,fill = NA) +
  coord_sf(xlim = c(-81.5, -34.5), ylim = c(-17.5, 12.5), expand = FALSE) +
  labs(x = "",y = "") +
  theme_bw()


ggplot(data = df  %>% filter(year == 2000,
                             scenario == "ref"),
       aes(x = sand_bottom, y = sand_top)) +
  geom_abline(slope = 1,intercept = 0,linetype = 2) +
  geom_bin2d(bins = 30) +
  scale_fill_continuous(type = "viridis") +
  labs(x = "",y = "") +
  theme_bw()
