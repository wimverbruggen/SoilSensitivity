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
  group_by(lat,lon,year,scenario) %>% mutate(group.id = group_indices())

ED_REG_LATMIN = -19.5
ED_REG_LATMAX =  13.5
ED_REG_LONMIN = -84.5
ED_REG_LONMAX = -30.5

GRID_RES = 1

world <- ne_countries(scale = "medium", returnclass = "sf")

ggplot(data = df  %>% filter(lat<=ED_REG_LATMAX,lat>=ED_REG_LATMIN,lon<=ED_REG_LONMAX,lon>=ED_REG_LONMIN,
                             scenario == "SoilGrids_mean")) +
  geom_raster(aes(x=lon, y = lat, fill = LAI),alpha = 0.3) +
  geom_sf(data = world,fill = NA) +
  coord_sf(xlim = c(-84.5, -30.5), ylim = c(-19.5, 15.5), expand = FALSE) +
  scale_fill_gradient2(low = "darkred",mid = "white",high = "darkgreen") +
  labs(x = "",y = "") +
  theme_bw()

df_wide <- df %>% mutate(scenario = sub(".*\\_", "", scenario)) %>% filter(year == max(df$year)) %>% dplyr::select(-group.id) %>%
  pivot_wider(names_from = scenario,
              values_from = -c(lat,lon,scenario,year))


hist(df_wide %>% mutate(AGB.change = AGB_mean - AGB_min ,
                        AGB.change.rel = AGB.change/AGB_mean,
                        LAI.change = LAI_mean - LAI_min ,
                        LAI.change.rel = LAI.change/LAI_mean) %>% pull(LAI.change.rel)*100)
