rm(list = ls())

library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggplot2)
library(dplyr)
library(viridis)
library(tidyr)
library(SoilSensitivity)

# system2("rsync",paste("-avz",
#                       "hpc:/data/gent/vo/000/gvo00074/felicien/R/df_SoilSens.RDS",
#                       "/home/femeunier/Documents/projects/SoilSensitivity/outputs/"))

df <- readRDS(file = "/home/femeunier/Documents/projects/SoilSensitivity/outputs/df_SoilSens.RDS") %>%
  group_by(lat,lon,scenario) %>% mutate(group.id = group_indices())

ED_REG_LATMIN = -10.5
ED_REG_LATMAX = -0.5
ED_REG_LONMIN = -60.5
ED_REG_LONMAX = -50.5

year.select = 2005

world <- ne_countries(scale = "medium", returnclass = "sf")

df <- df %>% group_by(group.id) %>% mutate(sand = ntext_soil2components(ntext_soil_tp)[[1]])

df_wide <- df %>% pivot_wider(names_from = scenario,
                              values_from = -c(lat,lon,scenario,year))

ggplot(data = df_wide  %>% filter(lat<=ED_REG_LATMAX,lat>=ED_REG_LATMIN,lon<=ED_REG_LONMAX,lon>=ED_REG_LONMIN,
                                  year == year.select)) +
  geom_raster(aes(x=lon, y = lat, fill = 100*(AGB_soilgrids - AGB_HWSD)/AGB_HWSD),alpha = 0.3) +
  geom_sf(data = world,fill = NA) +
  coord_sf(xlim = c(-81.5, -34.5), ylim = c(-17.5, 12.5), expand = FALSE) +
  scale_fill_gradient2(low = "darkred",mid = "white",high = "darkgreen") +
  labs(x = "",y = "") +
  theme_bw()

df.sum <- df %>% group_by(group.id) %>% mutate(AGB_rel = AGB/AGB[1])

ggplot(data = df.sum %>% filter(group.id %in% seq(1,5*length(unique(df.sum$scenario))))) +
  geom_line(aes(x = year, y = AGB, color = scenario,group = interaction(group.id,scenario))) +
  theme_bw()

ggplot(data = df.sum %>% filter(year == year.select,
                                AGB > 12)) +
  geom_boxplot(aes(x = scenario,y = AGB_rel)) +
  geom_abline(slope = 0, intercept = 1,color = "red",linetype = 3) +
  theme_bw()

