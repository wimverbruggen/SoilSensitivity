rm(list = ls())

library(SoilSensitivity)
library(lattice)
library(raster)
library(purrr)
library(tidyr)
library(ggplot2)
library(dplyr)

rc <- brick(file.path(".","maps", "soilgrid.grd"),expand = TRUE)

top.sand.mean <- raster::aggregate(rc[[c(1)]],
                                   1 / res(rc)[1] * 0.5,
                                   1 / res(rc)[2] * 0.5,
                                   fun = mean)

top.clay.mean <- raster::aggregate(rc[[c(2)]],
                                   1 / res(rc)[1] * 0.5,
                                   1 / res(rc)[2] * 0.5,
                                   fun = mean)

top.sand.min.clay <- aggregates(
  raster1 = rc[[c(2)]],
  raster2 = rc[[c(1)]],
  res1 = 1 / res(rc)[1] * 0.5,
  res2 = 1 / res(rc)[1] * 0.5,
  FUN = "min"
)

top.sand.max.clay <- aggregates(
  raster1 = rc[[c(2)]],
  raster2 = rc[[c(1)]],
  res1 = 1 / res(rc)[1] * 0.5,
  res2 = 1 / res(rc)[1] * 0.5,
  FUN = "max"
)

top.clay.max.clay <- raster::aggregate(rc[[c(2)]],
                                   1 / res(rc)[1] * 0.5,
                                   1 / res(rc)[2] * 0.5,
                                   fun = max)

top.clay.min.clay <- raster::aggregate(rc[[c(2)]],
                                   1 / res(rc)[1] * 0.5,
                                   1 / res(rc)[2] * 0.5,
                                   fun = min)

writeRaster(top.sand.mean, filename=file.path(".","maps", "soilgrid_top.sand_mean.grd"),overwrite = TRUE)
writeRaster(top.clay.mean, filename=file.path(".","maps", "soilgrid_top.clay_mean.grd"),overwrite = TRUE)
writeRaster(top.sand.max.clay, filename=file.path(".","maps", "soilgrid_top.sand_max.grd"),overwrite = TRUE)
writeRaster(top.clay.max.clay, filename=file.path(".","maps", "soilgrid_top.clay_max.grd"),overwrite = TRUE)
writeRaster(top.sand.min.clay, filename=file.path(".","maps", "soilgrid_top.sand_min.grd"),overwrite = TRUE)
writeRaster(top.clay.min.clay, filename=file.path(".","maps", "soilgrid_top.clay_min.grd"),overwrite = TRUE)


par(mfrow = c(2, 1))
plot(top.clay.min.clay)
plot(top.sand.min.clay)
top.clay.min.clay + top.sand.min.clay

par(mfrow = c(2, 1))
plot(top.clay.max.clay)
plot(top.sand.max.clay)
top.clay.max.clay + top.sand.max.clay

par(mfrow = c(2, 1))
plot(top.clay.mean)
plot(top.sand.mean)
top.clay.mean + top.sand.mean

df.hydro <-
  data.frame(
    sand.mean = as.numeric(as.vector(top.sand.mean)),
    clay.mean = as.numeric(as.vector(top.clay.mean)),
    sand.max = as.numeric(as.vector(top.sand.max.clay)),
    clay.max = as.numeric(as.vector(top.clay.max.clay)),
    sand.min = as.numeric(as.vector(top.sand.min.clay)),
    clay.min = as.numeric(as.vector(top.clay.min.clay))
  ) %>% filter(sand.mean > 0 & clay.mean > 0) %>%
  pivot_longer(cols = -c()) %>% mutate(texture = sub("\\..*", "", name),
                                       scenar = sub(".*\\.", "", name)) %>% dplyr::select(-c("name")) %>% mutate(ID = sort(rep(1:(length(
                                         scenar
                                       ) / 2), 2))) %>%
  pivot_wider(names_from = c("texture"),
              values_from = value) %>% mutate(pos = sort(rep(1:(length(
                scenar
              ) / 3), 3))) %>% dplyr::select(-c(ID)) %>%
  mutate(
    ksat = get_soilproperties(sand, clay)[["k_sat"]],
    b = get_soilproperties(sand, clay)[["b"]],
    tsat = get_soilproperties(sand, clay)[["theta_sat"]],
    tfc = get_soilproperties(sand, clay)[["theta_fc"]],
    twp = get_soilproperties(sand, clay)[["theta_wp"]]
  )

df.hydro.mean <- df.hydro %>% filter(scenar == "mean") %>% pivot_longer(cols = -c("scenar","pos"))

ggplot(data = df.hydro.mean) +
  geom_histogram(aes(x = value),alpha = 0.5, fill = "black") +
  facet_wrap(~name,scales = "free") +
  theme_bw()
ggsave(plot = last_plot(),
       filename = "./Figures/Figure_hydro_mean.png",
       dpi = 300,width = 12,height = 8)

df.hydro.diff <- df.hydro %>% pivot_wider(names_from = "scenar",
                                          values_from = -c(pos, scenar)) %>%
  mutate(
    sand.max =  (sand_max - sand_mean),
    clay.max =  (clay_max - clay_mean),
    ksat.max =  (ksat_max - ksat_mean),
    b.max =  (b_max - b_mean),
    tsat.max =  (tsat_max - tsat_mean),
    tfc.max =  (tfc_max - tfc_mean),
    twp.max =  (twp_max - twp_mean),
    sand.min =  (sand_min - sand_mean),
    clay.min =  (clay_min - clay_mean),
    ksat.min =  (ksat_min - ksat_mean),
    b.min =  (b_min - b_mean),
    tsat.min =  (tsat_min - tsat_mean),
    tfc.min =  (tfc_min - tfc_mean),
    twp.min =  (twp_min - twp_mean)
  ) %>% dplyr::select(c(ends_with(".min"), ends_with(".max"))) %>%
  pivot_longer(cols = -c(),
               names_to = "variable",
               values_to = "diff") %>% mutate(var = sub("\\..*", "", variable),
                                              scenar = sub(".*\\.", "", variable))

ggplot(data = df.hydro.diff) +
  geom_histogram(aes(x = diff,fill = scenar),alpha = 0.5) +
  geom_vline(xintercept = 0) +
  facet_wrap(~var,scales = "free") +
  theme_bw()

ggsave(plot = last_plot(),
       filename = "./Figures/Figure_hydro_diff.png",
       dpi = 300,width = 12,height = 8)

df.hydro.diff.rel <- df.hydro %>% pivot_wider(names_from = "scenar",
                                              values_from = -c(pos, scenar)) %>%
  mutate(
    sand.max =  100 * (sand_max - sand_mean) / sand_mean,
    clay.max =  100 * (clay_max - clay_mean) / clay_mean,
    ksat.max =  100 * (ksat_max - ksat_mean) / ksat_mean,
    b.max =  100 * (b_max - b_mean) / b_mean,
    tsat.max =  100 * (tsat_max - tsat_mean) / tsat_mean,
    tfc.max =  100 * (tfc_max - tfc_mean) / tfc_mean,
    twp.max =  100 * (twp_max - twp_mean) / twp_mean,
    sand.min =  100 * (sand_min - sand_mean) / sand_mean,
    clay.min =  100 * (clay_min - clay_mean) / clay_mean,
    ksat.min =  100 * (ksat_min - ksat_mean) / ksat_mean,
    b.min =  100 * (b_min - b_mean) / b_mean,
    tsat.min =  100 * (tsat_min - tsat_mean) / tsat_mean,
    tfc.min =  100 * (tfc_min - tfc_mean) / tfc_mean,
    twp.min =  100 * (twp_min - twp_mean) / twp_mean
  ) %>% dplyr::select(c(ends_with(".min"), ends_with(".max"))) %>%
  pivot_longer(cols = -c(),
               names_to = "variable",
               values_to = "diff.rel") %>% mutate(var = sub("\\..*", "", variable),
                                                  scenar = sub(".*\\.", "", variable))



ggplot(data = df.hydro.diff.rel %>% filter(!(var %in% c("clay","sand")))) +
  geom_histogram(aes(x = diff.rel,fill = scenar),alpha = 0.5) +
  geom_vline(xintercept = 0) +
  facet_wrap(~var,scales = "free") +
  theme_bw()


