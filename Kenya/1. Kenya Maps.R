#' ---
#' title: "Maps Kenya"
#' author: "Frédéric Baudron"
#' date: "October 30th, 2024"
#' ---


# CLEAR ENVIRONMENT-------------------------------------------------------------

rm(list = ls())


# LOADING NECESSARY PACKAGES----------------------------------------------------

library(openxlsx)
library(dplyr)
library(geodata)
library(sf)
library(raster)
library(terra)
library(tidyterra)
library(ggplot2)
library(ggspatial)
library(ggthemes)
library(jcolors)
library(scales)


# SET WORKING DIRECTORY---------------------------------------------------------

setwd("D:\\Mes Donnees\\1. Cirad\\AE-I\\Evaluation of AE practices\\Kenya\\")

# file.choose()

kia = read.xlsx("CSHEP-soil sampling.xlsx")

mak = read.xlsx("Makueni soil sampling GPS coordinates.xlsx")


data_kia_sf = st_as_sf(kia , coords = c("Longitude", "Latitude"), crs ="+proj=longlat + datum = WGS84 + no_defs")
data_kia_sf = as(data_kia_sf, "Spatial")
data_kia_sf = vect(data_kia_sf)
data_kia_sf = project(data_kia_sf, "EPSG:4326")

data_mak_sf = st_as_sf(mak , coords = c("Longitude", "Latitude"), crs ="+proj=longlat + datum = WGS84 + no_defs")
data_mak_sf = as(data_mak_sf, "Spatial")
data_mak_sf = vect(data_mak_sf)
data_mak_sf = project(data_mak_sf, "EPSG:4326")

plot(data_kia_sf)
plot(data_mak_sf)

# COUNTRY SHAPEFILES------------------------------------------------------------

ken0 = gadm(country = 'KEN', level = 0, path = "gadm")
ken1 = gadm(country = 'KEN', level = 1, path = "gadm")
ken2 = gadm(country = 'KEN', level = 2, path = "gadm")
ken3 = gadm(country = 'KEN', level = 3, path = "gadm")


kiambu = subset(ken2, ken2$NAME_1 == "Kiambu")
makueni = subset(ken2, ken2$NAME_1 == "Makueni")

limuru = subset(ken3, ken3$NAME_2 == "Limuru")
mbooni = subset(ken3, ken3$NAME_2 == "Mbooni")


plot(kiambu)
plot(data_kia_sf, add = T)

plot(makueni)
plot(data_mak_sf, add = T)


plot(limuru)
plot(data_kia_sf, add = T)

plot(mbooni)
plot(data_mak_sf, add = T)


tex_5 = geodata::soil_af_isda(var = 'texture', depth = 20, path = "D:\\Mes Donnees\\1. Cirad\\AE-I\\Evaluation of AE practices\\Kenya\\Soil")
elevation = geodata::elevation_30s("KEN", path = "D:\\Mes Donnees\\1. Cirad\\AE-I\\Evaluation of AE practices\\Kenya\\Soil")

tex_5_kia = crop(tex_5, kiambu)
tex_5_kia = mask(tex_5_kia, kiambu)
names(tex_5_kia) = "texture"

elevation_kia = crop(elevation, kiambu)
elevation_kia = mask(elevation_kia, kiambu)
names(elevation_kia) = "elevation"


tex_5_lim = crop(tex_5, limuru)
tex_5_lim = mask(tex_5_lim, limuru)
names(tex_5_lim) = "texture"

elevation_lim = crop(elevation, limuru)
elevation_lim = mask(elevation_lim, limuru)
names(elevation_lim) = "elevation"


plot(tex_5_kia)
plot(kiambu, add = T)
plot(data_kia_sf, add = T)

plot(elevation_kia)
plot(kiambu, add = T)
plot(data_kia_sf, add = T)


plot(tex_5_lim)
plot(limuru, add = T)
plot(data_kia_sf, add = T)

plot(elevation_lim)
plot(limuru, add = T)
plot(data_kia_sf, add = T)


elevation_kia_df = as.data.frame(elevation_kia, xy = TRUE)
tex_kia_df = as.data.frame(tex_5_kia, xy = TRUE)


elevation_lim_df = as.data.frame(elevation_lim, xy = TRUE)
tex_lim_df = as.data.frame(tex_5_lim, xy = TRUE)


tex_5_mak = crop(tex_5, makueni)
tex_5_mak = mask(tex_5_mak, makueni)
names(tex_5_mak) = "texture"

elevation_mak = crop(elevation, makueni)
elevation_mak = mask(elevation_mak, makueni)
names(elevation_mak) = "elevation"


tex_5_mbo = crop(tex_5, mbooni)
tex_5_mbo = mask(tex_5_mbo, mbooni)
names(tex_5_mbo) = "texture"

elevation_mbo = crop(elevation, mbooni)
elevation_mbo = mask(elevation_mbo, mbooni)
names(elevation_mbo) = "elevation"


plot(tex_5_mak)
plot(makueni, add = T)
plot(data_mak_sf, add = T)

plot(elevation_mak)
plot(makueni, add = T)
plot(data_mak_sf, add = T)


plot(tex_5_mbo)
plot(mbooni, add = T)
plot(data_mak_sf, add = T)

plot(elevation_mbo)
plot(mbooni, add = T)
plot(data_mak_sf, add = T)


elevation_mak_df = as.data.frame(elevation_mak, xy = TRUE)
tex_mak_df = as.data.frame(tex_5_mak, xy = TRUE)


elevation_mbo_df = as.data.frame(elevation_mbo, xy = TRUE)
tex_mbo_df = as.data.frame(tex_5_mbo, xy = TRUE)


# show_col(jcolors(palette = "pal6"))

ggplot() + ggtitle("Kiambu") + theme_bw() +
  geom_raster(data = na.omit(elevation_kia_df), aes(x = x, y = y, fill = elevation), alpha = 0.8) +
  geom_spatvector(data = kiambu, fill = NA, linewidth = 1, color = "black") +
  geom_spatvector(data = data_kia_sf, size = 3, aes(color = type), alpha = 0.8, color = "purple") +
  scale_fill_distiller(palette = "RdYlGn") +
  labs(fill = "Elevation (m.a.s.l.)", color = "Trial type") +
  theme(plot.title = element_text(hjust = 0, size = 20, face = "bold", margin = margin(0, 0, 10, 0)),
        axis.title = element_blank(),
        axis.text.y = element_text(angle = 90, hjust = 0.5, size = 12),
        axis.text.x = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.position = "right",
        plot.margin = margin(10, 10, 10, 10)) +
  annotation_scale(location = "bl", width_hint = 0.5, height = unit(0.25, "cm"), 
                   text_cex = 1, pad_x = unit(0.15, "cm"), pad_y = unit(0.15, "cm")) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         height = unit(1, "cm"), width = unit(1, "cm"),
                         pad_x = unit(0.15, "cm"), pad_y = unit(0.15, "cm"),
                         style = north_arrow_fancy_orienteering(text_size = 10))

# ggsave("Kiambu elevation.jpeg", units = "cm", width = 30, height = 17, dpi = 320)


ggplot() + ggtitle("Kiambu") + theme_bw() +
  geom_raster(data = tex_kia_df, aes(x = x, y = y, fill = texture), alpha = 0.8) +
  geom_spatvector(data = kiambu, fill = NA, linewidth = 1, color = "black") +
  geom_spatvector(data = data_kia_sf, size = 3, aes(color = type), alpha = 0.8, color = "purple") +
  scale_fill_brewer(palette = "YlOrBr", direction = -1) +
  labs(fill = "Texture (class)", color = "Trial type") +
  theme(plot.title = element_text(hjust = 0, size = 20, face = "bold", margin = margin(0, 0, 10, 0)),
        axis.title = element_blank(),
        axis.text.y = element_text(angle = 90, hjust = 0.5, size = 12),
        axis.text.x = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.position = "right",
        plot.margin = margin(10, 10, 10, 10)) +
  annotation_scale(location = "bl", width_hint = 0.5, height = unit(0.25, "cm"), 
                   text_cex = 1, pad_x = unit(0.15, "cm"), pad_y = unit(0.15, "cm")) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         height = unit(1, "cm"), width = unit(1, "cm"),
                         pad_x = unit(0.15, "cm"), pad_y = unit(0.15, "cm"),
                         style = north_arrow_fancy_orienteering(text_size = 10))

# ggsave("Kiambu texture.jpeg", units = "cm", width = 30, height = 17, dpi = 320)


ggplot() + ggtitle("Makueni") + theme_bw() +
  geom_raster(data = na.omit(elevation_mak_df), aes(x = x, y = y, fill = elevation), alpha = 0.8) +
  geom_spatvector(data = makueni, fill = NA, linewidth = 1, color = "black") +
  geom_spatvector(data = data_mak_sf, size = 3, aes(color = type), alpha = 0.8, color = "purple") +
  scale_fill_distiller(palette = "RdYlGn") +
  labs(fill = "Elevation (m.a.s.l.)", color = "Trial type") +
  theme(plot.title = element_text(hjust = 0, size = 20, face = "bold", margin = margin(0, 0, 10, 0)),
        axis.title = element_blank(),
        axis.text.y = element_text(angle = 90, hjust = 0.5, size = 12),
        axis.text.x = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.position = "right",
        plot.margin = margin(10, 10, 10, 10)) +
  annotation_scale(location = "bl", width_hint = 0.5, height = unit(0.25, "cm"), 
                   text_cex = 1, pad_x = unit(0.15, "cm"), pad_y = unit(0.15, "cm")) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         height = unit(1, "cm"), width = unit(1, "cm"),
                         pad_x = unit(0.15, "cm"), pad_y = unit(0.15, "cm"),
                         style = north_arrow_fancy_orienteering(text_size = 10))

# ggsave("Makueni elevation.jpeg", units = "cm", width = 22, height = 19, dpi = 320)


ggplot() + ggtitle("Makueni") + theme_bw() +
  geom_raster(data = tex_mak_df, aes(x = x, y = y, fill = texture), alpha = 0.8) +
  geom_spatvector(data = makueni, fill = NA, linewidth = 1, color = "black") +
  geom_spatvector(data = data_mak_sf, size = 3, aes(color = type), alpha = 0.8, color = "purple") +
  scale_fill_brewer(palette = "YlOrBr", direction = -1) +
  labs(fill = "Texture (class)", color = "Trial type") +
  theme(plot.title = element_text(hjust = 0, size = 20, face = "bold", margin = margin(0, 0, 10, 0)),
        axis.title = element_blank(),
        axis.text.y = element_text(angle = 90, hjust = 0.5, size = 12),
        axis.text.x = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.position = "right",
        plot.margin = margin(10, 10, 10, 10)) +
  annotation_scale(location = "bl", width_hint = 0.5, height = unit(0.25, "cm"), 
                   text_cex = 1, pad_x = unit(0.15, "cm"), pad_y = unit(0.15, "cm")) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         height = unit(1, "cm"), width = unit(1, "cm"),
                         pad_x = unit(0.15, "cm"), pad_y = unit(0.15, "cm"),
                         style = north_arrow_fancy_orienteering(text_size = 10))

# ggsave("Makueni texture.jpeg", units = "cm", width = 22, height = 19, dpi = 320)


ggplot() + ggtitle("Limuru") + theme_bw() +
  geom_raster(data = na.omit(elevation_lim_df), aes(x = x, y = y, fill = elevation), alpha = 0.8) +
  geom_spatvector(data = limuru, fill = NA, linewidth = 1, color = "black") +
  geom_spatvector(data = data_kia_sf, size = 3, aes(color = type), alpha = 0.8, color = "purple") +
  scale_fill_distiller(palette = "RdYlGn") +
  labs(fill = "Elevation (m.a.s.l.)", color = "Trial type") +
  theme(plot.title = element_text(hjust = 0, size = 20, face = "bold", margin = margin(0, 0, 10, 0)),
        axis.title = element_blank(),
        axis.text.y = element_text(angle = 90, hjust = 0.5, size = 12),
        axis.text.x = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.position = "right",
        plot.margin = margin(10, 10, 10, 10)) +
  annotation_scale(location = "bl", width_hint = 0.5, height = unit(0.25, "cm"), 
                   text_cex = 1, pad_x = unit(0.15, "cm"), pad_y = unit(0.15, "cm")) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         height = unit(1, "cm"), width = unit(1, "cm"),
                         pad_x = unit(0.15, "cm"), pad_y = unit(0.15, "cm"),
                         style = north_arrow_fancy_orienteering(text_size = 10))

# ggsave("Limuru elevation.jpeg", units = "cm", width = 27, height = 20, dpi = 320)


ggplot() + ggtitle("Limuru") + theme_bw() +
  geom_raster(data = tex_lim_df, aes(x = x, y = y, fill = texture), alpha = 0.8) +
  geom_spatvector(data = limuru, fill = NA, linewidth = 1, color = "black") +
  geom_spatvector(data = data_kia_sf, size = 3, aes(color = type), alpha = 0.8, color = "purple") +
  scale_fill_brewer(palette = "YlOrBr", direction = -1) +
  labs(fill = "Texture (class)", color = "Trial type") +
  theme(plot.title = element_text(hjust = 0, size = 20, face = "bold", margin = margin(0, 0, 10, 0)),
        axis.title = element_blank(),
        axis.text.y = element_text(angle = 90, hjust = 0.5, size = 12),
        axis.text.x = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.position = "right",
        plot.margin = margin(10, 10, 10, 10)) +
  annotation_scale(location = "bl", width_hint = 0.5, height = unit(0.25, "cm"), 
                   text_cex = 1, pad_x = unit(0.15, "cm"), pad_y = unit(0.15, "cm")) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         height = unit(1, "cm"), width = unit(1, "cm"),
                         pad_x = unit(0.15, "cm"), pad_y = unit(0.15, "cm"),
                         style = north_arrow_fancy_orienteering(text_size = 10))

# ggsave("Limuru texture.jpeg", units = "cm", width = 27, height = 20, dpi = 320)


ggplot() + ggtitle("Mbooni") + theme_bw() +
  geom_raster(data = na.omit(elevation_mbo_df), aes(x = x, y = y, fill = elevation), alpha = 0.8) +
  geom_spatvector(data = mbooni, fill = NA, linewidth = 1, color = "black") +
  geom_spatvector(data = data_mak_sf, size = 3, aes(color = type), alpha = 0.8, color = "purple") +
  scale_fill_distiller(palette = "RdYlGn") +
  labs(fill = "Elevation (m.a.s.l.)", color = "Trial type") +
  theme(plot.title = element_text(hjust = 0, size = 20, face = "bold", margin = margin(0, 0, 10, 0)),
        axis.title = element_blank(),
        axis.text.y = element_text(angle = 90, hjust = 0.5, size = 12),
        axis.text.x = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.position = "right",
        plot.margin = margin(10, 10, 10, 10)) +
  annotation_scale(location = "bl", width_hint = 0.5, height = unit(0.25, "cm"), 
                   text_cex = 1, pad_x = unit(0.15, "cm"), pad_y = unit(0.15, "cm")) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         height = unit(1, "cm"), width = unit(1, "cm"),
                         pad_x = unit(0.15, "cm"), pad_y = unit(0.15, "cm"),
                         style = north_arrow_fancy_orienteering(text_size = 10))

# ggsave("Mbooni elevation.jpeg", units = "cm", width = 40, height = 20, dpi = 320)


ggplot() + ggtitle("Mbooni") + theme_bw() +
  geom_raster(data = tex_mbo_df, aes(x = x, y = y, fill = texture), alpha = 0.8) +
  geom_spatvector(data = mbooni, fill = NA, linewidth = 1, color = "black") +
  geom_spatvector(data = data_mak_sf, size = 3, aes(color = type), alpha = 0.8, color = "purple") +
  scale_fill_brewer(palette = "YlOrBr", direction = -1) +
  labs(fill = "Texture (class)", color = "Trial type") +
  theme(plot.title = element_text(hjust = 0, size = 20, face = "bold", margin = margin(0, 0, 10, 0)),
        axis.title = element_blank(),
        axis.text.y = element_text(angle = 90, hjust = 0.5, size = 12),
        axis.text.x = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.position = "right",
        plot.margin = margin(10, 10, 10, 10)) +
  annotation_scale(location = "bl", width_hint = 0.5, height = unit(0.25, "cm"), 
                   text_cex = 1, pad_x = unit(0.15, "cm"), pad_y = unit(0.15, "cm")) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         height = unit(1, "cm"), width = unit(1, "cm"),
                         pad_x = unit(0.15, "cm"), pad_y = unit(0.15, "cm"),
                         style = north_arrow_fancy_orienteering(text_size = 10))

# ggsave("Mbooni texture.jpeg", units = "cm", width = 40, height = 20, dpi = 320)








