#' ---
#' title: "Maps Zimbabwe"
#' author: "Frédéric Baudron"
#' date: "October 24th, 2024"
#' ---


# CLEAR ENVIRONMENT-------------------------------------------------------------

rm(list = ls())


# LOADING NECESSARY PACKAGES----------------------------------------------------

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

setwd("D:\\Mes Donnees\\1. Cirad\\AE-I\\Evaluation of AE practices\\Zimbabwe\\")

# file.choose()

mother = read.csv("Data//Mother_demo coordinates.csv")

baby = read.csv("Data//baby_gps_coordinates.csv")


names(mother)[1] = "Farmer"
mother = mother %>% 
  mutate(ID = group_indices(., Farmer, District, Ward))
mother = mother[, c(10, 8:9, 3:4)]
names(mother) = c("ID", "District", "Ward", "lat", "lon")
mother$type = rep("Mother", nrow(mother))

names(baby)

baby = baby[ , c(1, 3:4, 8:9)]
baby = unique(baby)
baby = baby %>% 
  mutate(ID = group_indices(., Use.the.camera.to.scan.farmer.barcode, District, Ward))
baby = baby[, c(6, 4:5, 2:3)]
names(baby)[4:5] = c("lat", "lon")
baby$type = rep("Baby", nrow(baby))

data = rbind(mother, baby)
data = data %>% 
  mutate(ID = group_indices(., ID, type))


data$District = ifelse(data$District == "Murewa", "Murehwa", data$District)

data_mur = subset(data, District == "Murehwa")
data_mbi = subset(data, District == "Mbire")

data_mur_sf = st_as_sf(data_mur , coords = c("lon", "lat"), crs ="+proj=longlat + datum = WGS84 + no_defs")
data_mur_sf = as(data_mur_sf, "Spatial")
data_mur_sf = vect(data_mur_sf)
data_mur_sf = project(data_mur_sf, "EPSG:4326")

data_mbi_sf = st_as_sf(data_mbi , coords = c("lon", "lat"), crs ="+proj=longlat + datum = WGS84 + no_defs")
data_mbi_sf = as(data_mbi_sf, "Spatial")
data_mbi_sf = vect(data_mbi_sf)
data_mbi_sf = project(data_mbi_sf, "EPSG:4326")


# COUNTRY SHAPEFILES------------------------------------------------------------

zwe0 = gadm(country = 'ZWE', level = 0, path = "gadm")
zwe1 = gadm(country = 'ZWE', level = 1, path = "gadm")
zwe2 = gadm(country = 'ZWE', level = 2, path = "gadm")
zwe3 = gadm(country = 'ZWE', level = 3, path = "gadm")

zwe3_df = as.data.frame(zwe3)

mur = subset(zwe3, zwe3$NAME_2 == "Murehwa")
mbi = subset(zwe3, zwe3$NAME_2 == "Mbire")


plot(mur)
plot(data_mur_sf, add = T)


tex_5 = geodata::soil_af_isda(var = 'texture', depth = 20, path = "D:\\Mes Donnees\\1. Cirad\\AE-I\\Evaluation of AE practices\\Zimbabwe\\Soil")
elevation = geodata::elevation_30s("ZWE", path = "D:\\Mes Donnees\\1. Cirad\\AE-I\\Evaluation of AE practices\\Zimbabwe\\Soil")

tex_5_mur = crop(tex_5, mur)
tex_5_mur = mask(tex_5_mur, mur)
names(tex_5_mur) = "texture"

elevation_mur = crop(elevation, mur)
elevation_mur = mask(elevation_mur, mur)
names(elevation_mur) = "elevation"

plot(tex_5_mur)
plot(mur, add = T)
plot(data_mur_sf, add = T)

plot(elevation_mur)
plot(mur, add = T)
plot(data_mur_sf, add = T)

elevation_mur_df = as.data.frame(elevation_mur, xy = TRUE)
tex_mur_df = as.data.frame(tex_5_mur, xy = TRUE)

tex_5_mbi = crop(tex_5, mbi)
tex_5_mbi = mask(tex_5_mbi, mbi)
names(tex_5_mbi) = "texture"

elevation_mbi = crop(elevation, mbi)
elevation_mbi = mask(elevation_mbi, mbi)
names(elevation_mbi) = "elevation"

plot(tex_5_mbi)
plot(mbi, add = T)
plot(data_mbi_sf, add = T)

plot(elevation_mbi)
plot(mbi, add = T)
plot(data_mbi_sf, add = T)

elevation_mbi_df = as.data.frame(elevation_mbi, xy = TRUE)
tex_mbi_df = as.data.frame(tex_5_mbi, xy = TRUE)


# show_col(jcolors(palette = "pal6"))

ggplot() + ggtitle("Murehwa") + theme_bw() +
  geom_raster(data = na.omit(elevation_mur_df), aes(x = x, y = y, fill = elevation), alpha = 0.8) +
  geom_spatvector(data = mur, fill = NA, linewidth = 1, color = "black") +
  geom_spatvector(data = data_mur_sf, size = 3, aes(color = type), alpha = 0.8) +
  scale_color_manual(values = c("purple", "blue")) + 
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

# ggsave("Murehwa elevation.jpeg", units = "cm", width = 20, height = 20, dpi = 320)


ggplot() + ggtitle("Murehwa") + theme_bw() +
  geom_raster(data = tex_mur_df, aes(x = x, y = y, fill = texture), alpha = 0.8) +
  geom_spatvector(data = mur, fill = NA, linewidth = 1, color = "black") +
  geom_spatvector(data = data_mur_sf, size = 3, aes(color = type), alpha = 0.8) +
  scale_color_manual(values = c("purple", "blue")) + 
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

# ggsave("Murehwa texture.jpeg", units = "cm", width = 20, height = 20, dpi = 320)


ggplot() + ggtitle("Mbire") + theme_bw() +
  geom_raster(data = na.omit(elevation_mbi_df), aes(x = x, y = y, fill = elevation), alpha = 0.8) +
  geom_spatvector(data = mbi, fill = NA, linewidth = 1, color = "black") +
  geom_spatvector(data = data_mbi_sf, size = 3, aes(color = type), alpha = 0.8) +
  scale_color_manual(values = c("purple", "blue")) + 
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

# ggsave("Mbire elevation.jpeg", units = "cm", width = 22, height = 15, dpi = 320)


ggplot() + ggtitle("Mbire") + theme_bw() +
  geom_raster(data = tex_mbi_df, aes(x = x, y = y, fill = texture), alpha = 0.8) +
  geom_spatvector(data = mbi, fill = NA, linewidth = 1, color = "black") +
  geom_spatvector(data = data_mbi_sf, size = 3, aes(color = type), alpha = 0.8) +
  scale_color_manual(values = c("purple", "blue")) + 
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

# ggsave("Mbire texture.jpeg", units = "cm", width = 22, height = 15, dpi = 320)



