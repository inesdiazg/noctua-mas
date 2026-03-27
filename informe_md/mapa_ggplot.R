library(sf)
library(ggplot2)
library(dplyr)
library(rnaturalearthdata)
library(ggspatial)
library(prettymapr)

setwd("C:/Users/inesi/OneDrive - UAM (1)/Producto_BirdNET/informe_md")
data <- read.csv("noctua.csv",sep=";",header=TRUE, stringsAsFactors = FALSE)
data <- head(data, n=1)

punto <- st_as_sf(data, coords = c("Longitud", "Latitud"), crs = 4326)
# Buffer de 0.01 grados (~1 km) alrededor del punto
zona <- st_buffer(punto, dist = 0.01)
lon <- data$Longitud
lat <- data$Latitud
delta <- 0.1

ggplot() +
  annotation_map_tile(type = "osm") +   # 👈 fondo OpenStreetMap
  geom_sf(data = punto, color = "red", size = 3) +
  coord_sf(
    xlim = c(lon - delta, lon + delta),
    ylim = c(lat - delta, lat + delta),
    expand = FALSE
  ) +
  theme_void()

ggplot() +
  annotation_map_tile(type = "osm", zoom = 12) +
  geom_sf(data = punto, color = "red", size = 3) +
  coord_sf(expand = FALSE) +
  theme_void()
#####
library(ggmap)
ggmap::register_google()

# Coordenadas
lon <- data$Longitud
lat <- data$Latitud
delta <- 0.02   # controla el zoom (más pequeño = más zoom)

