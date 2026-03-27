library(dplyr)
library(lubridate)
library(glue)
library(kableExtra)
library(leaflet)

setwd("C:/Users/inesi/OneDrive - UAM (1)/Producto_BirdNET/informe_md")
datos <- read.csv("noctua.csv",sep=";",header=TRUE)
datos1 <- head(datos, n=1)

info_punto <- data.frame(
  punto = character(),
  voluntario = character(),
  temporada = numeric(),
  localizacion = character(),
  habitat = character(),
  stringsAsFactors = FALSE)

info_punto <- info_punto %>%
  add_row(
    punto = datos1$PuntoMuestreo,
    voluntario = datos1$Responsable,
    temporada = year(datos1$FechaInstalacion),
    localizacion = glue("{datos1$Provincia} ({datos1$Latitud}, {datos1$Longitud})"),
    habitat = datos1$Habitat
  )

# trasponer y cambiar nombres
info_puntot <- as.data.frame(t(info_punto))
row.names(info_puntot) <- c("Punto de muestreo","Voluntario/a","Temporada","Localización", "Hábitat")

info_puntot <- info_puntot %>%
  kbl(align = "l") %>%
  kable_styling(full_width = F, position = "left") %>%
  row_spec(0:nrow(info_puntot), extra_css = "border: 1px solid;") %>%
  column_spec(1, background = "#9BB1C9") %>%
  row_spec(0, extra_css = "display: none;")
info_puntot

# mapa
mapa <- leaflet(datos1) |>
  addTiles() |>
  setView(lng = datos1$Longitud, lat = datos1$Latitud, zoom = 11) |>
  addMarkers(
    data = datos1,
    lng = ~Longitud,
    lat = ~Latitud)
mapa
