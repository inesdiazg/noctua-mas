library(leaflet)
library(glue)
library(htmltools)
library(sf)
library(dplyr)

# v1 ----------------------------------------------------------------------

# 1️⃣ Cargar datos de puntos
ptos <- read.csv("datos/noctua.csv", sep = ";")
ptos <- ptos %>%
  mutate(PuntoMuestreo = gsub("_", " ", PuntoMuestreo))
ptos_sf <- st_as_sf(ptos, coords = c("Longitud", "Latitud"), crs = 4326)

# 2️⃣ Cargar cuadrículas
cuadrillas <- st_read("datos/Malla10x10_Ter_p.shp") %>%
  st_transform(crs = 4326)

# 3️⃣ Unión espacial: asignar cada punto a la cuadrícula donde cae
ptos_con_cuadrilla <- st_join(ptos_sf, cuadrillas, join = st_within)

# 4️⃣ Crear HTML de popup transpuesto
popup_html <- ptos_con_cuadrilla %>%
  st_set_geometry(NULL) %>%
  group_by(UTMCODE) %>%
  summarise(
    popup = {
      # Datos de cada punto
      puntos <- PuntoMuestreo
      provincias <- Provincia
      inicio <- FechaInicioGrabacionMuestreo
      fin <- FechaFinGrabacionMuestreo
      noches <- Dias_Grabados_Campo
      habitat <- Habitat
      
      # Columnas con ancho mínimo
      columnas <- paste0(
        "<th style='padding:4px 12px; min-width:100px;'>", puntos, "</th>", 
        collapse = ""
      )
      
      fila_provincia <- paste0("<td style='padding:4px 12px; min-width:100px;'>", provincias, "</td>", collapse = "")
      fila_inicio    <- paste0("<td style='padding:4px 12px; min-width:100px;'>", inicio, "</td>", collapse = "")
      fila_fin       <- paste0("<td style='padding:4px 12px; min-width:100px;'>", fin, "</td>", collapse = "")
      fila_noches    <- paste0("<td style='padding:4px 12px; min-width:100px;'>", noches, "</td>", collapse = "")
      fila_habitat   <- paste0("<td style='padding:4px 12px; min-width:100px;'>", habitat, "</td>", collapse = "")
      
      # Tabla envuelta en un div con scroll horizontal
      paste0(
        "<div style='overflow-x:auto; max-width:400px;'>",  # <- aquí ajusta el max-width del popup
        "<table style='border-collapse: collapse; width: 100%; border: 1px solid #ccc;'>",
        "<tr style='background-color:#f0f0f0;'><th style='padding:4px 12px;'></th>", columnas, "</tr>",
        "<tr><th style='padding:4px 12px;'>Provincia</th>", fila_provincia, "</tr>",
        "<tr><th style='padding:4px 12px;'>Inicio</th>", fila_inicio, "</tr>",
        "<tr><th style='padding:4px 12px;'>Fin</th>", fila_fin, "</tr>",
        "<tr><th style='padding:4px 12px;'>Noches</th>", fila_noches, "</tr>",
        "<tr><th style='padding:4px 12px;'>Hábitat</th>", fila_habitat, "</tr>",
        "</table>",
        "</div>"
      )
    },
    .groups = "drop"
  )

# 5️⃣ Unir info de popup a polígonos
cuad_con_info <- cuadrillas %>%
  inner_join(popup_html, by = "UTMCODE")

# 6️⃣ Crear mapa Leaflet
leaflet(cuad_con_info) %>%
  addTiles() %>%
  setView(lng = -3.7, lat = 40.4, zoom = 5) %>%
  addPolygons(
    fillOpacity = 0.1,
    color = "blue",
    weight = 1,
    popup = ~popup
  )


# v2 ----------------------------------------------------------------------

library(leaflet)
library(glue)
library(htmltools)
library(sf)
library(dplyr)

ptos <- read.csv("datos/noctua.csv", sep = ";")
ptos <- ptos %>%
  mutate(
    PuntoMuestreo = gsub("_", " ", PuntoMuestreo),
    FechaInicioGrabacionMuestreo = as.Date(FechaInicioGrabacionMuestreo, format="%Y-%m-%d"),
    FechaInicioGrabacionMuestreo = format(FechaInicioGrabacionMuestreo, "%d/%m/%Y"),
    FechaFinGrabacionMuestreo = as.Date(FechaFinGrabacionMuestreo, format="%Y-%m-%d"),
    FechaFinGrabacionMuestreo = format(FechaFinGrabacionMuestreo, "%d/%m/%Y"))

ptos_sf <- st_as_sf(ptos, coords = c("Longitud", "Latitud"), crs = 4326)

# Cargar cuadrículas
UTM <- st_read("datos/Malla10x10_Ter_p.shp") %>%
  st_transform(crs = 4326)

# Unión espacial: asignar cada punto a la cuadrícula donde cae
ptos_con_UTM <- st_join(ptos_sf, UTM, join = st_within)

# Crear HTML de popup transpuesto
popup_html <- ptos_con_UTM %>%
  st_set_geometry(NULL) %>%
  group_by(UTMCODE) %>%
  summarise(
    popup = {
      # Datos de cada punto
      puntos <- rep("Punto", dplyr::n())
      # provincia <- Provincia
      especies  <- "(aún no disponible)"
      inicio    <- FechaInicioGrabacionMuestreo
      fin       <- FechaFinGrabacionMuestreo
      noches    <- Dias_Grabados_Campo
      habitat   <- Habitat
      grabadora <- Grabadora.ID
      
      # Columnas con ancho mínimo
      columnas <- paste0("<th style='padding:4px 12px; min-width:100px;'>", puntos, "</th>", collapse = "")
      # fila_provincia    <- paste0("<td style='padding:4px 12px; min-width:100px;'>", provincia, "</td>", collapse = "")
      fila_especies  <-paste0("<td style='padding:4px 12px; min-width:100px;'>", especies, "</td>", collapse = "")
      fila_inicio    <- paste0("<td style='padding:4px 12px; min-width:100px;'>", inicio, "</td>", collapse = "")
      fila_fin       <- paste0("<td style='padding:4px 12px; min-width:100px;'>", fin, "</td>", collapse = "")
      fila_noches    <- paste0("<td style='padding:4px 12px; min-width:100px;'>", noches, "</td>", collapse = "")
      fila_habitat   <- paste0("<td style='padding:4px 12px; min-width:100px;'>", habitat, "</td>", collapse = "")
      fila_grabadora <- paste0("<td style='padding:4px 12px; min-width:100px;'>", grabadora, "</td>", collapse = "")
      
      # Tabla envuelta en un div con scroll horizontal
      paste0(
        "<div style='overflow-x:auto; max-width:400px;'>",  # <- aquí ajusta el max-width del popup
        "<table style='border-collapse: collapse; width: 100%; border: 1px solid #ccc;'>",
        "<tr style='background-color:#f0f0f0;'><th style='padding:4px 12px;'></th>", columnas, "</tr>",
        "<tr><th style='padding:4px 12px;'>Especies</th>", fila_especies, "</tr>",
        "<tr><th style='padding:4px 12px;'>Hábitat</th>", fila_habitat, "</tr>",
        "<tr><th style='padding:4px 12px;'>Inicio</th>", fila_inicio, "</tr>",
        "<tr><th style='padding:4px 12px;'>Fin</th>", fila_fin, "</tr>",
        "<tr><th style='padding:4px 12px;'>Nº noches</th>", fila_noches, "</tr>",
        "<tr><th style='padding:4px 12px;'>ID grabadora</th>", fila_grabadora, "</tr>",
        "</table>",
        "</div>"
      )
    },
    .groups = "drop"
  )

# Unir info de popup a polígonos
cuad_con_info <- UTM %>%
  inner_join(popup_html, by = "UTMCODE")

# Crear mapa Leaflet
leaflet(cuad_con_info) %>%
  addTiles() %>%
  setView(lng = -3.7, lat = 40.4, zoom = 5) %>%
  addPolygons(
    fillOpacity = 0.1,
    color = "blue",
    weight = 1,
    popup = ~popup
  )
