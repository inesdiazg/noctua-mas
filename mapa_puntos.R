library(leaflet)
library(glue)
library(htmltools)

ptos <- read.csv("datos/noctua.csv", sep = ";")

resumen_html <- glue(
  "<ul>
    <li><b>Total de grabadoras:</b> {length(unique(ptos$Grabadora.ID))}</li>
    <li><b>Número de responsables:</b> {length(unique(ptos$Responsable))}</li>
    <li><b>Cuadrículas UTM cubiertas:</b> {length(unique(ptos$UTM10_Asignada))}</li>
    <li><b>Noches grabadas:</b> {round(mean(ptos$Dias_Grabados_Campo),1)} de media 
    (mínimo: {min(ptos$Dias_Grabados_Campo)}, máximo: {max(ptos$Dias_Grabados_Campo)})</li>
  </ul>"
)

HTML(resumen_html)

leaflet(ptos) |>
  addTiles() |>
  setView(lng = -3.7, lat = 40.4, zoom = 5) |>
  addMarkers(
    lng = ~Longitud,
    lat = ~Latitud,
    popup = ~glue(
      "<div style='text-align: center; margin-bottom: 1px;'><span style='font-size: larger;'><b>{PuntoMuestreo}</b></span></div><br/>",
      "<div style='margin-top: 8px;'><b>Provincia: </b> {Provincia}</div>",
      "<div style='margin-top: 8px;'><b>Cuadrícula UTM10: </b> {UTM10_Asignada}</div>",
      "<div style='margin-top: 8px;'><b>Inicio de grabación: </b> {FechaInicioGrabacionMuestreo}</div>",
      "<div style='margin-top: 8px;'><b>Fin de grabación: </b> {FechaFinGrabacionMuestreo}</div>",
      "<div style='margin-top: 8px;'><b>Noches grabadas: </b> {Dias_Grabados_Campo}</div>",
      "<div style='margin-top: 8px;'><b>Hábitat: </b> {Habitat}</div>"
    )
  )

### Mapa con puntos:

# Haz clic en los iconos ![](images/marker-icon.png){width="16"} para ver la información sobre los puntos de muestreo.

leaflet(ptos) |>
  addTiles() |>
  setView(lng = -3.7, lat = 40.4, zoom = 5) |>
  addMarkers(
    lng = ~Longitud,
    lat = ~Latitud,
    popup = ~glue(
      "<div style='text-align: center; margin-bottom: 1px;'><span style='font-size: larger;'><b>{PuntoMuestreo}</b></span></div><br/>",
      "<div style='margin-top: 8px;'><b>Provincia: </b> {Provincia}</div>",
      "<div style='margin-top: 8px;'><b>Cuadrícula UTM10: </b> {UTM10_Asignada}</div>",
      "<div style='margin-top: 8px;'><b>Inicio de grabación: </b> {FechaInicioGrabacionMuestreo}</div>",
      "<div style='margin-top: 8px;'><b>Fin de grabación: </b> {FechaFinGrabacionMuestreo}</div>",
      "<div style='margin-top: 8px;'><b>Noches grabadas: </b> {Dias_Grabados_Campo}</div>",
      "<div style='margin-top: 8px;'><b>Hábitat: </b> {Habitat}</div>"
    )
  )