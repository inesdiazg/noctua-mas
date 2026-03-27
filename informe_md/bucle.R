library(rmarkdown)
library(pagedown)
library(dplyr)

#setwd("C:/Users/inesi/OneDrive - UAM (1)/Producto_BirdNET/informe_md")
setwd("C:/noctua-mas/informe_md")
datos_bucle <- read.csv("noctua.csv",sep=";",header=TRUE, stringsAsFactors = FALSE)
datos_bucle <- datos_bucle %>%
  mutate(
    FechaInicioGrabacionMuestreo = as.Date(FechaInicioGrabacionMuestreo),
    FechaInicioGrabacionMuestreo = format(FechaInicioGrabacionMuestreo, "%d/%m/%Y"),
    FechaFinGrabacionMuestreo = as.Date(FechaFinGrabacionMuestreo),
    FechaFinGrabacionMuestreo = format(FechaFinGrabacionMuestreo, "%d/%m/%Y"))
datos_bucle <- head(datos_bucle, n=1) # Eliminar fila en la versión definitiva

# Crear los HTMLs
for(i in 1:nrow(datos_bucle)) {
  render("boceto_bucle_html.Rmd",
         output_file = paste0("informe_", datos_bucle$PuntoMuestreo[i], ".html"),
         params = list(puntos = datos_bucle[i, ]),
         envir = new.env())}

# Imprimir HTMLs a PDFs
for(i in 1:nrow(datos_bucle)) {
  html_file <- paste0("informe_", datos_bucle$PuntoMuestreo[i], ".html")
  pdf_file  <- paste0("informe_", datos_bucle$PuntoMuestreo[i], ".pdf")
  chrome_print(input = html_file, output = pdf_file)}
