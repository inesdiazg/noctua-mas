### pruebas output birdnet a tabla presencia-ausencia

library(dplyr)
library(tidyverse)
library(hms)

birdnet <- read.csv("C:/Users/inesi/OneDrive - UAM (1)/Producto_BirdNET/audios_prueba/002/BirdNET_CombinedTable.csv",sep=",",header=TRUE, stringsAsFactors = FALSE)

# Definir umbral de confianza
min_conf <- 0.6

# por pasos por fragmento ---------------------------------------------------------------

# 1. Corregir escalas
birdnet_clean <- birdnet %>%
  mutate(Start..s. = Start..s. / 10, End..s. = End..s. / 10, Confidence = Confidence / 10000)

# 2. Para cada fragmento, quedarse con el ave con mayor confidence
birdnet_max <- birdnet_clean %>%
  group_by(Start..s., End..s.) %>%
  slice_max(Confidence, n = 1, with_ties = FALSE) %>%
  ungroup()

# 3. Crear tabla presencia/ausencia (una fila por fragmento)
birdnet_wide <- birdnet_max %>%
  mutate(presence = 1) %>%
  select(Start..s., End..s., Scientific.name, presence) %>%
  pivot_wider(
    names_from = Scientific.name,
    values_from = presence,
    values_fill = 0
  )


# por fragmento --------------------------------------------------------------

birdnet_wide <- birdnet %>%
  mutate(
    Start = Start..s. / 10,
    End = End..s. / 10,
    Confidence = Confidence / 10000
  ) %>%
  group_by(Start, End) %>%
  slice_max(Confidence, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(presence = as.integer(Confidence > min_conf)) %>%
  select(Start, End, Scientific.name, presence) %>%
  pivot_wider(
    names_from = Scientific.name,
    values_from = presence,
    values_fill = 0
  )

# por sitio ----------------------------------------------------

all_species <- unique(birdnet$Scientific.name)

birdnet <- birdnet %>%
  mutate(
    Fecha = as.Date(str_extract(File, "(?<=_)\\d{8}(?=_)"), format = "%Y%m%d"),
    Hora = str_extract(File, "(?<=_)\\d{6}(?=\\.WAV)") |>
      str_replace("(\\d{2})(\\d{2})(\\d{2})", "\\1:\\2:\\3") |>
      as_hms(),
    Grabadora = str_extract(File, "(?<=audios_prueba\\\\)\\d+(?=\\\\)")
  )

birdnet_site <- birdnet %>%
  # mutate(Confidence = Confidence / 10000) %>%
  group_by(Start..s., End..s.) %>%
  slice_max(Confidence, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  filter(Confidence > min_conf) %>%
  distinct(Scientific.name) %>%
  mutate(Presencia = 1) %>%
  right_join(
    data.frame(Scientific.name = all_species),
    by = "Scientific.name") %>%
  mutate(Presencia = ifelse(is.na(Presencia), 0, 1)) %>%
  pivot_wider(
    names_from = Scientific.name,
    values_from = Presencia)

birdnet_site <- birdnet_site %>%
  mutate(
    Fecha = birdnet$Fecha,
    Grabadora = birdnet$Grabadora)

str(birdnet)

  
# para cuando haya más sitios ---------------------------------------------

# 1. Definir umbral de confianza a partir del cual considerar "presencia"
min_conf <- 0.6

# 2. Leer todos los archivos de una carpeta (donde solo están los datos en .csv)
files <- list.files("C:/Users/inesi/OneDrive - UAM (1)/Producto_BirdNET/audios/csvs/", full.names = TRUE)

birdnet_all <- files %>%
  set_names() %>% # dar a los elementos del vector el nombre del archivo que irá en PuntoMuestreo
  map_dfr(~ read_csv2(.x), .id = "PuntoMuestreo")

# Si queremos que Site sea el nombre del archivo limpio:
birdnet_all$PuntoMuestreo <- tools::file_path_sans_ext(basename(birdnet_all$PuntoMuestreo))

# 3. Corregir confidence (porque en el .csv que sale de birdNET me salía mal el valor)
birdnet_all <- birdnet_all %>%
  mutate(Confidence = Confidence / 10000)

# 4. Definir especies de interés
especies <- unique(birdnet_all$`Scientific name`) # cambiarlo por la lista de especies de interés)

# 5. Crear la tabla final de presncia/ausencia por sitio
PuntosMuestreo <- birdnet_all %>%
  group_by(PuntoMuestreo, `Start (s)`, `End (s)`) %>% # agrupar por detección por fragmento
  slice_max(Confidence, n = 1, with_ties = FALSE) %>% # en cada grupo, quedarse con 1, el de mayor confianza
  ungroup() %>%
  filter(Confidence > min_conf) %>%
  distinct(PuntoMuestreo, `Scientific name`) %>% # quedarse con 1 combinación única por punto y especie
  mutate(presence = 1) %>%
  right_join(
    expand.grid(
      PuntoMuestreo = unique(birdnet_all$PuntoMuestreo),
      `Scientific name` = especies), # generar todas las combinaciones posibles entre: todos los puntos de muestreo y todas las especies (vector "especies")
    by = c("PuntoMuestreo", "Scientific name")) %>%
  mutate(presence = ifelse(is.na(presence), 0, 1)) %>%
  pivot_wider(
    names_from = `Scientific name`,
    values_from = presence)

PuntosMuestreo

# por fecha (para gráfico de especies por nº de noches) ---------------------------------------------------------------

library(dplyr)

birdnet <- birdnet %>%
  mutate(
    Fecha = as.Date(str_extract(File, "(?<=_)\\d{8}(?=_)"), format = "%Y%m%d"),
    Hora = str_extract(File, "(?<=_)\\d{6}(?=\\.WAV)") |>
      str_replace("(\\d{2})(\\d{2})(\\d{2})", "\\1:\\2:\\3") |>
      as_hms(),
    Grabadora = str_extract(File, "(?<=audios_prueba\\\\)\\d+(?=\\\\)"))

noches_especies <- birdnet %>%
  filter(Confidence >= min_conf) %>%          # aseguramos detecciones válidas
  distinct(Scientific.name, Fecha) %>%        # una fila por especie y día
  count(Scientific.name, name = "n_noches") %>% # contar días
  arrange(n_noches)
