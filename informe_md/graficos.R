setwd("C:/Users/inesi/OneDrive - UAM (1)/Producto_BirdNET/Figuras TFM Guzmán")
datos <- read.csv("Cantos_Corregidos_Relativos_Al_Atardecer.csv",sep=";",header=TRUE, stringsAsFactors = FALSE)


# gráficos violín ---------------------------------------------------------

# Cargar librerías necesarias
library(ggplot2)
library(dplyr)
library(readr)
library(hrbrthemes)
library(forcats)

# Filtrar solo las especies válidas
especies_validas <- c(
  "Asio otus", "Athene noctua","Caprimulgus europaeus", "Caprimulgus ruficollis",
  "Otus scops", "Strix aluco", "Tyto alba"
)

colores_especies <- c(
  "Otus scops" = "#4dc0b5",
  "Athene noctua" = "#3490dc",
  "Strix aluco" = "#9561e2",
  "Caprimulgus europaeus" = "#e3342f",
  "Caprimulgus ruficollis" = "#f6993f",
  "Gryllotalpa gryllotalpa" = "#ffed4a",
  "Gryllus sp." = "#f66d9b",
  "Asio otus" = "#38c172",
  "Tyto alba" = "#6574cd"
)

# Filtrar solo las especies diana
data_filtrada <- datos %>%
  filter(Species %in% especies_validas)

# Reordenar especies si se desea (opcional)
data_filtrada$Species <- factor(data_filtrada$Species, levels = especies_validas)

# Crear gráfico de violín horizontal. Este es para densidad de nº de cantos
p <- ggplot(data_filtrada, aes(x = Minutes_From_Sunset, y = fct_rev(Species), fill = Species, color = Species)) +
  geom_violin(scale = "width", adjust = 1.1, trim = FALSE) +
  scale_fill_manual(values = colores_especies) +
  scale_color_manual(values = colores_especies) +
  theme_ipsum() +
  theme(legend.position = "none") +
  xlab("") +
  ylab("Especie") +
  ggtitle("") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black")  # Marca el atardecer
p
ggsave("Gráfico_violin_num_cantos_def.png", width = 10, height = 6, dpi = 300)

#Este es para tiempo total(o sea Delta time)
q <- ggplot(data_filtrada, aes(
  x = Minutes_From_Sunset,
  y = fct_rev(Species),
  fill = Species,
  color = Species,
  weight = Delta.Time..s.  # duración de cada canto
)) +
  geom_violin(scale = "area", trim = FALSE, adjust = 1.1) +
  scale_fill_manual(values = colores_especies) +
  scale_color_manual(values = colores_especies) +
  theme_ipsum() +
  theme(legend.position = "none") +
  xlab("") +
  ylab("Especie") +
  ggtitle("") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black")  # Marca el atardecer
q
ggsave("Gráfico_violin_tiempo_cantos_def.png", width = 10, height = 6, dpi = 300)

