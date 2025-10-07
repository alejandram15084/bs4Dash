# --- Librerías principales ---
library(shiny)          # Aplicaciones web
library(bs4Dash)        # Estilo de dashboard
library(readr)          # Lectura de CSV
library(dplyr)          # Manipulación de datos
library(plotly)         # Gráficos interactivos
library(leaflet)        # Mapas interactivos
library(DT)             # Tablas interactivas
library(shinyWidgets)   # Inputs avanzados para Shiny
library(janitor)        # Limpieza de datos
library(purrr)          # Funciones funcionales (map, etc.)
library(shinyjs)        # Funciones adicionales para Shiny
library(stringr)        # Manejo de texto
library(echarts4r)      # Gráficos con librería JS Echarts
library(tibble)
library(data.table)

# --- Leer la base de datos unificada ---
#datos_total <- read_csv("data/datos_total.csv", show_col_types = FALSE)

datos_total <- readRDS("datos_total.rds")
datos_total <- as.data.table(datos_total)


# Indices en las columnas de filtros
setkey(datos_total, Indicador1, Municipio, Ano)


# --- Crear listas auxiliares para filtros (si usas selectInput o pickerInput) ---
municipios <- sort(unique(datos_total$Municipio))
anios <- sort(unique(datos_total$Ano))
condicion <- sort(unique(datos_total$Condicion))
indicadores <- sort(unique(datos_total$Indicador1))

# --- Crear diccionario de indicadores por condición ---
indicadores_categoria <- datos_total %>%
  group_by(Condicion) %>%
  summarise(indicadores = list(unique(Indicador1))) %>%
  deframe()

cat("✅ Base de datos cargada con", nrow(datos_total), "filas y", ncol(datos_total), "columnas.\n")
