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
library(shinycssloaders)

# --- Leer la base de datos unificadas ---
#datos_total <- read_csv("data/datos_total.csv", show_col_types = FALSE)

datos_total <- readRDS("datos_total.rds")
datos_total <- as.data.table(datos_total)

datos_demencia <- readRDS("datos_demencia.rds")
datos_demencia <- as.data.table(datos_demencia)

datos_suicidio <- readRDS("datos_suicidio.rds")
datos_suicidio <- as.data.table(datos_suicidio)

datos_servicios <- readRDS("datos_servicios.rds")
datos_servicios <- as.data.table(datos_servicios)

# Indices en las columnas de filtros
setkey(datos_total, Indicador1, Municipio, Tipo, Categoria, Ano)
setkey(datos_suicidio, Indicador11, Municipio, Sexo, Grupo_Etareo, Año)
setkey(datos_demencia, Indicador11, Municipio, Sexo, Grupo_Etareo, Año)
setkey(datos_servicios, Indicador11, Municipio, Sexo, Grupo_Etareo, Año)


# --- Crear listas auxiliares para filtros  ---
municipios <- sort(unique(datos_total$Municipio))
anios <- sort(unique(datos_total$Ano))
condicion <- sort(unique(datos_total$Condicion))
indicadores <- sort(unique(datos_total$Indicador1))

# --- Crear diccionario de indicadores por condición ---
indicadores_categoria <- datos_total[, .(indicadores = list(unique(Indicador1))), by = Condicion]
indicadores_categoria <- setNames(indicadores_categoria$indicadores, indicadores_categoria$Condicion)

cat("✅ Todas las bases de datos cargadas.\\n", nrow(datos_total), "filas y", ncol(datos_total), "columnas.\n")








