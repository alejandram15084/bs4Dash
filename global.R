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
indicadores_categoria <- datos_total[, .(indicadores = list(sort(unique(Indicador1)))), by = Condicion]
indicadores_categoria <- setNames(indicadores_categoria$indicadores, indicadores_categoria$Condicion)

cat("✅ Todas las bases de datos cargadas.\\n", nrow(datos_total), "filas y", ncol(datos_total), "columnas.\n")

# -----------------------------------------------------------------------
# CONSTANTES Y FUNCIONES AUXILIARES
# -----------------------------------------------------------------------

# Municipios mostrados por defecto en todos los pickerInput.
# Centralizado aquí para no repetirlo en 6+ lugares del server.R
MUNICIPIOS_DEFAULT <- c("La Union", "El Carmen", "Rionegro", "La Ceja", "El Retiro")


# Etiqueta del eje Y según la pestaña activa.
# Reemplaza el switch() que estaba duplicado 3 veces en server.R
etiqueta_eje_y <- function(tab) {
  switch(tab,
    "Infraestructura" = "Número",
    "Años Perdidos"   = "Tasa por 100.000 personas",
    "Atención"        = "Porcentaje (%)",
    "Hospitalización" = "Porcentaje (%)",
    "Mortalidad"      = "Tasa por 100.000 personas",
    "Letalidad"       = "Porcentaje (%)",
    "Valor"           # valor por defecto si no hay coincidencia
  )
}


# Indicadores pre-filtrados por pestaña usando regex.
# Evita ejecutar str_detect() en caliente en cada cambio de filtro del server.R
indicadores_avpp <- datos_total[
  str_detect(Indicador1, regex("año|vida|perd", ignore_case = TRUE)),
  unique(Indicador1)
]
indicadores_atencion <- datos_total[
  str_detect(Indicador1, regex("atendid", ignore_case = TRUE)),
  unique(Indicador1)
]
indicadores_hospitalizacion <- datos_total[
  str_detect(Indicador1, regex("hospitalizad", ignore_case = TRUE)),
  unique(Indicador1)
]

# -----------------------------------------------------------------------
# CACHÉ COMPARTIDO ENTRE SESIONES (cache = "app" en bindCache)
# -----------------------------------------------------------------------
shinyOptions(cache = cachem::cache_mem(
  max_size = 256 * 1024^2,  # 256 MB
  max_age  = 3600            # 1 hora
))

# -----------------------------------------------------------------------
# HELPERS MOVIDOS DESDE server.R
# -----------------------------------------------------------------------

# Detectar columna de valor en datos_servicios (una sola vez al arrancar)
valor_col_servicios <- if ("Valor1" %in% names(datos_servicios)) {
  "Valor1"
} else if ("Valor" %in% names(datos_servicios)) {
  "Valor"
} else {
  stop("No se encontró columna de valor en datos_servicios.")
}

# Salto de línea para etiquetas largas en gráficos de barras
wrap_text <- function(text, width = 80) {
  wrapped <- stringr::str_wrap(text, width = width)
  gsub("\n", "<br>", wrapped)
}
