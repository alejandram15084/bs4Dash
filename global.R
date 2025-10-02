library(shiny) #aplicaciones web
library(bs4Dash) #Estilo de dashboard
#library(shinydashboard) #estructura dashboard
library(readr) #Lectura CSV
library(dplyr) #Manipulación de datos
library(plotly) #Gráficos interactivos
library(leaflet) #Mapas interactivos
library(DT) #Tablas interactivas
library(shinyWidgets) #Inputs para shiny
library(janitor) #Limpieza de datos
library(purrr) #Map
library(shinyjs) #Funciones para Shiny
library(stringr)
library(echarts4r) #Gráficos con la librería JS Echarts

# --- Ruta a los datos  ---
ruta_sexo <- "C:/Users/aleja/OneDrive/Documentos/bs4Dash/data/sexo"
ruta_edad <- "C:/Users/aleja/OneDrive/Documentos/bs4Dash/data/edad"
ruta_geografia <- "C:/Users/aleja/OneDrive/Documentos/bs4Dash/data/geografia"

# --- Leer BD geografía ---
archivos_geografia <- list.files(path = ruta_geografia,
                                 pattern = "\\.csv$",
                                 full.names = TRUE) #buscar archivos carpeta
datos_geografia <- lapply(archivos_geografia,
                          function(file) {
                            df <- read_csv(file, show_col_types = FALSE)
                            municipio <- gsub("1\\.|\\.csv", "",
                                              basename(file))
                            df$Municipio <- municipio
                            df <- df %>%
                              mutate(Categoria = "Geografia",    # columna para unificar
                                Tipo = "Geografia"
                              ) %>%
                              select(Municipio,
                                     Indicador1,
                                     Ano,
                                     Tipo,
                                     Categoria,
                                     Valor1) #categorias interes
                            return(df)
                          })  %>%
  bind_rows()

# --- Leer BD sexo ---
archivos_sexo <- list.files(path = ruta_sexo,
                            pattern = "\\.csv$",
                            full.names = TRUE) #buscar archivos de la carpeta

datos_sexo <- lapply(archivos_sexo,
                     function(file) {
                       df <- read_csv(file, show_col_types = FALSE)
                       municipio <- gsub("2\\.|\\.csv",
                                         "",
                                         basename(file))
                       df$Municipio <- municipio
                       df <- df %>%
                         mutate(
                           Categoria = sexo,    # columna unificada
                           Tipo = "Sexo"
                         ) %>%
                         select(Municipio,
                                Indicador1,
                                Ano,
                                Tipo,
                                Categoria,
                                Valor1) #categorias de interes
                       return(df)
                     }) %>%
  bind_rows()


# --- Leer BD edad ---
archivos_edad <- list.files(path = ruta_edad,
                            pattern = "\\.csv$",
                            full.names = TRUE)

datos_edad <- lapply(archivos_edad,
                     function(file) {
                       df <- read_csv(file, show_col_types = FALSE)
                       municipio <- gsub("3\\.|\\.csv", "", basename(file))
                       df$Municipio <- municipio
                       df <- df %>%
                         mutate(
                           Categoria = Grupo_Etareo,  # columna unificada
                           Tipo = "Edad"
                         ) %>%
                         select(Municipio,
                                Indicador1,
                                Ano,
                                Tipo,
                                Categoria,
                                Valor1) #categorias de interes
                       return(df)
                     }) %>%
  bind_rows()


# --- Unir todo ---
datos_total <- bind_rows(datos_sexo, datos_edad, datos_geografia)


# --- Normalizar categorías ---
datos_total <- datos_total %>%
  mutate(Categoria = str_to_title( #mayúscula inicial
                                  trimws(Categoria)))  #elimina espacios

# Normalizar Valor1 a numérico (reemplaza comas por puntos si aplica)
datos_total <- datos_total %>%
  mutate(Valor1 = Valor1 %>%
           str_replace_all(",", ".") %>%
           str_trim() %>%
           as.numeric())


# --- Extraer variables útiles ---
indicadores <- sort(unique(datos_total$Indicador1)) #indicadores disponibles
years <- sort(unique(datos_total$Ano)) #años disponibles
sexos <- sort(unique(na.omit(datos_sexo$Categoria[datos_total$Tipo == "Sexo"]))) #categorías de sexo
edades <- sort(unique(na.omit(datos_edad$Categoria[datos_total$Tipo == "Edad"]))) #grupos de edad
municipios <- sort(unique(datos_total$Municipio)) #lista de municipio


# --- Normalizar nombres ---
datos_total <- datos_total %>%
  mutate(Indicador1 = str_trim(Indicador1))



# --- Lista de indicadores ---
indicadores_categoria <- list(
                              "Infraestructura" = c("Número de IPS habilitadas con el servicio de neuropediatría",
                                                    "Número de IPS habilitadas con el servicio de psiquiatría",
                                                    "Número de IPS habilitadas con el servicio de psicología",
                                                    "Número de IPS habilitadas con el servicio de psiquiatría o unidad de salud mental",
                                                    "Número de IPS habilitadas con el servicio de neurología",
                                                    "Número de camas de farmacodependencia",
                                                    "Número de camas de cuidado agudo mental",
                                                    "Número de camas de psiquiatría",
                                                    "Número de camas de cuidado intermedio mental"),

                              "Años perdidos" = c("Tasa de años de vida potencialmente perdidos por lesiones autoinfligidas intencionalmente",
                                                  "Tasa de años de vida potencialmente perdidos por  Trastorno mental no especificado",
                                                  "Tasa de años de vida potencialmente perdidos por  Trastornos emocionales y del comportamiento que aparecen habitualmente en la niñez y en la adolescencia",
                                                  "Tasa de años de vida potencialmente perdidos por  Trastornos del desarrollo psicológico",
                                                  "Tasa de años de vida potencialmente perdidos por  Retraso mental",
                                                  "Tasa de años de vida potencialmente perdidos por  Trastornos de la personalidad y del comportamiento en adultos",
                                                  "Tasa de años de vida potencialmente perdidos por  Síndromes del comportamiento asociados con alteraciones fisiológicas y factores físicos",
                                                  "Tasa de años de vida potencialmente perdidos por  Trastornos neuróticos, trastornos relacionados con el estrés y trastornos somatomorfos",
                                                  "Tasa de años de vida potencialmente perdidos por  Trastornos del humor [afectivos]",
                                                  "Tasa de años de vida potencialmente perdidos por  Esquizofrenia, trastornos esquizotípicos y trastornos delirantes",
                                                  "Tasa de años de vida potencialmente perdidos por  Trastornos mentales y del comportamiento debidos al uso de sustancias psicoactivas",
                                                  "Tasa de años de vida potencialmente perdidos por  Trastornos mentales orgánicos, incluidos los trastornos sintomáticos",
                                                  "Tasa de años de vida potencialmente perdidos por trastornos mentales y del comportamiento"),


                              "Atención"        = c("Porcentaje de personas atendidas por Psicosis de origen no organico, no especificado; por Trastorno afectivo bipolar",
                                                    "Porcentaje de personas atendidas problemas relacionados con el ambiente social",
                                                    "Porcentaje de personas atendidas problemas relacionados con la vivienda y las circunstancias",
                                                    "Porcentaje de personas atendidas por Estado de mal epileptico de tipo no especificado",
                                                    "Porcentaje de personas atendidas por Otros estados epilépticos",
                                                    "Porcentaje de personas atendidas por Estado de mal epileptico parcial complejo",
                                                    "Porcentaje de personas atendidas por Estado de pequeño mal epileptico",
                                                    "Porcentaje de personas atendidas por Estado de gran mal epileptico",
                                                    "Porcentaje de personas atendidas por Epilepsia, tipo no especificado",
                                                    "Porcentaje de personas atendidas por Otras epilepsias",
                                                    "Porcentaje de personas atendidas por Pequeño mal, no especificado (sin ataque de gran mal)",
                                                    "Porcentaje de personas atendidas por Ataques de gran mal, no especificados (con o sin pequeño mal)",
                                                    "Porcentaje de personas atendidas por Sindromes epilepticos especiales",
                                                    "Porcentaje de personas atendidas por Otras epilepsias y sindromes epilepticos generalizados",
                                                    "Porcentaje de personas atendidas por Epilepsia y sindromes epilepticos idiopaticos generalizados",
                                                    "Porcentaje de personas atendidas por Epilepsia y sindromes epilepticos sintomaticos relacionados con localizaciones (focales) (parciales) y con ataques parciales complejos",
                                                    "Porcentaje de personas atendidas por Epilepsia y sindromes epilepticos sintomaticos relacionados con localizaciones (focales) (parciales) y con ataques parciales simples",
                                                    "Porcentaje de personas atendidas por Epilepsia y sindromes epilepticos idiopaticos relacionados con localizaciones (focales) (parciales) y con ataques de inicio localizado",
                                                    "Porcentaje de personas atendidas por epilepsia(CIE-10: G40 – G41)",
                                                    "Porcentaje de personas atendidas por Psicosis de origen no organico, no especificado; por Trastorno afectivo bipolar;por Episodio depresivo moderado; por Episodio depresivo grave sin sintomas psicoticos; por Episodio depresivo grave con sintomas psicotico",
                                                    "Porcentaje de personas atendidas por  problemas relacionados con otras circunstancias psicosociales",
                                                    "Porcentaje de personas atendidas por  problemas relacionados con ciertas circunstancias psicosociales",
                                                    "Porcentaje de personas atendidas por otros problemas relacionados con el grupo primario de apoyo, inclusive circunstancias familiares",
                                                    "Porcentaje de personas atendidas problemas relacionados con hechos Otros problemas relacionados con la crianza del niño",
                                                    "Porcentaje de personas atendidas problemas relacionados con hechos negativos en la niñez",
                                                    "Porcentaje de personas atendidas problemas relacionados con el ambiente social económicas",
                                                    "Porcentaje de personas atendidas problemas relacionados con la vivienda y las circunstancias económicas",
                                                    "Porcentaje de personas atendidas problemas relacionados con el ambiente físico",
                                                    "Porcentaje de personas atendidas por exposición a factores de riesgo ocupacional",
                                                    "Porcentaje de personas atendidas por problemas relacionados con el empleo y el desempleo",
                                                    "Porcentaje de personas atendidas por problemas relacionados con la educación y la alfabetización",
                                                    "Porcentaje de personas atendidas por riesgos potenciales para su salud, relacionados con circunstancias socioeconómicas y psicosociales",
                                                    "Porcentaje de personas atendidas por Trastorno mental no especificado",
                                                    "Porcentaje de personas atendidas por Trastornos emocionales y del comportamiento que aparecen habitualmente en la niñez y en la adolescencia",
                                                    "Porcentaje de personas atendidas por Trastornos del desarrollo psicológico",
                                                    "Porcentaje de personas atendidas por Retraso mental",
                                                    "Porcentaje de personas atendidas por Trastornos de la personalidad y del comportamiento en adultos",
                                                    "Porcentaje de personas atendidas por Síndromes del comportamiento asociados con alteraciones fisiológicas y factores físicos",
                                                    "Porcentaje de personas atendidas por Trastornos neuróticos, trastornos relacionados con el estrés y trastornos somatomorfos",
                                                    "Porcentaje de personas atendidas por Trastornos del humor [afectivos]",
                                                    "Porcentaje de personas atendidas por Esquizofrenia, trastornos esquizotípicos y trastornos delirantes",
                                                    "Porcentaje de personas atendidas por Trastornos mentales y del comportamiento debidos al uso de sustancias psicoactivas",
                                                    "Porcentaje de personas atendidas por Trastornos mentales orgánicos, incluidos los trastornos sintomáticos",
                                                    "Porcentaje de personas atendidas por trastornos mentales y del comportamiento"),


                              "Hospitalización" = c("Porcentaje de personas hospitalizadas por Trastornos emocionales y del comportamiento que aparecen habitualmente en la niñez y en la adolescia",
                                                    "Porcentaje de personas hospitalizadas por Trastorno mental no especificado",
                                                    "Porcentaje de personas hospitalizadas por Trastornos emocionales y del comportamiento que aparecen habitualmente en la niñez y en la adolescencia",
                                                    "Porcentaje de personas hospitalizadas por Trastornos del desarrollo psicológico",
                                                    "Porcentaje de personas hospitalizadas por Retraso mental",
                                                    "Porcentaje de personas hospitalizadas por Trastornos de la personalidad y del comportamiento en adultos",
                                                    "Porcentaje de personas hospitalizadas por Síndromes del comportamiento asociados con alteraciones fisiológicas y factores físicos",
                                                    "Porcentaje de personas hospitalizadas por Trastornos neuróticos, trastornos relacionados con el estrés y trastornos somatomorfos",
                                                    "Porcentaje de personas hospitalizadas por Trastornos del humor [afectivos]",
                                                    "Porcentaje de personas hospitalizadas por Esquizofrenia, trastornos esquizotípicos y trastornos delirantes",
                                                    "Porcentaje de personas hospitalizadas por Trastornos mentales y del comportamiento debidos al uso de sustancias psicoactivas",
                                                    "Porcentaje de personas hospitalizadas por Trastornos mentales orgánicos, incluidos los trastornos sintomáticos",
                                                    "Porcentaje de personas hospitalizadas por  trastornos mentales y del comportamiento"),


                              "Mortalidad"      = c("Tasa ajustada de mortalidad por epilepsia",
                                                    "Tasa ajustada de mortalidad por trastornos mentales y del comportamiento",
                                                    "Tasa ajustada de mortalidad por lesiones autoinflingidas intencionalmente"),

                              "Letalidad"       =c("Letalidad por lesiones autoinflingidas intencionalmente",
                                                   "Letalidad de intoxicaciones"))



#colnames(datos_total)
#unique(datos_total$Indicador1)
#names(datos_total),
#unique(datos_total$Categoria)

# Guardar base unificada
#write_csv(datos_total, "C:/Users/aleja/OneDrive/Documentos/bs4Dash/data/datos_total.csv")

