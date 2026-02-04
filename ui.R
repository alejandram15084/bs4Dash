library(shiny)
library(bs4Dash)

ui <- bs4DashPage(

  # --- HEADER ---
  header = dashboardHeader(
    title = dashboardBrand(
      title = "Dashboard",
      color = "gray-dark",
      href = "https://www.udea.edu.co/wps/portal/udea/web/inicio",
      image = "log.png"
    ),
    skin = "light",
    border = TRUE,
    controlbarIcon = NULL,
    rightUi = NULL
  ),

  # --- SIDEBAR ---
  sidebar = bs4DashSidebar(
    bs4SidebarMenu(
      id = "tabs_menu",
      bs4SidebarMenuItem(
        "Indicadores salud mental",
        tabName = "indicadores",
        icon = icon("chart-line")
      ),
      bs4SidebarMenuItem(
        "Tasa de intento de suicidio",
        tabName = "suicidio",
        icon = icon("heartbeat")
      ),
      bs4SidebarMenuItem(
        "Indicador de demencia",
        tabName = "demencia",
        icon = icon("brain")
      ),
      bs4SidebarMenuItem(
        "Uso de servicios",
        tabName = "servicios",
        icon = icon("hospital")
      )
    )
  ),

  # --- BODY ---
  body = bs4DashBody(

    # Enlace a CSS
    tags$head(
      tags$link(rel = "stylesheet",
                type = "text/css",
                href = "custom.css"),
      tags$script(src = "custom.js")
    ),

    bs4TabItems(
      # --- PESTAÑA 1: INDICADORES ---
      bs4TabItem(
        tabName = "indicadores",

        # PANEL DE TABS
        fluidRow(
          column(
            width = 12,
            tabsetPanel(
              id = "tab_activa",
              type = "pills",
              selected = "Infraestructura",

              # --- SECCION 1 ---
              tabPanel("Infraestructura",
                uiOutput("contenido_infraestructura")
              ),

              # --- SECCION 2---
              tabPanel("Años Perdidos",
                uiOutput("contenido_añosperdidos")
              ),

              # --- SECCION 3 ---
              tabPanel("Atención",
                uiOutput("contenido_atencion")
              ),

              # --- SECCION 4 ---
              tabPanel("Hospitalización",
                uiOutput("contenido_hospitalizacion")
              ),

              # --- SECCION 5 ---
              tabPanel("Mortalidad",
                uiOutput("contenido_mortalidad")
              ),

              # --- SECCION 6 ---
              tabPanel("Letalidad",
                uiOutput("contenido_letalidad")
              )
            )
          )
        ),
        br(),
        # CAJA DE DEFINICIONES
        fluidRow(
          column(
            width = 12,
            box(
              title = "Definición",
              width = 12,
              collapsible = FALSE,
              uiOutput("definicion_ui")
            )
          )
        ),
        br(),
        # ================== GRÁFICO DE LINEAS PRINCIPAL ===================
        # FILTRO GLOBAL
        fluidRow(
          bs4Card(
            title = "Filtros de análisis",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            collapsible = FALSE,
            # Primera fila
            fluidRow(
              column(8, uiOutput("indicador_ui")),
              column(4, uiOutput("muni_ui"))
            ),
            # Segunda fila
            fluidRow(
              column(2, selectInput("year_min", "Año desde",
                                    choices = anios, selected = min(anios))),
              column(2, selectInput("year_max", "Año hasta",
                                    choices = anios, selected = max(anios))),
              column(2, uiOutput("tipo_ui")),
              column(3, uiOutput("categoria_ui")),
              column(3,
                div(style = "text-align: right; padding-top: 25px;",
                  #actionButton("run_query", "Actualizar", class = "btn btn-primary"),
                  actionButton("reset_filters", "Limpiar filtros",
                               class = "btn btn-secondary")
                )
              )
            )
          )
        ),
        # ================== GRÁFICO LINEAS ===================
        fluidRow(
          bs4Card(
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            #title = "Serie temporal",
            collapsible = FALSE,
            br(),
            plotlyOutput("linePlot", height = "400px")
            %>% withSpinner(color = "black")
          )
        ),
        br(),
        # ================== CAJA DE KPIS ===========================
        uiOutput("kpi_boxes"),
        br(),
        # ================== GRÁFICO DE BARRAS SEXO ===================
        conditionalPanel(
          condition = "input.tab_activa != 'Infraestructura'",
          fluidRow(
            bs4Card(
              width = 12,
              status = "primary",
              solidHeader = TRUE,
              collapsible = FALSE,
              title = "Comparación por sexo",
              fluidRow(
                column(
                  width = 8,
                  uiOutput("indicador_sexo_ui")
                ),
                column(
                  width = 3,
                  selectInput(
                    "anio_sexo",
                    "Año",
                    choices = sort(unique(datos_total$Ano)),
                    selected = max(datos_total$Ano)
                  )
                )
              ),
              br(),
              plotlyOutput("barPlotSexo", height = "400px")
              %>% withSpinner(color = "black")
            )
          )
        ),
        # ==================== GRAFICA ETARIO ====================
        conditionalPanel(
          condition = "input.tab_activa != 'Infraestructura' 
          && input.tab_activa != 'Mortalidad'",
          fluidRow(
            bs4Card(
              width = 12,
              solidHeader = TRUE,
              status = "primary",
              collapsible = FALSE,
              title = "Comparación por grupo etario",
              fluidRow(
                column(width = 6, uiOutput("indicador_edad_ui")),
                column(width = 3, uiOutput("municipio_edad_ui")),
                column(width = 3, uiOutput("anio_edad_ui"))
              ),
              br(),
              plotlyOutput("barPlotEdad", height = "400px")
              %>% withSpinner(color = "black")
            )
          )
        ),
        # ================== SOLO PARA AÑOS PERDIDOS: GRÁFICO DE BARRAS ===================
        conditionalPanel(
          condition = "input.tab_activa == 'Años Perdidos'",
          fluidRow(
            bs4Card(
              width = 12,
              status = "primary",
              solidHeader = TRUE,
              collapsible = FALSE,
              title = "Principales causas de años de vida potencialmente perdidos",
              uiOutput("anio_seleccionado_ui"),
              plotlyOutput("grafico_top_causas", height = "400px")  
              %>% withSpinner(color = "black")
            )
          )
        ),
        # ================== SOLO PARA ATENCIÓN: GRÁFICO DE BARRAS ===================
        conditionalPanel(
          condition = "input.tab_activa == 'Atención'",
          fluidRow(
            bs4Card(
              width = 12,
              status = "primary",
              solidHeader = TRUE,
              collapsible = FALSE,
              uiOutput("anio_atencion_ui"),
              plotlyOutput("grafico_top_atencion", height = "400px")
              %>% withSpinner(color = "black")
            )
          )
        ),
        # ================== SOLO PARA HOSPITALIZACIÓN: GRÁFICO DE BARRAS ===================
        conditionalPanel(
          condition = "input.tab_activa == 'Hospitalización'",
          fluidRow(
            bs4Card(
              width = 12,
              status = "primary",
              solidHeader = TRUE,
              collapsible = FALSE,
              uiOutput("anio_hospitalizacion_ui"),
              plotlyOutput("grafico_top_hospitalizacion", height = "400px")
              %>% withSpinner(color = "black")
            )
          )
        ),
        # ================== SOLO PARA LETALIDAD: GRÁFICO DE BARRAS ===================
        conditionalPanel(
          condition = "input.tab_activa == 'Letalidad'",
          fluidRow(
            bs4Card(
              width = 12,
              status = "primary",
              solidHeader = TRUE,
              collapsible = FALSE,
              uiOutput("anio_letalidad_ui"),
              plotlyOutput("grafico_lesiones_autoinfligidas", height = "400px")
              %>% withSpinner(color = "black")
            )
          )
        )
      ),
      # --- PESTAÑA 2: SUICIDIO ---
      bs4TabItem(
        tabName = "suicidio",
        fluidRow(
          column(
            width = 12,
            box(
              title = "Definición",
              width = 12,
              collapsible = FALSE,
              uiOutput("definicion_suicidio_ui")
            )
          )
        ),
        br(),
        fluidRow(
          bs4Card(
            title = "Filtros de análisis",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            collapsible = FALSE,
            #Primera fila
            fluidRow(
              column(8, uiOutput("indicador_suicidio_ui")),
              column(4, uiOutput("muni_suicidio_ui"))
            ),
            # Segunda fila
            fluidRow(
              column(2, uiOutput("year_min_suicidio_ui")),
              column(2, uiOutput("year_max_suicidio_ui")),
              column(2, uiOutput("tipo_suicidio_ui")),
              column(3, uiOutput("categoria_suicidio_ui")),
              column(3,
                div(
                  style = "text-align: right; padding-top: 25px;",
                  actionButton("reset_filters_suicidio", "Limpiar filtros",
                               class = "btn btn-secondary")
                )
              )
            )
          )
        ),
        # ================== GRÁFICO LINEAS ===================
        fluidRow(
          bs4Card(
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            #title = "Serie temporal",
            collapsible = FALSE,
            br(),
            plotlyOutput("grafico_suicidio", height = "400px")
            %>% withSpinner(color = "black")
          )
        ),
        br(),
      ),
      # --- PESTAÑA 3: DEMENCIA ---
      bs4TabItem(
        tabName = "demencia",
        fluidRow(
          column(
            width = 12,
            box(
              title = "Definición",
              width = 12,
              collapsible = FALSE,
              uiOutput("definicion_demencia_ui")
            )
          )
        ),
        br(),
        fluidRow(
          bs4Card(
            title = "Filtros de análisis",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            collapsible = FALSE,
            #Primera fila
            fluidRow(
              column(8, uiOutput("indicador_demencia_ui")),
              column(4, uiOutput("muni_demencia_ui"))
            ),
            # Segunda fila
            fluidRow(
              column(2, selectInput("year_min_demencia", "Año desde",
                                    choices = anios, selected = min(anios))),
              column(2, selectInput("year_max_demencia", "Año hasta",
                                    choices = anios, selected = max(anios))),
              column(2, uiOutput("tipo_demencia_ui")),
              column(3, uiOutput("categoria_demencia_ui")),
              column(3,
                div(
                  style = "text-align: right; padding-top: 25px;",
                  actionButton("reset_filters_demencia", "Limpiar filtros",
                               class = "btn btn-secondary")
                )
              )
            )
          )
        ),
        # ================== GRÁFICO LINEAS ===================
        fluidRow(
          bs4Card(
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            #title = "Serie temporal",
            collapsible = FALSE,
            br(),
            plotlyOutput("grafico_demencia", height = "400px")
            %>% withSpinner(color = "black")
          )
        ),
        br(),
      ),
      # --- PESTAÑA 4: SERVICIOS ---
      bs4TabItem(
        tabName = "servicios",
        
        # PRINCIPAl
        fluidRow(
          bs4Card(
            title = "Definición",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            collapsible = FALSE,
            p("Métrica que representa la frecuencia con la que se utiliza un recurso, producto o servicio en particular durante un período específico. 
         Se calcula dividiendo el número de usuarios activos por el total de usuarios potenciales, multiplicando el resultado por 100 para obtener un porcentaje.")
          )
        ),
        
        # FILTROS 
        fluidRow(
          bs4Card(
            title = "Filtros de análisis",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            collapsible = FALSE,
            
            fluidRow(
              column(8, uiOutput("indicador_servicios_ui")),
              column(4, uiOutput("muni_servicios_ui"))
            ),
            
            fluidRow(
              column(2, uiOutput("year_min_servicios_ui")),
              column(2, uiOutput("year_max_servicios_ui")),
              column(2, uiOutput("tipo_servicios_ui")),     
              column(3, uiOutput("categoria_servicios_ui")),  
              #column(3, uiOutput("tipo_grafico_servicios_ui")),
              column(
                3,
                div(
                  style = "text-align: right; padding-top: 25px;",
                  actionButton(
                    "reset_filters_servicios",
                    "Limpiar filtros",
                    class = "btn btn-secondary"
                  )
                )
              )
            )
          )
        ),
        
        # GRÁFICO PRINCIPAL
        fluidRow(
          bs4Card(
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            collapsible = FALSE,
            br(),
            plotlyOutput("grafico_servicios", height = "400px") %>%
              withSpinner(color = "black")
          )
        )
      )
    )  ,
    # BOTON SCROLL
    tags$div(
      id = "scrollTopBtn",
      icon("arrow-up", lib = "font-awesome"),
      class = "scroll-top-button"
    ),
  ), #FINAL BODY
  footer = bs4DashFooter(
    left = tagList(
      div(
        style = "padding: 0px; border: none; color: black",
        # Caja con tres columnas ---
        fluidRow(
          column(
            width = 4,
            bs4Card(
              title = "Fuente de datos",
              width = 12,
              collapsible = FALSE,
              solidHeader = TRUE,
              style = "height: 100px;",
              "Sistema Integrado de Información de la Protección Social SISPRO. 
              Datos agregados desde 2005 hasta 2024."
            )
          ),
          column(
            width = 4,
            bs4Card(
              title = "Cobertura",
              width = 12,
              collapsible = FALSE,
              solidHeader = TRUE,
              style = "height: 100px;",
              "23 municipios del Oriente Antioqueño"
            )
          ),
          column(
            width = 4,
            bs4Card(
              title = "Última actualización",
              width = 12,
              collapsible = FALSE,
              solidHeader = TRUE,
              style = "height: 100px;",
              "Los datos mostrados corresponden a la información más reciente 
              disponible hasta diciembre de 2024."
            )
          )
        )
      ),
      br(),
      # --- Texto de créditos ---
      div(
        style = "width: 100%; text-align: center;",
        HTML("
          Tablero interactivo desarrollado como trabajo de grado por 
          <strong>Ps. Valentina Jaramillo Marín</strong>, 
          <strong>Ps. Jenifer Alejandra Martínez Mendoza</strong> y 
          <strong>Ps. Víctor Manuel Restrepo López</strong>, 
          bajo la asesoría de Ps., Mg., Juan Pablo Sánchez Escudero, Ph. D.
          <br>2026
        ")
      )
    )
  ),
  # --- CONTROLBAR ---
  controlbar = NULL,
  title = "Dashboard Salud Mental"
)
