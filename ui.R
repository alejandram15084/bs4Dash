library(shiny)
library(bs4Dash)

ui <- bs4DashPage(

  # --- HEADER ---
  header = dashboardHeader(
    title = dashboardBrand(
      title = "Dashboard",
      color = "gray-dark",
      href = "https://www.udea.edu.co/wps/portal/udea/web/inicio",
      image = "logo.png"
    )
  ),

  # --- SIDEBAR ---
  sidebar = bs4DashSidebar(
    bs4SidebarMenu(
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
    # Enlace a CSS personalizado
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),

    bs4TabItems(
      # --- TAB 1: INDICADORES ---
      bs4TabItem(
        tabName = "indicadores",

        # 1️⃣ PANEL DE TABS
        fluidRow(
          column(
            width = 12,
            tabsetPanel(
              id = "tab_activa",
              type = "pills",
              selected = "Infraestructura",
              tabPanel("Infraestructura"),
              tabPanel("Años perdidos"),
              tabPanel("Atención"),
              tabPanel("Hospitalización"),
              tabPanel("Mortalidad"),
              tabPanel("Letalidad")
            )
          )
        ),

        br(),
        # 2️⃣ CAJA DE DEFINICIONES
        fluidRow(
          uiOutput("kpi_boxes")
        ),

        br(),

        # 4️⃣ FILTRO GLOBAL
        fluidRow(
          bs4Card(
            title = "Filtros de análisis",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            collapsible = FALSE,

            fluidRow(
              column(2, selectInput("year_min", "Año desde",
                                    choices = anios, selected = min(anios))),
              column(2, selectInput("year_max", "Año hasta",
                                    choices = anios, selected = max(anios))),

              column(2, uiOutput("tipo_ui")),

              column(3, uiOutput("categoria_ui")),
              column(3, pickerInput(
                inputId = "muni",
                label = "Municipios",
                choices = municipios,
                multiple = TRUE,
                selected = c("LaUnion", "ElCarmen", "Rionegro", "LaCeja", "ElRetiro"),
                options = list(
                  `actions-box` = TRUE,          # seleccionar deseleccionar todo
                  `live-search` = TRUE,          # buscar municipios
                  `selected-text-format` = "count > 3"  # muestra "3 seleccionados" si hay muchos
                  )
                )
              ),
            ),
            fluidRow(
              column(12, div(style = "text-align: right;",
                             actionButton("reset_filters", "Limpiar filtros")))
            )
          )
        ),
        # Gráfico líneas (Plotly)
        fluidRow(
          bs4Card(
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            #title = "Serie temporal",
            collapsible = FALSE,

            fluidRow(
              column(
                width = 8,
                uiOutput("indicador_ui")
              )
            ),
            br(),
            plotlyOutput("linePlot", height = "400px")
          )
        )
      ),
      # --- TAB 2: SUICIDIO ---
      bs4TabItem(
        tabName = "suicidio",
        fluidRow(
          h3("Tasa de intento de suicidio")
        )
      ),
      # --- TAB 3: DEMENCIA ---
      bs4TabItem(
        tabName = "demencia",
        fluidRow(
          h3("Indicador de demencia")
        )
      ),
      # --- TAB 4: SERVICIOS ---
      bs4TabItem(
        tabName = "servicios",
        fluidRow(
          h3("Uso de servicios de salud mental"),
          p("HALLO LEUTE, WIE GEHTS DIR")
        )
      )
    ),
    br()
  ), #FINAL BODY






  footer = bs4DashFooter(
    left = tagList(
      div(
        style = "padding: 0px; border: none; color: black",
        # ✅ Caja con tres columnas ---
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
          <strong>Valentina Jaramillo Marín</strong>, 
          <strong>Jenifer Alejandra Martínez Mendoza</strong> y 
          <strong>Víctor Manuel Restrepo López</strong>, 
          bajo la asesoría de Juan Pablo Sánchez Escudero.
          <br>2026
        ")
      )
    )
  ),






  # --- CONTROLBAR ---
  controlbar = bs4DashControlbar(),

  title = "Dashboard Salud Mental"

)
