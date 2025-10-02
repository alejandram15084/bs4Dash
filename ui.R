library(shiny)
library(bs4Dash)

ui <- bs4DashPage(

  # --- HEADER ---
  header = dashboardHeader(
    title = dashboardBrand(
      title = "Dashboard",
      color = "gray-dark",
      href = "https://adminlte.io/themes/v3",
      image = "https://adminlte.io/themes/v3/dist/img/AdminLTELogo.png"
    )
  ),

  # --- SIDEBAR ---
  sidebar = bs4DashSidebar(
    bs4SidebarMenu(
      bs4SidebarMenuItem(
        "Indicadores de salud mental",
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
      # --- TAB PRINCIPAL ---
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
          bs4Card(
            title = "Definición",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            collapsible = FALSE,
            uiOutput("definicion_ui")
          )
        ),

        br(),

        # 3️⃣ FILA DE KPI BOXES
        fluidRow(
          bs4ValueBox(
            value = "45%",
            subtitle = "Total de camas especialidas en salud mental",
            icon = icon("heartbeat"),
            width = 3
          ),
          bs4ValueBox(
            value = "3.2%",
            subtitle = "Total de IPS con servicios de salud mental",
            icon = icon("skull"),
            width = 3
          ),
          bs4ValueBox(
            value = "12.350",
            subtitle = "Municipio con mayor disponibilidad de servicios",
            icon = icon("hospital"),
            width = 3
          ),
          bs4ValueBox(
            value = "1.5%",
            subtitle = "Demencia",
            icon = icon("brain"),
            width = 3
          )
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
                                    choices = years, selected = min(years))),
              column(2, selectInput("year_max", "Año hasta",
                                    choices = years, selected = max(years))),

              column(2, selectInput("tipo", "Tipo",
                                    choices = c("Geografia", "Sexo", "Edad"),
                                    selected = "Geografia")),

              column(3, uiOutput("categoria_ui")),
              column(3, selectInput("muni", "Municipios",
                                    choices = c(unique(datos_total$Municipio)),
                                    multiple = TRUE))
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
            title = "Serie temporal",
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
        ),

      )
    ),
    # ✅ Caja con tres columnas
    fluidRow(
      column(
        width = 4,
        bs4Card(
          title = "Fuente de datos",
          width = 12,
          collapsible = FALSE,
          status = "secondary",
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
          status = "secondary",
          solidHeader = TRUE,
          style = "height: 100px;",
          "23 municipios del Oriente Antioqueño
          ", br()
        )
      ),
      column(
        width = 4,
        bs4Card(
          title = "Última actualización",
          width = 12,
          collapsible = FALSE,
          status = "secondary",
          solidHeader = TRUE,
          style = "height: 100px;",
          "Los datos mostrados corresponden a la información más reciente 
          disponible hasta diciembre de 2024."
        )
      )
    ),
    br()
  ), #FINAL BODY

  footer = bs4DashFooter(
    left = HTML('
    <div style="width: 100%; text-align: center;">
      Tablero interactivo desarrollado como trabajo de grado por 
      <strong>Valentina Jaramillo Marín</strong>, 
      <strong>Jenifer Alejandra Martínez Mendoza</strong> y 
      <strong>Víctor Manuel Restrepo López</strong>, 
      bajo la asesoría de Juan Pablo Sánchez Escudero.
      <br>2026
    </div>
  ')
  ),

  # --- CONTROLBAR ---
  controlbar = bs4DashControlbar(),

  title = "Dashboard Salud Mental"

)