server <- function(input, output, session) {

  # 1️⃣ DEFINICIÓN DINÁMICA SEGÚN PESTAÑA ACTIVA
  output$definicion_ui <- renderUI({
    definicion <- switch(input$tab_activa,
    "Infraestructura" = "Disponibilidad de servicios y recursos físicos para la atención en salud mental, representada por el número de IPS habilitadas y las camas destinadas a estos servicios.",
    "Años perdidos" = "Años que una persona deja de vivir cuando muere antes de la edad esperada, expresados por cada 100.000 habitantes.",
    "Atención" = "Porcentaje de personas atendidas en los servicios de salud por una causa específica frente al total de atenciones registradas.",
    "Hospitalización" = "Proporción de personas que requieren ingreso hospitalario por una causa específica frente al total de hospitalizaciones.",
    "Mortalidad" = "Número de muertes por cada 100.000 habitantes, ajustado según la estructura de edad de una población estándar.",
    "Letalidad" = "Porcentaje de personas que fallecen entre quienes presentan un diagnóstico o evento específico.",
    )

    HTML(definicion)
  })



 # -- KPIS ---
  output$kpi_boxes <- renderUI({
    if (input$tab_activa == "Infraestructura") {
      fluidRow(
        bs4InfoBox(title = "Total de IPS con servicios de salud mental", 
                   value = "48",
                   icon = icon("hospital"),
                   color = "secondary",
                   gradient = TRUE,
                   fill = TRUE,
                   width = 3),
        bs4InfoBox(title = "Total de camas especializadas en salud",
                   value = "50",
                   icon = icon("bed"),
                   color = "secondary",
                   gradient = TRUE,
                   fill = TRUE,
                   width = 3),
        bs4InfoBox(title = "Municipio con mayor disponibilidad de servicios",
                   value = "Rionegro",
                   icon = icon("map-marker-alt"),
                   color = "secondary",
                   gradient = TRUE,
                   fill = TRUE,
                   width = 3),
        bs4InfoBox(title = "% de municipios con mas de dos servicios",
                   value = "1.5%",
                   icon = icon("network-wired"),
                   color = "secondary",
                   gradient = TRUE,
                   fill = TRUE,
                   width = 3)
      )
    } else if (input$tab_activa == "Años perdidos") {
      fluidRow(
        bs4InfoBox(title = "Total AVPP",
                   value = "",
                   icon = icon("chart-line"),
                   color = "secondary",
                   gradient = TRUE,
                   fill = TRUE,
                   width = 3),
        bs4InfoBox(title = "Trastorno con mayor AVPP",
                   value = "",
                   icon = icon("exclamation-triangle"),
                   color = "secondary",
                   gradient = TRUE,
                   fill = TRUE,
                   width = 3),
        bs4InfoBox(title = "Municipio con mayor AVPP",
                   value = "",
                   icon = icon("map-marker-alt"),
                   color = "secondary",
                   gradient = TRUE,
                   fill = TRUE,
                   width = 3),
        bs4InfoBox(title = "Municipio con menor AVPP",
                   value = "",
                   icon = icon("map-marker"),
                   color = "secondary",
                   gradient = TRUE,
                   fill = TRUE,
                   width = 3),
      )
    } else if (input$tab_activa == "Atención") {
      fluidRow(
        bs4InfoBox(title = "% total de población atendida",
                   value = "",
                   icon = icon("users"),
                   color = "secondary",
                   gradient = TRUE,
                   fill = TRUE,
                   width = 3),
        bs4InfoBox(title = "Trastorno con mayor atención",
                   value = "",
                   icon = icon("stethoscope"),
                   color = "secondary",
                   gradient = TRUE,
                   fill = TRUE,
                   width = 3),
        bs4InfoBox(title = "Municipio con mayor cobertura",
                   value = "",
                   icon = icon("map-marker-alt"),
                   color = "secondary",
                   gradient = TRUE,
                   fill = TRUE,
                   width = 3),
        bs4InfoBox(title = "Municipio con menor cobertura",
                   value = "",
                   icon = icon("map-marker"),
                   color = "secondary",
                   gradient = TRUE,
                   fill = TRUE,
                   width = 3),
      )
    } else if (input$tab_activa == "Hospitalización") {
      fluidRow(
        bs4InfoBox(title = "% total de hospitalizaciones",
                   value = "",
                   icon = icon("hospital-user"),
                   color = "secondary",
                   gradient = TRUE,
                   fill = TRUE,
                   width = 3),
        bs4InfoBox(title = "Trastorno con mayor hospitalización",
                   value = "",
                   icon = icon("procedures"),
                   color = "secondary",
                   gradient = TRUE,
                   fill = TRUE,
                   width = 3),
        bs4InfoBox(title = "Municipio con mayor % hospitalización",
                   value = "",
                   icon = icon("map-marker-alt"),
                   color = "secondary",
                   gradient = TRUE,
                   fill = TRUE,
                   width = 3),
        bs4InfoBox(title = "Municipio con menor % hospitalización",
                   value = "",
                   icon = icon("map-marker"),
                   color = "secondary",
                   gradient = TRUE,
                   fill = TRUE,
                   width = 3),
      )
    } else if (input$tab_activa == "Mortalidad") {
      fluidRow(
        bs4InfoBox(title = "Mortalidad por epilepsia",
                   value = "",
                   icon = icon("brain"),
                   color = "secondary",
                   gradient = TRUE,
                   fill = TRUE,
                   width = 3),
        bs4InfoBox(title = "Mortalidad por suicidio",
                   value = "",
                   icon = icon("skull-crossbones"),
                   color = "secondary",
                   gradient = TRUE,
                   fill = TRUE,
                   width = 3),
        bs4InfoBox(title = "Mortalidad por trastornos mentales",
                   value = "",
                   icon = icon("hosphead-side-brainital"),
                   color = "secondary",
                   gradient = TRUE,
                   fill = TRUE,
                   width = 3),
        bs4InfoBox(title = "Municipio con mayor mortalidad",
                   value = "",
                   icon = icon("map-marker-alt"),
                   color = "secondary",
                   gradient = TRUE,
                   fill = TRUE,
                   width = 3),
      )
    } else if (input$tab_activa == "Letalidad") {
      fluidRow(
        bs4InfoBox(title = "Letalidad por intoxicaciones",
                   value = "",
                   icon = icon("biohazard"),
                   color = "secondary",
                   gradient = TRUE,
                   fill = TRUE,
                   width = 3),
        bs4InfoBox(title = "Letalidad por lesiones autoinfligidas",
                   value = "",
                   icon = icon("skull"),
                   color = "secondary",
                   gradient = TRUE,
                   fill = TRUE,
                   width = 3),
        bs4InfoBox(title = "Municipio con mayor letalidad",
                   value = "",
                   icon = icon("map-marker-alt"),
                   color = "secondary",
                   gradient = TRUE,
                   fill = TRUE,
                   width = 3),
        bs4InfoBox(title = "Municipio con menor letalidad",
                   value = "",
                   icon = icon("map-marker"),
                   color = "secondary",
                   gradient = TRUE,
                   fill = TRUE,
                   width = 3),
      )
    }
  })





  # --- Filtro años---
  output$year_min_ui <- renderUI({
    selectInput("year_min", "Año desde",
                choices = sort(unique(datos_total$Ano)),
                selected = min(datos_total$Ano))
  })

  output$year_max_ui <- renderUI({
    selectInput("year_max", "Año hasta",
                choices = sort(unique(datos_total$Ano)),
                selected = max(datos_total$Ano))
  })

  # --- Filtro tipo ----
output$tipo_ui <- renderUI({
  req(input$tab_activa)  # Asegura que la pestaña esté definida

  if(input$tab_activa == "Infraestructura") {
    selectInput("tipo", "Tipo",
                choices = "Geografia",
                selected = "Geografia")
  } else {
    selectInput("tipo", "Tipo",
                choices = c("Geografia", "Sexo", "Edad"),
                selected = "Geografia")
  }
})


  # --- Filtro categorías Sexo y Edad---
output$categoria_ui <- renderUI({
  req(input$tipo)
  if (input$tipo == "Sexo") {
    selectInput("categoria", "Sexo",
                choices = sort(unique(datos_total$Categoria[datos_total$Tipo=="Sexo"])),
                selected = "Total")
  } else if (input$tipo == "Edad") {
    selectInput("categoria", "Grupo etario",
                choices = sort(unique(datos_total$Categoria[datos_total$Tipo=="Edad"])),
                selected = "Total")
  } else {
    return(NULL)
  }
})

# 3️⃣ INPUT DEL INDICADOR SEGÚN LA PESTAÑA ACTIVA
output$indicador_ui <- renderUI({
  req(input$tab_activa)
  selectInput(
    inputId = "indicador",
    label = "Indicador",
    choices = indicadores_categoria[[input$tab_activa]],
    selected = indicadores_categoria[[input$tab_activa]][1]
  )
})


# 4️⃣ ACTUALIZACIÓN DE MUNICIPIOS SEGÚN INDICADOR

  output$muni_ui <- renderUI({
    req(input$indicador)

    municipios_disp <- datos_total %>%
      filter(Indicador1 == input$indicador) %>%
      pull(Municipio) %>%
      unique() %>%
      sort()

    selectInput(
      inputId = "muni",
      label = "Municipios",
      choices = municipios_disp,
      selected = municipios_disp,
      multiple = TRUE
    )
  })



  # -------------------------------------------------------------------
  # 5️⃣ REACTIVO GENERAL DE FILTRADO DE DATOS
  # -------------------------------------------------------------------
  data_filtrada <- reactive({
    req(input$indicador, input$muni, input$year_min, input$year_max)

    df <- datos_total[
    Indicador1 == input$indicador &
    Municipio  %in% input$muni &
    Ano        >= input$year_min &
    Ano        <= input$year_max]

    # Si se selecciona categoría (Sexo o Edad), aplicar filtro adicional
    if (!is.null(input$categoria) && input$categoria != "Total") {
    df <- df[Categoria == input$categoria]
  }

    df
  })

  # -------------------------------------------------------------------
  # 6️⃣ GRÁFICO DE LÍNEAS PLOTLY
  # -------------------------------------------------------------------
  output$linePlot <- renderPlotly({
    df <- data_filtrada()
    validate(
      need(nrow(df) > 0, "No hay datos disponibles para los filtros seleccionados")
    )  # Asegura que haya datos

    plot_ly(
      data = df,
      x = ~Ano,
      y = ~Valor1,
      color = ~Municipio,
      type  = "scatter",
      mode  = "lines+markers",
      hoverinfo = "text",
      text = ~paste(
        "Municipio:", Municipio,
        "<br>Año:", Ano,
        "<br>Valor:", Valor1
      )
    ) %>%
      layout(
        #title  = paste("Indicador:", input$indicador),
        xaxis  = list(title = "Año"),
        yaxis  = list(title = "Valor"),
        hovermode = "x unified"
      )
  })

  # -------------------------------------------------------------------
  # 7️⃣ BOTÓN PARA LIMPIAR FILTROS
  # -------------------------------------------------------------------
  observeEvent(input$reset_filters, {
    updateSelectInput(session, "year_min", selected = min(anios))
    updateSelectInput(session, "year_max", selected = max(anios))
    updateSelectInput(session, "tipo", selected = "Geografia")

    # Actualizar municipios según indicador actual
    req(input$indicador)
    municipios_disp <- datos_total %>%
      filter(Indicador1 == input$indicador) %>%
      pull(Municipio) %>%
      unique() %>%
      sort()

    updateSelectInput(session, "muni",
                      choices = municipios_disp,
                      selected = municipios_disp)
  })









}