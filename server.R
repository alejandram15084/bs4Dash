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




  # 3️⃣ FILA DE KPI BOXES
  output$kpi_boxes <- renderUI({
    req(input$tab_activa)
    # Aquí defines los valores según la pestaña activa
    if (input$tab_activa == "Infraestructura") {
      kpi1 <- "45%"
      kpi2 <- "3.2%"
      kpi3 <- "12.350"
      kpi4 <- "1.5%"
    } else if (input$tab_activa == "Años perdidos") {
      kpi1 <- "120"
      kpi2 <- "2.8%"
      kpi3 <- "9.500"
      kpi4 <- "0.5%"
    } else {
      kpi1 <- "-"
      kpi2 <- "-"
      kpi3 <- "-"
      kpi4 <- "-"
    }
    # Crear las cajas dinámicamente
    tagList(
      bs4ValueBox(value = kpi1, subtitle = "Indicador 1", 
                  icon = icon("heartbeat"), width = 3),
      bs4ValueBox(value = kpi2, subtitle = "Indicador 2",
                  icon = icon("skull"), width = 3),
      bs4ValueBox(value = kpi3, subtitle = "Indicador 3",
                  icon = icon("hospital"), width = 3),
      bs4ValueBox(value = kpi4, subtitle = "Indicador 4",
                  icon = icon("brain"), width = 3)
    )
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

    df <- datos_total %>%
      filter(
        Indicador1 == input$indicador,
        Municipio  %in% input$muni,
        Ano        >= input$year_min,
        Ano        <= input$year_max
      )

    # Si se selecciona categoría (Sexo o Edad), aplicar filtro adicional
    if (!is.null(input$categoria) && input$categoria != "Total") {
      df <- df %>% filter(Categoria == input$categoria)
    }

    df
  })

  # -------------------------------------------------------------------
  # 6️⃣ GRÁFICO DE LÍNEAS PLOTLY
  # -------------------------------------------------------------------
  output$linePlot <- renderPlotly({
    df <- data_filtrada()
    req(nrow(df) > 0)  # Asegura que haya datos

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
        title  = paste("Indicador:", input$indicador),
        xaxis  = list(title = "Año"),
        yaxis  = list(title = "Valor"),
        hovermode = "x unified"
      )
  })

  # -------------------------------------------------------------------
  # 7️⃣ BOTÓN PARA LIMPIAR FILTROS
  # -------------------------------------------------------------------
  observeEvent(input$reset_filters, {
    updateSelectInput(session, "year_min", selected = min(years))
    updateSelectInput(session, "year_max", selected = max(years))
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
