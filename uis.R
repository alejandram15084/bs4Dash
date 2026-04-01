# -------------------------------------------------------------------
#  GRÁFICO DE BARRAS POR SEXO 
# -------------------------------------------------------------------


# Indicador

output$indicador_sexo_ui <- renderUI({
  req(input$tab_activa)
  
  indicadores_tab <- indicadores_categoria[[input$tab_activa]]
  
  if (is.null(indicadores_tab)) {
    indicadores_tab <- sort(unique(datos_total$Indicador1))
  }
  
  indicadores_validos_sexo <- datos_total[
    Indicador1 %in% indicadores_tab &
    !is.na(Valor1) &
    as.numeric(Valor1) > 0,
    sort(unique(Indicador1))
  ]
  
  selectInput(
    inputId = "indicador_sexo",
    label = "Indicador",
    choices = indicadores_validos_sexo,
    selected = indicadores_validos_sexo[1]
  )
})



# Municipio 

output$municipio_sexo_ui <- renderUI({
  req(input$tab_activa, input$indicador_sexo, input$anio_sexo)

  municipios_validos <- datos_total[
    Indicador1 == input$indicador_sexo &
    Ano == input$anio_sexo &
    Tipo == "Sexo" &
    !is.na(Valor1) &
    as.numeric(Valor1) > 0,
    sort(unique(Municipio))
  ]


  pickerInput(
    inputId = "municipio_sexo",
    label = "Municipio",
    choices = municipios_validos,
    selected = municipios_validos,
    multiple = TRUE,
    options = list(
      `actions-box` = TRUE,
      `live-search` = TRUE,
      `none-selected-text` = "Todos los municipios",
      `select-all-text` = "Seleccionar todos",
      `deselect-all-text` = "Quitar todos",
      `selected-text-format` = "count > 3"
    )
  )
})


# Gráfica

output$barPlotSexo <- renderPlotly({
  req(input$indicador_sexo, input$anio_sexo, input$municipio_sexo)
  
  df <- datos_total[
    Indicador1 == input$indicador_sexo &
    Ano == input$anio_sexo &
    Tipo == "Sexo" &
    Categoria != "Total" &
    Municipio %in% input$municipio_sexo
  ]
  
  validate(
    need(nrow(df) > 0,
         "No hay datos disponibles para la selección realizada")
  )
  
  # agregación
  df_agg <- df[
    !is.na(Valor1) & Valor1 > 0,
    .(Valor = mean(as.numeric(Valor1), na.rm = TRUE)),
    by = .(Municipio, Categoria)
  ]
  
  # asegurar que siempre aparezcan ambos sexos
  sexos_completos <- data.table(Categoria = c("Masculino", "Femenino"))
  
  df_agg <- merge(
    df_agg,
    sexos_completos,
    by = "Categoria",
    all = TRUE
  )
  
  df_agg[is.na(Valor), Valor := 0]
  
  # colores
  colores_sexo <- c(
    "Masculino" = "#344e41",
    "Femenino" = "#a3b18a"
  )
  
  plot_ly(
    data = df_agg,
    x = ~Municipio,
    y = ~Valor,
    color = ~Categoria,
    colors = colores_sexo,
    type = "bar",
    hovertemplate = "Municipio: %{x}<br>Valor: %{y}<extra></extra>"
  ) %>%
    layout(
      barmode = "group",
      xaxis = list(title = "Municipio", tickangle = -45),
      yaxis = list(title = "Valor"),
      legend = list(title = list(text = "<b>Sexo</b>")),
      plot_bgcolor = "white",
      paper_bgcolor = "white"
    )
})
