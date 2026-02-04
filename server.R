server <- function(input, output, session) {

  # -------------------------------------------------------------------
  # DEFINICIONES DE TEXTO
  # -------------------------------------------------------------------
  output$definicion_ui <- renderUI({
    req(input$tab_activa) 

    definicion <- switch(input$tab_activa,
      "Infraestructura" = "Disponibilidad de servicios y recursos físicos para la atención en salud mental, representada por el número de IPS habilitadas y las camas destinadas a estos servicios.",
      "Años Perdidos"    = "Número de años que dejan de vivir las personas al momento de morir, expresados por cada 100.000 habitantes.",
      "Atención"         = "Porcentaje de personas atendidas en los servicios de salud por una causa específica frente al total de atenciones registradas.",
      "Hospitalización"  = "Proporción de personas que hospitalizadas en los servicios de salud por una causa específica frente al total de hospitalizaciones.",
      "Mortalidad"       = "Número de muertes por 100.000 personas que ocurrirían si la población tuviera la misma estructura de edad que la población estándar.",
      "Letalidad"        = "Porcentaje de personas que mueren por lesiones autoinflingidas intencionalmente entre las personas con diagnóstico de lesiones autoinflingidas intencionalmente."
    )
    HTML(definicion)
  })

    # Definición fija para pestaña "Tasa de intento de suicidio"
    output$definicion_suicidio_ui <- renderUI({
      HTML("Métrica que cuantifica la frecuencia de intentos de suicidio no fatales en una población determinada durante un período específico. Se calcula dividiendo el número de personas que han realizado al menos un intento de suicidio y han sobrevivido al evento entre la población total del área geográfica correspondiente, y multiplicando el resultado por 100.000 habitantes para obtener la tasa.")
    })
    
    # Definición fija para pestaña "Indicador de demencia"
    output$definicion_demencia_ui <- renderUI({
      HTML("Métrica que expresa la proporción de personas que han recibido atención en los servicios de salud por diagnósticos asociados a demencia durante un período determinado, en relación con el total de personas atendidas en los servicios de salud por cualquier causa. Se calcula dividiendo el número de personas atendidas por demencia entre el total de personas atendidas, y multiplicando el resultado por 100 para obtener un porcentaje.")
    })
    

  # -------------------------------------------------------------------
  # KPIs
  # -------------------------------------------------------------------
  # Función para obtener max/min
  kpi_max_min <- function(df, columna, tipo = "max") {
    if (nrow(df) == 0) return(NA)
    # Agrupar y sumar
    agg_df <- df[, .(valor = sum(Valor1, na.rm = TRUE)), by = c(columna)]
    # Ordenar
    if (tipo == "max") {
      setorder(agg_df, -valor)
    } else {
      setorder(agg_df, valor)
    }
    # Devolver el primer valor
    return(agg_df[[columna]][1])
  }
  # Función para calcular KPIs según pestaña
  calcular_kpis <- function(df, tab_activa) {
    req(nrow(df) > 0)
    ultimo_anio <- max(df$Ano, na.rm = TRUE)
    df_ultimo_anio <- df[Ano == ultimo_anio]
    req(nrow(df_ultimo_anio) > 0)
    total <- round(sum(df_ultimo_anio$Valor1, na.rm = TRUE), 0)
    promedio <- round(mean(df_ultimo_anio$Valor1, na.rm = TRUE), 2)
    mayor_mpio <- kpi_max_min(df_ultimo_anio, "Municipio", "max")
    menor_mpio <- kpi_max_min(df_ultimo_anio, "Municipio", "min")
    list(
      total = total,
      promedio = promedio,
      mayor_mpio = mayor_mpio,
      menor_mpio = menor_mpio
    )

  }
  # Reactive que calcula KPIs según la pestaña activa y filtros
  kpi_cache <- reactive({
    req(input$tab_activa)
    df <- data_filtrada()
    calcular_kpis(df, input$tab_activa)
  })
  # Renderizado de los KPI boxes
  output$kpi_boxes <- renderUI({
    req(kpi_cache())
    kpis <- kpi_cache()
    titulos <- list(
      "Infraestructura" = c("Total IPS", "Promedio IPS",
                            "Municipio mayor disponibilidad",
                            "Municipio menor disponibilidad"),
      "Años Perdidos"   = c("Total AVPP", "Promedio AVPP",
                            "Municipio mayor AVPP", "Municipio menor AVPP"),
      "Atención"        = c("% población atendida", "Promedio % atendida",
                            "Municipio mayor cobertura",
                            "Municipio menor cobertura"),
      "Hospitalización" = c("% hospitalizaciones", "Promedio % hospitalización",
                            "Municipio mayor % hospitalización",
                            "Municipio menor % hospitalización"),
      "Mortalidad"      = c("Total mortalidad", "Promedio mortalidad",
                            "Municipio mayor mortalidad",
                            "Municipio menor mortalidad"),
      "Letalidad"       = c("Total letalidad", "Promedio letalidad",
                            "Municipio mayor letalidad",
                            "Municipio menor letalidad")
    )
    fluidRow(
      bs4InfoBox(title = titulos[[input$tab_activa]][1], 
                 value = format(kpis$total, big.mark = ".",
                                decimal.mark = ","),
                 icon = icon("chart-line"), color = "secondary",
                 gradient = TRUE, fill = TRUE, width = 3),
      bs4InfoBox(title = titulos[[input$tab_activa]][2],
                 value = format(kpis$promedio, big.mark = ".",
                                decimal.mark = ","),
                 icon = icon("calculator"),
                 color = "secondary", gradient = TRUE, fill = TRUE, width = 3),
      bs4InfoBox(title = titulos[[input$tab_activa]][3],
                 value = kpis$mayor_mpio, icon = icon("map-marker-alt"),
                 color = "secondary", gradient = TRUE, fill = TRUE, width = 3),
      bs4InfoBox(title = titulos[[input$tab_activa]][4],
                 value = kpis$menor_mpio, icon = icon("map-marker"),
                 color = "secondary", gradient = TRUE, fill = TRUE, width = 3)
    )
  })
  # -------------------------------------------------------------------
  # FILTROS DE AÑOS (UI dinámico)
  # -------------------------------------------------------------------

  observeEvent(input$year_min, ignoreInit = TRUE, {
    req(input$year_min)
    valid_max <- anios[anios >= input$year_min]
    sel <- if (!is.null(input$year_max) && input$year_max %in% valid_max)
      input$year_max else max(valid_max)
    updateSelectInput(session, "year_max", choices = valid_max, selected = sel)
  })
  observeEvent(input$year_max, ignoreInit = TRUE, {
    req(input$year_max)
    valid_min <- anios[anios <= input$year_max]
    sel <- if (!is.null(input$year_min) && input$year_min %in% valid_min)
      input$year_min else min(valid_min)
    updateSelectInput(session, "year_min", choices = valid_min, selected = sel)
  })
  # -------------------------------------------------------------------
  # TIPO / CATEGORIA 
  # -------------------------------------------------------------------
  output$tipo_ui <- renderUI({
    req(input$tab_activa)
    if (input$tab_activa == "Infraestructura") {
      selectInput("tipo", "Tipo", choices = "Geografia", selected = "Geografia")
    } else {
      selectInput("tipo", "Tipo",
                  choices = c("Geografia", "Sexo", "Edad"),
                  selected = "Geografia")
    }
  })
  output$categoria_ui <- renderUI({
    req(input$tipo)
    if (input$tipo == "Sexo") {
      opciones <- datos_total[Tipo == "Sexo", sort(unique(Categoria))]
      selectInput("categoria", "Sexo",
                  choices = opciones,
                  selected = "Total")
    } else if (input$tipo == "Edad") {
      opciones <- datos_total[Tipo == "Edad", sort(unique(Categoria))]
      selectInput("categoria", "Grupo etario",
                  choices = opciones,
                  selected = "Total")
    } else {
      return(NULL)
    }
  })
  # -------------------------------------------------------------------
  # SELECCIÓN DE INDICADOR (según pestaña)
  # -------------------------------------------------------------------
  output$indicador_ui <- renderUI({
    req(input$tab_activa)
    opciones <- indicadores_categoria[[input$tab_activa]]
    if (is.null(opciones) || length(opciones) == 0) opciones <- indicadores
    selectInput(inputId = "indicador",
                label = "Indicador", choices = opciones, selected = opciones[1])
  })
  # -------------------------------------------------------------------
  # MUNICIPIOS DINAMICO
  # -------------------------------------------------------------------
  output$muni_ui <- renderUI({
    req(input$indicador)
    municipios_disp <- municipios_disponibles()
    defaults <- c("La Union", "El Carmen", "Rionegro", "La Ceja", "El Retiro")
    selected_defaults <- intersect(defaults, municipios_disp)
    if (length(selected_defaults) == 0) {
      selected_defaults <- head(municipios_disp, 5)
    }
    pickerInput(
      inputId = "muni",
      label = "Municipios",
      choices = municipios_disp,
      selected = selected_defaults,
      multiple = TRUE,
      options = list(`actions-box` = TRUE,
                     `live-search` = TRUE,
                     `selected-text-format` = "count > 3",
                     selectAllText = "Seleccionar todo",
                     deselectAllText = "Deseleccionar todo",
                     countSelectedText = "{0} municipios seleccionados",
                     noneSelectedText = "Ningún municipio seleccionado"
    ))
  })
  # -------------------------------------------------------------------
  # FILTRADO DE DATOS (REACTIVO PRINCIPAL)
  # -------------------------------------------------------------------
  data_filtrada <- reactive({
    req(input$indicador, input$muni, input$year_min, 
        input$year_max, input$tipo)
    municipios_validos <- municipios_disponibles()
    req(all(input$muni %in% municipios_validos))
    # 1. Filtro principal
    df_step1 <- datos_total[
      Indicador1 == input$indicador &
      Municipio  %chin%  input$muni &
      Tipo       == input$tipo
    ]
    # 2. Filtro de Categoria
    df_step2 <- if (input$tipo != "Geografia") {
      req(input$categoria)
      if (input$categoria == "Total") {
        df_step1[Categoria == "Total"]
      } else {
        df_step1[Categoria == input$categoria]
      }
    } else {
      df_step1
    }
    # 3. Filtro de Rango (Ano)
      df_final <- df_step2[Ano >= input$year_min & Ano <= input$year_max]
      df_final
    })
  # -------------------------------------------------------------------
  # BOTÓN LIMPIAR
  # -------------------------------------------------------------------
  observeEvent(input$reset_filters, {
    updateSelectInput(session, "year_min", selected = min(anios))
    updateSelectInput(session, "year_max", selected = max(anios))
    updateSelectInput(session, "tipo", selected = "Geografia")
    req(input$indicador)
    municipios_disp <- datos_total[Indicador1 == input$indicador, sort(unique(Municipio))]
    updatePickerInput(session, "muni",
                      choices = municipios_disp,
                      selected = c("La Union",
                                   "El Carmen",
                                   "Rionegro",
                                   "La Ceja",
                                   "El Retiro"))
  })
  # -------------------------------------------------------------------
  # DISPARADOR INICIAL Y AL CAMBIAR DE PESTAÑA
  # -------------------------------------------------------------------
  observeEvent(input$tab_activa, {
    if (!is.null(indicadores_categoria[[input$tab_activa]])) {
      primeros <- indicadores_categoria[[input$tab_activa]][1]
      try(updateSelectInput(session, "indicador", selected = primeros), silent = TRUE)
    }
    updateSelectInput(session, "year_min", 
                      choices = anios, 
                      selected = min(anios))
    
    updateSelectInput(session, "year_max", 
                      choices = anios, 
                      selected = max(anios))
  })
  municipios_disponibles <- reactive({
    req(input$indicador)
    datos_total[Indicador1 == input$indicador, sort(unique(Municipio))]
  })
  # -------------------------------------------------------------------
  #  GRÁFICO DE LÍNEAS 
  # -------------------------------------------------------------------
  output$linePlot <- renderPlotly({
    df_filtrado <- data_filtrada()
    validate(need(nrow(df_filtrado) > 0, 
    "No hay datos disponibles para los filtros seleccionados"))
    df_agg <- df_filtrado[, .(Valor_Plot = mean(Valor1, na.rm = TRUE)), 
                          by = .(Ano, Municipio)]
    indicador_actual <- isolate(input$indicador)
    plot_ly(
      data = df_agg,
      x = ~Ano,
      y = ~Valor_Plot,
      color = ~Municipio,
      type  = "scatter",
      mode  = "lines+markers",
      hoverinfo = "text",
      text = ~paste("Municipio:", 
                    Municipio, "<br>Año:",
                    Ano, "<br>Valor:",
                    round(Valor_Plot, 2)),
      opacity = 1
    ) %>%
      layout(
        #title = paste(indicador_actual),
        xaxis = list(title = "Año", dtick =1),
        yaxis = list(title = "Valor"),
        hovermode = "x unified",
        plot_bgcolor = "white",
        paper_bgcolor = "white"
      )
  })
  # -------------------------------------------------------------------
  #  GRÁFICO DE BARRAS POR SEXO 
  # -------------------------------------------------------------------
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
      selected = indicadores_validos_sexo
    )
  })
  output$barPlotSexo <- renderPlotly({
    req(input$indicador_sexo, input$anio_sexo)
    df <- datos_total[
      Indicador1 == input$indicador_sexo &
      Ano == input$anio_sexo &
      Tipo == "Sexo" &
      Categoria != "Total"
    ]
    validate(
      need(nrow(df) > 0,
      "No hay datos disponibles para el indicador o año seleccionados")
    )
    df_agg <- df[
      !is.na(Valor1) & Valor1 > 0,
      .(Valor = mean(as.numeric(Valor1), na.rm = TRUE)),
      by = .(Municipio, Categoria)
    ]
    colores_sexo <- c("Masculino" = "#344e41", 
                      "Femenino" = "#a3b18a")
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
        #title = paste(input$indicador_sexo, "por sexo"),
        barmode = "group",
        xaxis = list(title = "Municipio", tickangle = -45),
        yaxis = list(title = "Valor"),
        legend = list(title = list(text = "<b>Sexo</b>")),
        plot_bgcolor = "white",
        paper_bgcolor = "white"
      )
  })
  # -------------------------------------------------------------------
  #  GRÁFICO POR GRUPO ETARIO
  # -------------------------------------------------------------------
  output$indicador_edad_ui <- renderUI({
    req(input$tab_activa)
    indicadores_tab <- indicadores_categoria[[input$tab_activa]]
    if (is.null(indicadores_tab)) {
      indicadores_tab <- sort(unique(datos_total$Indicador1))
    }
    indicadores_validos_edad <- datos_total[
      Indicador1 %in% indicadores_tab &
      Tipo == "Edad" &
      !is.na(Valor1) &
      as.numeric(Valor1) > 0,
      sort(unique(Indicador1))
    ]
    selectInput(
      inputId = "indicador_edad",
      label = "Indicador",
      choices = indicadores_validos_edad,
      selected = indicadores_validos_edad
    )
  })
  output$anio_edad_ui <- renderUI({
    req(input$indicador_edad)
    anios_validos <- datos_total[
      Indicador1 == input$indicador_edad &
      Tipo == "Edad" &
      !is.na(Valor1) &
      as.numeric(Valor1) > 0,
      sort(unique(Ano))
    ]
    selectInput(
      inputId = "anio_edad",
      label = "Año",
      choices = anios_validos,
      selected = if (length(anios_validos) > 0) max(anios_validos) else NULL
    )
  })
  output$municipio_edad_ui <- renderUI({
    req(input$indicador_edad, input$anio_edad)
    municipios_validos <- datos_total[
      Indicador1 == input$indicador_edad &
      Ano == input$anio_edad &
      Tipo == "Edad" &
      !is.na(Valor1) &
      as.numeric(Valor1) > 0,
      sort(unique(Municipio))
    ]
    selectInput(
      inputId = "municipio_edad",
      label = "Municipio",
      choices = municipios_validos,
      selected = ifelse("Rionegro" %in% municipios_validos, "Rionegro", municipios_validos[1])
    )
  })
  output$barPlotEdad <- renderPlotly({
    req(input$indicador_edad, input$anio_edad, input$municipio_edad)
    df <- datos_total[
      Indicador1 == input$indicador_edad &
      Ano == input$anio_edad &
      Tipo == "Edad" &
      Municipio == input$municipio_edad &
      Categoria != "Total" &
      !is.na(Valor1) & as.numeric(Valor1) > 0,
      .(Valor = mean(as.numeric(Valor1), na.rm = TRUE)),
      by = Categoria
    ]
    validate(need(nrow(df) > 0,
    "No hay datos disponibles para el municipio y año seleccionados"))
    plot_ly(
      data = df,
      x = ~Categoria,
      y = ~Valor,
      type = "bar",
      color = ~Categoria,
      colors = "Greens",
      hovertemplate = "Edad: %{x}<br>Valor: %{y}<extra></extra>"
    ) %>%
      layout(
        #title = paste(input$indicador_edad, "por grupo de edad"),
        xaxis = list(title = "Grupo de edad", tickangle = -45),
        yaxis = list(title = "Valor"),
        plot_bgcolor = "white",
        paper_bgcolor = "white"
      )
  })
  # ------------------------------------------------------------------- 
  # GRÁFICO DE BARRAS: PRINCIPALES CAUSAS AÑOS PERDIDOS 
  # ------------------------------------------------------------------- 
  # --- FUNCIÓN DE AYUDA PARA SALTO DE LÍNEA ---
  # Usa str_wrap para cortar texto y reemplaza el \n (R) por <br> (HTML)
  wrap_text <- function(text, width = 80) {
    wrapped <- stringr::str_wrap(text, width = width)
    return(gsub("\\n", "<br>", wrapped))
  }
  # --- Selector dinámico de año ---
  output$anio_seleccionado_ui <- renderUI({ 
    df <- datos_total [
        str_detect(Indicador1, regex("año|vida|perd", ignore_case = TRUE)) &
        Tipo == "Geografia" &
        Categoria == "Geografia" &
        !is.na(Valor1) & Valor1 > 0
    ]
    anios_disponibles <- sort(unique(df$Ano))
    pickerInput( 
      inputId = "anio_seleccionado",
      label = "Selecciona un año:",
      choices = anios_disponibles,
      selected = max(anios_disponibles),
      multiple = FALSE,
      width = "160px"
    )
  })
  # --- Gráfico de barras horizontal ---
  output$grafico_top_causas <- renderPlotly({ 
    req(input$tab_activa == "Años Perdidos") 
    df <- datos_total %>% 
      filter(str_detect(Indicador1, regex("año|vida|perd", ignore_case = TRUE))) 
    # Año seleccionado; si no existe, usar el último disponible
    selected_year <- input$anio_seleccionado %||% max(df$Ano, na.rm = TRUE)
    df_year <- df %>% 
      filter(Ano == selected_year)
    # Validación de datos disponibles
    validate( 
      need(nrow(df_year) > 0, paste("No hay datos para el año", selected_year))
    ) 
    # --- Agrupar y calcular el top 10 de causas ---
    top10_causas <- df_year %>% 
      filter(Tipo == "Geografia", Categoria == "Geografia") %>% 
      group_by(Indicador1) %>% 
      summarise(valor_total = mean(as.numeric(Valor1), na.rm = TRUE), .groups = "drop") %>% 
      filter(!is.na(valor_total) & valor_total > 0) %>% 
      arrange(desc(valor_total)) %>% 
      slice_head(n = 10) %>%
      mutate(Indicador_wrap = wrap_text(Indicador1, width = 80))
      validate(need(nrow(top10_causas) > 0, "No hay causas con valores disponibles")) 
    # --- Crear gráfico ---
    plot_ly( 
      data = top10_causas,
      x = ~valor_total,
      y = ~reorder(Indicador_wrap, valor_total), 
      type = "bar",
      orientation = "h",
      text = ~paste(round(valor_total, 2)),
      hoverinfo = "text",
      marker = list(color = "#588157")
    ) %>% 
      layout( 
        #title = paste("Principales causas de años perdidos en", selected_year), 
        xaxis = list(title = "Años perdidos"),
        yaxis = list(title = ""),
        plot_bgcolor = "white",
        paper_bgcolor = "white"
      )
  })
  # -------------------------------------------------------------------
  #  GRÁFICO DE BARRAS: LESIONES AUTOINFLIGIDAS INTENCIONALMENTE
  # -------------------------------------------------------------------
  output$anio_letalidad_ui <- renderUI({
    anios_disponibles <- datos_total[
      Indicador1 == "Letalidad por lesiones autoinflingidas intencionalmente" &
      Tipo == "Geografia",
      sort(unique(Ano))
    ]
    pickerInput(
      inputId = "anio_letalidad",
      label = "Selecciona un año:",
      choices = anios_disponibles,
      selected = if (length(anios_disponibles) > 0)
        max(anios_disponibles) else NULL,
      multiple = FALSE,
      width = "160px"
    )
  })
  output$grafico_lesiones_autoinfligidas <- renderPlotly({
    req(input$tab_activa == "Letalidad", input$anio_letalidad)
    selected_year <- input$anio_letalidad
    top_municipios <- datos_total[
      Indicador1 == "Letalidad por lesiones autoinflingidas intencionalmente" &
      Tipo == "Geografia" &
      Ano == selected_year &
      !is.na(Valor1) & as.numeric(Valor1) > 0,
      .(valor_total = mean(as.numeric(Valor1), na.rm = TRUE)),
      by = Municipio
    ][order(-valor_total)]
    validate(
      need(nrow(top_municipios) > 0,
           paste("No hay datos para el año", selected_year))
    )
    plot_ly(
      data = top_municipios,
      x = ~valor_total,
      y = ~reorder(Municipio, valor_total),
      type = "bar",
      orientation = "h",
      text = ~round(valor_total, 2),
      hoverinfo = "text",
      marker = list(color = "#588157")
    ) %>%
      layout(
        #title = paste("Letalidad por lesiones autoinfligidas intencionalmente en", selected_year),
        xaxis = list(title = "Tasa de letalidad"),
        yaxis = list(title = "", automargin = TRUE),
        plot_bgcolor = "white",
        paper_bgcolor = "white"
      )
  })
  # -------------------------------------------------------------------
  #  GRÁFICO DE BARRAS: PRINCIPALES CAUSAS ATENCIÓN
  # -------------------------------------------------------------------
  output$anio_atencion_ui <- renderUI({
    anios_disponibles <- datos_total[
      str_detect(Indicador1, regex("atendid", ignore_case = TRUE)) & 
      Tipo == "Geografia" &
      Categoria == "Geografia" &
      !is.na(Valor1) & Valor1 > 0,
      sort(unique(Ano))
    ]
    pickerInput(
      inputId = "anio_atencion",
      label = "Selecciona un año:",
      choices = anios_disponibles,
      selected = if (length(anios_disponibles) > 0)
        max(anios_disponibles) else NULL,
      multiple = FALSE,
      width = "160px"
    )
  })
  output$grafico_top_atencion <- renderPlotly({
    req(input$tab_activa == "Atención", input$anio_atencion)
    selected_year_a <- input$anio_atencion
    df_year_a <- datos_total[
      Ano == selected_year_a &
      str_detect(Indicador1, regex("atendid", ignore_case = TRUE))
    ]
    validate(
      need(nrow(df_year_a) > 0, paste("No hay datos para el año", selected_year_a))
    )
    top10_causas <- df_year_a[
      Tipo == "Geografia" & Categoria == "Geografia" & !is.na(Valor1) & Valor1 > 0,
      .(valor_total = mean(Valor1, na.rm = TRUE)),
      by = Indicador1
    ][order(-valor_total)][1:10]
    validate(
      need(nrow(top10_causas) > 0, "No hay causas con valores disponibles")
    )
    top10_causas[, Indicador_wrap := wrap_text(Indicador1, width = 110)]
    # --- Crear gráfico ---
    plot_ly(
      data = top10_causas,
      x = ~valor_total,
      y = ~reorder(Indicador_wrap, valor_total),
      type = "bar",
      orientation = "h",
      text = ~round(valor_total, 2),
      hoverinfo = "text",
      marker = list(color = "#588157")
    ) %>%
      layout(
        #title = paste("Promedio de principales causas de atención en", selected_year_a),
        xaxis = list(title = "Casos de atención"),
        yaxis = list(title = "")
      )
  })
  # -------------------------------------------------------------------
  #  GRÁFICO DE BARRAS: PRINCIPALES CAUSAS HOSPITALIZACIÓN
  # -------------------------------------------------------------------
  output$anio_hospitalizacion_ui <- renderUI({
    anios_disponibles <- datos_total[
      str_detect(Indicador1, regex("hospitalizad", ignore_case = TRUE)) &
      Tipo == "Geografia" &
      Categoria == "Geografia" &
      !is.na(Valor1) & Valor1 > 0,
      sort(unique(Ano))
    ]
    pickerInput(
      inputId = "anio_hospitalizacion",
      label = "Selecciona un año:",
      choices = anios_disponibles,
      selected = if (length(anios_disponibles) > 0) max(anios_disponibles) else NULL,
      multiple = FALSE,
      width = "160px"
    )
  })
  output$grafico_top_hospitalizacion <- renderPlotly({
    req(input$tab_activa == "Hospitalización", input$anio_hospitalizacion)
    selected_year_h <- input$anio_hospitalizacion
    df_year_h <- datos_total[
      Ano == selected_year_h &
      str_detect(Indicador1, regex("hospitalizad", ignore_case = TRUE))
    ]
    validate(
      need(nrow(df_year_h) > 0, paste("No hay datos para el año", selected_year_h))
    )
    top10_causas_h <- df_year_h[
      Tipo == "Geografia" & Categoria == "Geografia" & !is.na(Valor1) & Valor1 > 0,
      .(valor_total = mean(Valor1, na.rm = TRUE)),
      by = Indicador1
    ][order(-valor_total)][1:10]
    validate(
      need(nrow(top10_causas_h) > 0, "No hay causas con valores disponibles")
    )
    top10_causas_h[, Indicador_wrap := wrap_text(Indicador1, width = 100)]
    # --- Crear gráfico ---
    plot_ly(
      data = top10_causas_h,
      x = ~valor_total,
      y = ~reorder(Indicador_wrap, valor_total),
      type = "bar",
      orientation = "h",
      text = ~round(valor_total, 2),
      hoverinfo = "text",
      marker = list(color = "#588157")
    ) %>%
      layout(
        #title = paste("Promedio de principales causas de hospitalización en", selected_year_h),
        xaxis = list(title = "Casos de hostpitalización"),
        yaxis = list(title = "")
      )
  })
  # -------------------------------------------------------------------
  # INDICADOR DEMENCIA: GRÁFICO DE LÍNEAS
  # -------------------------------------------------------------------
  # --- Filtros dinámicos ---
  output$indicador_demencia_ui <- renderUI({
    selectInput("indicador_demencia", "Indicador:",
      choices = sort(unique(datos_demencia$Indicador11)), 
      selected = unique(datos_demencia$Indicador11)[1]
    )
  })
  
  output$muni_demencia_ui <- renderUI({
    req(input$indicador_demencia)

    municipios_disp_demencia <- datos_demencia[Indicador11 == input$indicador_demencia]
    municipios_disp_demencia <- sort(unique(municipios_disp_demencia$Municipio))

    defaults <- c("La Union", "El Carmen", "Rionegro", "La Ceja", "El Retiro")
    selected_defaults <- intersect(defaults, municipios_disp_demencia)

    if (length(selected_defaults) == 0) {
    selected_defaults <- head(municipios_disp_demencia, 5)
  }

  pickerInput(
      inputId = "municipio_demencia", 
      label = "Municipio:",           
      choices = municipios_disp_demencia,
      selected = selected_defaults,
      multiple = TRUE,
      options = list(`actions-box` = TRUE,
                    `live-search` = TRUE, 
                    `selected-text-format` = "count > 3",
                     selectAllText = "Seleccionar Todo",
                     deselectAllText = "Deseleccionar Todo",
                     countSelectedText = "{0} municipios seleccionados")
    )
  })

  output$year_min_demencia_ui <- renderUI({
    selectInput("year_min_demencia", "Año desde:",
                choices = sort(unique(datos_demencia$Año)),
                selected = min(datos_demencia$Año))
  })

  output$year_max_demencia_ui <- renderUI({
    selectInput("year_max_demencia", "Año hasta:",
                choices = sort(unique(datos_demencia$Año)),
                selected = max(datos_demencia$Año))
  })
  output$tipo_demencia_ui <- renderUI({
    selectInput("tipo_demencia", "Tipo:",
                choices = c("Sexo", "Grupo Etario"),
                selected = "Sexo")
  })
  # El contenido de "Categoría" depende del tipo elegido
  output$categoria_demencia_ui <- renderUI({
    req(input$tipo_demencia)
    if (input$tipo_demencia == "Sexo") {
      categorias <- c("Todos" = "Total", sort(unique(datos_demencia$Sexo[datos_demencia$Sexo != "Total"])))
    } else {
      categorias <- c("Todos" = "TOTAL", sort(unique(datos_demencia$Grupo_Etareo[datos_demencia$Grupo_Etareo != "TOTAL"])))
    }
    selectInput("categoria_demencia", "Categoría:",
                choices = categorias,
                selected = ifelse(input$tipo_demencia == "Sexo", "Total", "TOTAL"))
  })
    # --- Observador para AÑO MÍNIMO ---
  observeEvent(input$year_min_demencia, {
    req(input$year_min_demencia, input$year_max_demencia)
    todos_los_anios <- sort(unique(datos_demencia$Año))
    min_anio_seleccionado <- as.numeric(input$year_min_demencia)
    max_anio_actual <- as.numeric(input$year_max_demencia)
    anios_validos_max <- todos_los_anios[todos_los_anios >= min_anio_seleccionado]
    nuevo_max_seleccionado <- if (max_anio_actual %in% anios_validos_max) {
      max_anio_actual
    } else {
      max(anios_validos_max)
    }
    updateSelectInput(session, "year_max_demencia",
                      choices = anios_validos_max,
                      selected = nuevo_max_seleccionado)
  })
  # --- Observador para AÑO MAXIMO ---
  observeEvent(input$year_max_demencia, {
    req(input$year_min_demencia, input$year_max_demencia)
    todos_los_anios <- sort(unique(datos_demencia$Año))
    max_anio_seleccionado <- as.numeric(input$year_max_demencia)
    min_anio_actual <- as.numeric(input$year_min_demencia)
    anios_validos_min <- todos_los_anios[todos_los_anios <= max_anio_seleccionado]
    nuevo_min_seleccionado <- if (min_anio_actual %in% anios_validos_min) {
      min_anio_actual
    } else {
      min(anios_validos_min)
    }
    updateSelectInput(session, "year_min_demencia",
                      choices = anios_validos_min,
                      selected = nuevo_min_seleccionado)
  })
  # --- Reactivo: Filtrado de datos ---
  datos_filtrados_demencia <- reactive({
    req(input$indicador_demencia,
      input$year_min_demencia,
      input$year_max_demencia,
      input$municipio_demencia,
      input$tipo_demencia,
      input$categoria_demencia
    )
    df_step1 <- datos_demencia [
      Indicador11 == input$indicador_demencia &
      Año >= input$year_min_demencia &
      Año <= input$year_max_demencia &
      Municipio %chin% input$municipio_demencia
    ]
    df_final <- if (input$tipo_demencia == "Sexo") {
      df_step1[
        Grupo_Etareo == "TOTAL" &
        Sexo == input$categoria_demencia
      ]
    } else {
      df_step1[
        Sexo == "Total" &
        Grupo_Etareo == input$categoria_demencia
      ]
    }
    return(df_final)
  })
  # --- Gráfico Plotly ---
  output$grafico_demencia <- renderPlotly({
    df_filtrado <- datos_filtrados_demencia() 
    validate(need(nrow(df_filtrado) > 0,
    "No hay datos disponibles para los filtros seleccionados."))
    df_agg <- df_filtrado[, .(Valor_Plot = mean(Valor, na.rm = TRUE)), 
                            by = .(Año, Municipio)]
    
    df_agg <- df_agg %>%
        mutate(Año = factor(Año))
    #  Plot
    plot_ly(df_agg, 
            x = ~Año,
            y = ~Valor_Plot,
            color = ~Municipio,
            type = 'scatter', 
            mode = 'lines+markers',
            hoverinfo = "text",
            text = ~paste("Municipio:", Municipio, "<br>Año:", Año, "<br>Valor:", round(Valor_Plot, 2)),
            opacity = 1) %>%
      layout(
        #title = input$indicador_demencia, 
        xaxis = list(title = "Año"),
        yaxis = list(title = "Valor"),
        hovermode = "x unified",
        plot_bgcolor = "white", 
        paper_bgcolor = "white" 
      )
  })
  # --- Botones: Actualizar y Limpiar ---
  observeEvent(input$reset_filters_demencia, {
    req(input$indicador_demencia)

    municipios_disp_demencia <- datos_demencia[Indicador11 == input$indicador_demencia]
    municipios_disp_demencia <- sort(unique(municipios_disp_demencia$Municipio))

    defaults <- c("La Union", "El Carmen", "Rionegro", "La Ceja", "El Retiro")
      selected_defaults <- intersect(defaults, municipios_disp_demencia)
      
      if (length(selected_defaults) == 0) {
        selected_defaults <- head(municipios_disp_demencia, 5)
      }

    updatePickerInput(session, "municipio_demencia",
                    selected = selected_defaults)

   
    updateSelectInput(session, "tipo_demencia", selected = "Sexo")
    updateSelectInput(session, "categoria_demencia", selected = "Total")
    updateSelectInput(session, "year_min_demencia", selected = min(datos_demencia$Año))
    updateSelectInput(session, "year_max_demencia", selected = max(datos_demencia$Año))
  })
  # -------------------------------------------------------------------
  # GRÁFICO DE LÍNEAS: INDICADOR SUICIDIO
  # -------------------------------------------------------------------
  # --- Filtros dinámicos ---
  output$indicador_suicidio_ui <- renderUI({
    selectInput("indicador_suicidio", "Indicador:",
      choices = sort(unique(datos_suicidio$Indicador11)), 
      selected = unique(datos_suicidio$Indicador11)[1]
    )
  })
  output$muni_suicidio_ui <- renderUI({
    req(input$indicador_suicidio)

    municipios_disp_suicidio <- datos_suicidio[Indicador11 == input$indicador_suicidio]
    municipios_disp_suicidio <- sort(unique(municipios_disp_suicidio$Municipio))

    defaults <- c("La Union", "El Carmen", "Rionegro", "La Ceja", "El Retiro")
    selected_defaults <- intersect(defaults, municipios_disp_suicidio)

    if (length(selected_defaults) == 0) {
    selected_defaults <- head(municipios_disp_suicidio, 5)
  }

  pickerInput(
      inputId = "municipio_suicidio", 
      label = "Municipio:",           
      choices = municipios_disp_suicidio,
      selected = selected_defaults,
      multiple = TRUE,
      options = list(`actions-box` = TRUE,
                    `live-search` = TRUE, 
                    `selected-text-format` = "count > 3",
                     selectAllText = "Seleccionar Todo",
                     deselectAllText = "Deseleccionar Todo",
                     countSelectedText = "{0} municipios seleccionados")
    )
  })
  output$year_min_suicidio_ui <- renderUI({
    selectInput("year_min_suicidio", "Año desde:",
                choices = sort(unique(datos_suicidio$Año)),
                selected = min(datos_suicidio$Año))
  })
  output$year_max_suicidio_ui <- renderUI({
    selectInput("year_max_suicidio", "Año hasta:",
                choices = sort(unique(datos_suicidio$Año)),
                selected = max(datos_suicidio$Año))
  })
  output$tipo_suicidio_ui <- renderUI({
    selectInput("tipo_suicidio", "Tipo:",
                choices = c("Sexo", "Grupo Etario"),
                selected = "Sexo")
  })
  # El contenido de "Categoría" depende del tipo elegido
  output$categoria_suicidio_ui <- renderUI({
    req(input$tipo_suicidio)
    
    if (input$tipo_suicidio == "Sexo") {
      categorias <- c("Todos" = "Total", sort(unique(datos_suicidio$Sexo[datos_suicidio$Sexo != "Total"])))
    } else {
      categorias <- c("Todos" = "Total", sort(unique(datos_suicidio$Grupo_Etareo[datos_suicidio$Grupo_Etareo != "Total"])))
    }
    selectInput("categoria_suicidio", "Categoría:",
                choices = categorias,
                selected = ifelse(input$tipo_suicidio == "Sexo",
                                  "Total", "Total"))
  })
  # --- Observador para AÑO MÍNIMO ---
  observeEvent(input$year_min_suicidio, {
    req(input$year_min_suicidio, input$year_max_suicidio)
    todos_los_anios <- sort(unique(datos_suicidio$Año))
    min_anio_seleccionado <- as.numeric(input$year_min_suicidio)
    max_anio_actual <- as.numeric(input$year_max_suicidio)
    anios_validos_max <- todos_los_anios[todos_los_anios >= min_anio_seleccionado]
    nuevo_max_seleccionado <- if (max_anio_actual %in% anios_validos_max) {
      max_anio_actual
    } else {
      max(anios_validos_max)
    }
    updateSelectInput(session, "year_max_suicidio",
                      choices = anios_validos_max,
                      selected = nuevo_max_seleccionado)
  })
  # --- Observador para AÑO MAXIMO ---
  observeEvent(input$year_max_suicidio, {
    req(input$year_min_suicidio, input$year_max_suicidio)
    todos_los_anios <- sort(unique(datos_suicidio$Año))
    max_anio_seleccionado <- as.numeric(input$year_max_suicidio)
    min_anio_actual <- as.numeric(input$year_min_suicidio)
    anios_validos_min <- todos_los_anios[todos_los_anios <= max_anio_seleccionado]
    nuevo_min_seleccionado <- if (min_anio_actual %in% anios_validos_min) {
      min_anio_actual
    } else {
      min(anios_validos_min)
    }
    updateSelectInput(session, "year_min_suicidio",
                      choices = anios_validos_min,
                      selected = nuevo_min_seleccionado)
  })


  # --- Reactivo: Filtrado de datos ---
  datos_filtrados_suicidio <- reactive({
    req(input$indicador_suicidio, 
        input$year_min_suicidio, 
        input$year_max_suicidio, 
        input$municipio_suicidio,
        input$tipo_suicidio,
        input$categoria_suicidio
    )
    df_step1 <- datos_suicidio [
      Indicador11 == input$indicador_suicidio &
      Año >= input$year_min_suicidio &
      Año <= input$year_max_suicidio &
      Municipio %chin% input$municipio_suicidio
    ]
    df_final <- if (input$tipo_suicidio == "Sexo") {
      df_step1[
        Grupo_Etareo == "Total" &
        Sexo == input$categoria_suicidio
      ]
    } else {
      df_step1[
        Sexo == "Total" &
        Grupo_Etareo == input$categoria_suicidio
      ]
    }
    return(df_final)
  })
  # --- Gráfico Plotly ---
  output$grafico_suicidio <- renderPlotly({
    df_filtrado <- datos_filtrados_suicidio() 
    validate(need(nrow(df_filtrado) > 0, "No hay datos disponibles para los filtros seleccionados."))
    df_agg <- df_filtrado[, .(Valor_Plot = mean(Valor, na.rm = TRUE)), 
                            by = .(Año, Municipio)]
    
    df_agg <- df_agg %>%
        mutate(Año = factor(Año))
    #  Plot
    plot_ly(df_agg, 
            x = ~Año,     
            y = ~Valor_Plot,  
            color = ~Municipio,
            type = "scatter", 
            mode = "lines+markers",
            hoverinfo = "text",
            text = ~paste("Municipio:", Municipio, "<br>Año:", Año, "<br>Valor:", round(Valor_Plot, 2)),
            opacity = 1) %>%
      layout(
        #title = input$indicador_suicidio, 
        xaxis = list(title = "Año"),
        yaxis = list(title = "Valor"),
        hovermode = "x unified",
        plot_bgcolor = "white", 
        paper_bgcolor = "white" 
      )
  })
  # --- Botones: Actualizar y Limpiar ---
  observeEvent(input$reset_filters_suicidio, {
    req(input$indicador_suicidio)

    municipios_disp_suicidio <- datos_suicidio[Indicador11 == input$indicador_suicidio]
    municipios_disp_suicidio <- sort(unique(municipios_disp_suicidio$Municipio))

    defaults <- c("La Union", "El Carmen", "Rionegro", "La Ceja", "El Retiro")
      selected_defaults <- intersect(defaults, municipios_disp_suicidio)
      
      if (length(selected_defaults) == 0) {
        selected_defaults <- head(municipios_disp_suicidio, 5)
      }

    updatePickerInput(session, "municipio_suicidio",
                    selected = selected_defaults)
    updateSelectInput(session, "tipo_suicidio", selected = "Sexo")
    updateSelectInput(session, "categoria_suicidio", selected = "Total")
    updateSelectInput(session, "year_min_suicidio", selected = min(datos_suicidio$Año))
    updateSelectInput(session, "year_max_suicidio", selected = max(datos_suicidio$Año))
  })

  # -------------------------------------------------------------------
#  GRÁFICO DE USO DE SERVICIOS 
# -------------------------------------------------------------------

# Detectar columna de valor válida en datos_servicios (Valor1 o Valor)
valor_col_servicios <- if ("Valor1" %in% names(datos_servicios)) {
  "Valor1"
} else if ("Valor" %in% names(datos_servicios)) {
  "Valor"
} else {
  stop("No se encontró columna de valor en 'datos_servicios'. Debe existir 'Valor1' o 'Valor'.")
}

# --- Filtros dinámicos ---
output$indicador_servicios_ui <- renderUI({
  selectInput(
    inputId = "indicador_servicios",
    label = "Indicador:",
    choices = sort(unique(datos_servicios$Indicador11)),
    selected = unique(datos_servicios$Indicador11)[1]
  )
})

output$muni_servicios_ui <- renderUI({
  req(input$indicador_servicios)
  municipios_disp_servicios <- datos_servicios[Indicador11 == input$indicador_servicios]
  municipios_disp_servicios <- sort(unique(municipios_disp_servicios$Municipio))
  defaults <- c("La Union", "El Carmen", "Rionegro", "La Ceja", "El Retiro")
  selected_defaults <- intersect(defaults, municipios_disp_servicios)
  if (length(selected_defaults) == 0) selected_defaults <- head(municipios_disp_servicios, 5)
  pickerInput(
    inputId = "municipio_servicios",
    label = "Municipio:",
    choices = municipios_disp_servicios,
    selected = selected_defaults,
    multiple = TRUE,
    options = list(`actions-box` = TRUE, `live-search` = TRUE, `selected-text-format` = "count > 3")
  )
})

# --- Año desde / hasta ---
output$year_min_servicios_ui <- renderUI({
  selectInput("year_min_servicios", "Año desde:",
              choices = sort(unique(datos_servicios$Año)),
              selected = min(datos_servicios$Año))
})
output$year_max_servicios_ui <- renderUI({
  selectInput("year_max_servicios", "Año hasta:",
              choices = sort(unique(datos_servicios$Año)),
              selected = max(datos_servicios$Año))
})

# --- NUEVOS: Tipo y Categoria (botones solicitados) ---
output$tipo_servicios_ui <- renderUI({
  selectInput(
    inputId = "tipo_servicios",
    label = "Tipo:",
    choices = c("Sexo", "Grupo Etario"),
    selected = "Sexo"
  )
})

output$categoria_servicios_ui <- renderUI({
  req(input$tipo_servicios)
  if (input$tipo_servicios == "Sexo") {
    opciones <- sort(unique(datos_servicios$Sexo))
    # normalizar / quitar posibles NA y valores vacíos
    opciones <- opciones[!is.na(opciones) & str_trim(opciones) != ""]
    choices <- c("Todos", opciones)
    selected <- "Todos"
  } else {
    opciones <- sort(unique(datos_servicios$Grupo_Etareo))
    opciones <- opciones[!is.na(opciones) & str_trim(opciones) != ""]
    choices <- c("Todos", opciones)
    selected <- "Todos"
  }
  selectInput(
    inputId = "categoria_servicios",
    label = "Categoria:",
    choices = choices,
    selected = selected
  )
})

# -------------------------------------------------------------------
#  FILTRADO DE DATOS (SERVICIOS)
# -------------------------------------------------------------------
datos_filtrados_servicios <- reactive({
  req(
    input$indicador_servicios,
    input$year_min_servicios,
    input$year_max_servicios,
    input$municipio_servicios,
    input$tipo_servicios,
    input$categoria_servicios
  )
  
  df_filtrado <- datos_servicios[
    Indicador11 == input$indicador_servicios &
      Año >= input$year_min_servicios &
      Año <= input$year_max_servicios &
      Municipio %chin% input$municipio_servicios
  ]
  
  # Aplicar filtro por tipo / categoría si se seleccionó algo distinto de "Todos"
  if (!is.null(input$tipo_servicios) && !is.null(input$categoria_servicios)) {
    if (input$tipo_servicios == "Sexo" && input$categoria_servicios != "Todos") {
      df_filtrado <- df_filtrado[Sexo == input$categoria_servicios]
    } else if (input$tipo_servicios == "Grupo Etario" && input$categoria_servicios != "Todos") {
      df_filtrado <- df_filtrado[Grupo_Etareo == input$categoria_servicios]
    }
  }
  
  return(df_filtrado)
})

# -------------------------------------------------------------------
#  GRÁFICO PLOTLY (líneas — promedio por año y municipio)
# -------------------------------------------------------------------
output$grafico_servicios <- renderPlotly({
  df_filtrado <- datos_filtrados_servicios()
  validate(need(nrow(df_filtrado) > 0, "No hay datos disponibles para los filtros seleccionados."))
  
  # Asegurarse de que la columna de valor esté numérica
  # (get(valor_col_servicios) ya la devuelve; compute mean)
  df_agg <- df_filtrado[, .(Valor_Plot = mean(get(valor_col_servicios), na.rm = TRUE)), by = .(Año, Municipio)]
  
  df_agg <- df_agg %>%
        mutate(Año = factor(Año))

  plot_ly(
    df_agg,
    x = ~Año,
    y = ~Valor_Plot,
    color = ~Municipio,
    type = "scatter",
    mode = "lines+markers",
    hoverinfo = "text",
    text = ~paste("Municipio:", Municipio, "<br>Año:", Año, "<br>Valor:", round(Valor_Plot, 2)),
    opacity = 1
  ) %>%
    layout(
      xaxis = list(title = "Año"),
      yaxis = list(title = "Valor promedio"),
      hovermode = "x unified",
      plot_bgcolor = "white",
      paper_bgcolor = "white"
    )
})

# -------------------------------------------------------------------
#  BOTÓN RESET filtros servicios
# -------------------------------------------------------------------
observeEvent(input$reset_filters_servicios, {
  req(input$indicador_servicios)
  
  municipios_disp_servicios <- datos_servicios[Indicador11 == input$indicador_servicios]
  municipios_disp_servicios <- sort(unique(municipios_disp_servicios$Municipio))
  defaults <- c("La Union", "El Carmen", "Rionegro", "La Ceja", "El Retiro")
  selected_defaults <- intersect(defaults, municipios_disp_servicios)
  if (length(selected_defaults) == 0) selected_defaults <- head(municipios_disp_servicios, 5)
  
  updatePickerInput(session, "municipio_servicios", selected = selected_defaults)
  updateSelectInput(session, "tipo_servicios", selected = "Sexo")
  updateSelectInput(session, "categoria_servicios", selected = "Todos")
  updateSelectInput(session, "year_min_servicios", selected = min(datos_servicios$Año))
  updateSelectInput(session, "year_max_servicios", selected = max(datos_servicios$Año))
})

}
