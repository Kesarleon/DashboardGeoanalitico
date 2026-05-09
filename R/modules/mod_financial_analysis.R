# ══════════════════════════════════════════════════════════════════════════════
# R/modules/mod_financial_analysis.R
# Módulo Shiny: Análisis Financiero (UI + Server)
# Proyecciones a 10 años, métricas TIR/VPN/ROI/Payback y análisis de
# sensibilidad. Usa proyectar_flujos_caja() y calcular_metricas_financieras()
# de R/models/financial_model.R.
# Fuente original: app_canibalizacion_lovable_3.R (líneas 322-406, 773-970)
# ══════════════════════════════════════════════════════════════════════════════


# ── 1. mod_financial_analysis_ui ─────────────────────────────────────────────

#' Financial Analysis Module — UI
#'
#' Panel de análisis financiero con sidebar de parámetros, KPIs de inversión,
#' gráfica de flujos de caja a 10 años, tabla de proyecciones detalladas,
#' comparación de tres modelos de negocio y análisis de sensibilidad.
#'
#' @param id character. Namespace ID del módulo Shiny.
#'
#' @return Un objeto de UI Shiny (`tagList`).
#'
#' @seealso [mod_financial_analysis_server()]
#'
#' @export
mod_financial_analysis_ui <- function(id) {
  ns <- NS(id)

  layout_sidebar(
    fillable = FALSE,

    # ── Sidebar ───────────────────────────────────────────────────────────────
    sidebar = sidebar(
      width = 300,

      tags$h4("Parámetros Financieros",
              class = "sidebar-title", style = "margin-top:0;"),
      tags$p("Ajusta los supuestos para proyectar los flujos a 10 años.",
             style = "color:#64748b; font-size:11px;"),
      tags$hr(),

      # ① Inversión (CAPEX)
      tags$h5("① Inversión (CAPEX)"),
      numericInput(ns("capex_fase1"), "CAPEX Fase 1 (MDP):",
                   value = 250, min = 50, step = 10),
      numericInput(ns("capex_fase2"), "CAPEX Fase 2 (MDP):",
                   value = 200, min = 0,  step = 10),
      numericInput(ns("capex_fase3"), "CAPEX Fase 3 (MDP):",
                   value = 200, min = 0,  step = 10),
      tags$hr(),

      # ② Ingresos
      tags$h5("② Ingresos"),
      numericInput(ns("captacion_anual"), "Captación anual (pacientes):",
                   value = 8429, min = 1000, step = 100),
      numericInput(ns("tarifa_promedio"), "Tarifa promedio (MXN/paciente):",
                   value = 50000, min = 10000, step = 1000),
      sliderInput(ns("tasa_ocupacion"), "Tasa de ocupación:",
                  min = 0.60, max = 0.95, value = 0.85, step = 0.05),
      sliderInput(ns("tasa_crecimiento"), "Crecimiento anual (ingresos):",
                  min = 0.00, max = 0.10, value = 0.03, step = 0.01),
      tags$hr(),

      # ③ Egresos
      tags$h5("③ Egresos"),
      numericInput(ns("costo_variable"), "Costo variable/paciente (MXN):",
                   value = 30000, min = 5000, step = 1000),
      numericInput(ns("costos_fijos"), "Costos fijos anuales (MDP):",
                   value = 50, min = 10, step = 5),
      tags$hr(),

      # ④ Modelo de negocio
      tags$h5("④ Modelo de Negocio"),
      selectInput(
        ns("modelo_negocio"),
        "Modelo de Negocio:",
        choices = c(
          "Solo Fase 1"                   = "fase1",
          "Escalonado 2 Fases (Año 4)"    = "escalonado2",
          "Escalonado 3 Fases (Año 4, 7)" = "escalonado3"
        ),
        selected = "escalonado3"
      )
    ),

    # ── Contenido principal ───────────────────────────────────────────────────
    div(style = "padding: 4px 8px;",

      page_header(
        "⑤ FINANCIERO",
        "Proyecciones y Modelo de Negocio",
        "Análisis financiero a 10 años · TIR, VPN, ROI y sensibilidad de escenarios",
        icon = "currency-dollar"
      ),

      # KPIs
      tags$div(
        class = "kpi-row",
        uiOutput(ns("kpi_irr")),
        uiOutput(ns("kpi_npv")),
        uiOutput(ns("kpi_roi")),
        uiOutput(ns("kpi_payback"))
      ),

      # Gráfica de flujos de caja
      card(
        card_header(
          tags$div(
            class = "card-title-with-icon",
            bsicons::bs_icon("graph-up"),
            "Proyección de Flujos de Caja (10 años)"
          )
        ),
        card_body(
          plotly::plotlyOutput(ns("grafica_flujos"), height = "380px")
        )
      ),

      layout_columns(
        col_widths = c(8, 4),

        # Tabla de proyecciones año por año
        card(
          card_header("Proyecciones Detalladas por Año"),
          card_body(
            DT::dataTableOutput(ns("tabla_proyecciones"))
          )
        ),

        # Comparación de los 3 modelos
        card(
          card_header("Comparación de Modelos"),
          card_body(
            uiOutput(ns("comparacion_modelos"))
          )
        )
      ),

      # Análisis de sensibilidad
      card(
        card_header(
          tags$div(
            class = "card-title-with-icon",
            bsicons::bs_icon("sliders"),
            "Análisis de Sensibilidad"
          )
        ),
        card_body(
          layout_columns(
            col_widths = c(4, 8),

            tags$div(
              selectInput(
                ns("param_sensibilidad"),
                "Parámetro a variar:",
                choices = c(
                  "Tarifa Promedio"    = "tarifa_promedio",
                  "Captación Anual"   = "captacion_anual",
                  "Costos Fijos"      = "costos_fijos_anuales",
                  "Tasa de Ocupación" = "tasa_ocupacion"
                )
              ),
              sliderInput(
                ns("rango_sensibilidad"),
                "Rango de variación (%):",
                min = -30, max = 30,
                value = c(-20, 20),
                step = 5
              ),
              tags$div(
                style = "margin-top:8px;",
                HTML('<div class="info-box"><p>Cada punto muestra la TIR resultante cuando el
                parámetro seleccionado varía dentro del rango indicado. El centro (0%) representa el
                escenario base.</p></div>')
              )
            ),

            plotly::plotlyOutput(ns("grafica_sensibilidad"), height = "300px")
          )
        )
      )
    )
  )
}


# ── 2. mod_financial_analysis_server ─────────────────────────────────────────

#' Financial Analysis Module — Server
#'
#' Lógica del servidor para proyecciones financieras a 10 años, cálculo de
#' métricas (TIR, VPN, ROI, payback), comparación de tres modelos de negocio
#' y análisis de sensibilidad sobre parámetros clave.
#'
#' @param id character. Namespace ID del módulo Shiny.
#' @param project_data reactive opcional. Datos del proyecto (captacion, etc.)
#'   provenientes de otros módulos como market simulation.
#'
#' @return list con reactivos:
#'   \describe{
#'     \item{flujos_proyectados}{data.frame con proyecciones anuales.}
#'     \item{metricas_financieras}{list con TIR, VPN, ROI, payback.}
#'     \item{comparacion_modelos}{list con métricas de los 3 modelos.}
#'   }
#'
#' @export
mod_financial_analysis_server <- function(id, project_data = NULL) {
  moduleServer(id, function(input, output, session) {

    # ══════════════════════════════════════════════════════════════════════════
    # PARÁMETROS REACTIVOS
    # ══════════════════════════════════════════════════════════════════════════

    # Resolución del modelo: años de arranque de cada fase según selección
    config_fases <- reactive({
      list(
        año_fase2 = if (input$modelo_negocio %in% c("escalonado2", "escalonado3")) 4L  else NULL,
        año_fase3 = if (input$modelo_negocio == "escalonado3")                      7L  else NULL,
        capex2    = if (input$modelo_negocio %in% c("escalonado2", "escalonado3")) input$capex_fase2 * 1e6 else 0,
        capex3    = if (input$modelo_negocio == "escalonado3")                      input$capex_fase3 * 1e6 else 0
      )
    })

    # Lista de parámetros base para proyectar_flujos_caja() y analisis_sensibilidad()
    # Unidades: pesos MXN (CAPEX y costos_fijos convertidos desde MDP × 1e6)
    params_base <- reactive({
      req(input$capex_fase1, input$captacion_anual, input$tarifa_promedio,
          input$tasa_ocupacion, input$costo_variable, input$costos_fijos,
          input$tasa_crecimiento)

      cf <- config_fases()
      list(
        capex_fase1             = input$capex_fase1 * 1e6,
        capex_fase2             = cf$capex2,
        capex_fase3             = cf$capex3,
        año_fase2               = cf$año_fase2,
        año_fase3               = cf$año_fase3,
        captacion_anual         = input$captacion_anual,
        tarifa_promedio         = input$tarifa_promedio,
        tasa_ocupacion          = input$tasa_ocupacion,
        costo_variable_paciente = input$costo_variable,
        costos_fijos_anuales    = input$costos_fijos * 1e6,
        tasa_crecimiento        = input$tasa_crecimiento,
        años                    = 10L
      )
    })

    # ══════════════════════════════════════════════════════════════════════════
    # PROYECCIONES FINANCIERAS
    # ══════════════════════════════════════════════════════════════════════════

    flujos_proyectados <- reactive({
      req(params_base())
      tryCatch(
        do.call(proyectar_flujos_caja, params_base()),
        error = function(e) NULL
      )
    })

    metricas_financieras <- reactive({
      req(flujos_proyectados(), params_base())
      tryCatch(
        calcular_metricas_financieras(
          flujos_df      = flujos_proyectados(),
          capex_inicial  = -params_base()$capex_fase1,
          tasa_descuento = 0.12
        ),
        error = function(e) NULL
      )
    })

    # ══════════════════════════════════════════════════════════════════════════
    # COMPARACIÓN DE LOS 3 MODELOS
    # ══════════════════════════════════════════════════════════════════════════

    comparacion_modelos_data <- reactive({
      req(input$captacion_anual, input$tarifa_promedio)

      params_comun <- list(
        captacion_anual         = input$captacion_anual,
        tarifa_promedio         = input$tarifa_promedio,
        tasa_ocupacion          = input$tasa_ocupacion,
        costo_variable_paciente = input$costo_variable,
        costos_fijos_anuales    = input$costos_fijos * 1e6,
        tasa_crecimiento        = input$tasa_crecimiento,
        años                    = 10L
      )
      c1 <- input$capex_fase1 * 1e6
      c2 <- input$capex_fase2 * 1e6
      c3 <- input$capex_fase3 * 1e6

      calc_modelo <- function(extra_params) {
        p <- c(params_comun, extra_params)
        fl <- tryCatch(do.call(proyectar_flujos_caja, p), error = function(e) NULL)
        if (is.null(fl)) return(list(flujos = NULL, metricas = NULL))
        mt <- tryCatch(
          calcular_metricas_financieras(fl, capex_inicial = -p$capex_fase1),
          error = function(e) NULL
        )
        list(flujos = fl, metricas = mt)
      }

      list(
        fase1 = calc_modelo(list(
          capex_fase1 = c1, capex_fase2 = 0,  capex_fase3 = 0,
          año_fase2 = NULL, año_fase3 = NULL
        )),
        escalonado2 = calc_modelo(list(
          capex_fase1 = c1, capex_fase2 = c2, capex_fase3 = 0,
          año_fase2 = 4L,   año_fase3 = NULL
        )),
        escalonado3 = calc_modelo(list(
          capex_fase1 = c1, capex_fase2 = c2, capex_fase3 = c3,
          año_fase2 = 4L,   año_fase3 = 7L
        ))
      )
    })

    # ══════════════════════════════════════════════════════════════════════════
    # ANÁLISIS DE SENSIBILIDAD
    # ══════════════════════════════════════════════════════════════════════════

    sensibilidad_results <- reactive({
      req(params_base(), input$param_sensibilidad, input$rango_sensibilidad)

      min_mult <- 1 + input$rango_sensibilidad[1] / 100
      max_mult <- 1 + input$rango_sensibilidad[2] / 100
      rango    <- seq(min_mult, max_mult, length.out = 9)

      tryCatch(
        analisis_sensibilidad(
          params_base    = params_base(),
          variable       = input$param_sensibilidad,
          rango          = rango,
          tasa_descuento = 0.12
        ),
        error = function(e) NULL
      )
    })

    # ══════════════════════════════════════════════════════════════════════════
    # OUTPUTS — KPIs
    # ══════════════════════════════════════════════════════════════════════════

    output$kpi_irr <- renderUI({
      req(metricas_financieras())
      m <- metricas_financieras()
      tir_txt <- if (is.na(m$irr)) "N/A" else format_percentage(m$irr, 1)
      kpi_card("TIR / IRR", tir_txt,
               "Tasa interna de retorno · benchmark 12%",
               sub_cls = if (!is.na(m$irr) && m$irr > 0.12) "kpi-up" else "kpi-down",
               icon = "percent")
    })

    output$kpi_npv <- renderUI({
      req(metricas_financieras())
      m <- metricas_financieras()
      vpn_mdp <- round(m$npv / 1e6, 1)
      kpi_card("VPN / NPV",
               paste0("$", vpn_mdp, " MDP"),
               "Valor presente neto · WACC 12%",
               sub_cls = if (m$npv > 0) "kpi-up" else "kpi-down",
               icon = "cash-stack")
    })

    output$kpi_roi <- renderUI({
      req(metricas_financieras())
      m <- metricas_financieras()
      roi_txt <- if (is.na(m$roi)) "N/A" else format_percentage(m$roi - 1, 1)
      kpi_card("ROI 10 AÑOS", roi_txt,
               "Retorno sobre inversión total",
               sub_cls = if (!is.na(m$roi) && m$roi > 1) "kpi-up" else "kpi-down",
               icon = "graph-up-arrow")
    })

    output$kpi_payback <- renderUI({
      req(metricas_financieras())
      m <- metricas_financieras()
      pb_txt <- if (is.na(m$payback_periodo)) "No recupera" else paste0(m$payback_periodo, " años")
      kpi_card("PAYBACK", pb_txt,
               "Período de recuperación de la inversión",
               icon = "clock-history")
    })

    # ══════════════════════════════════════════════════════════════════════════
    # OUTPUTS — GRÁFICAS
    # ══════════════════════════════════════════════════════════════════════════

    output$grafica_flujos <- plotly::renderPlotly({
      req(flujos_proyectados())
      d <- flujos_proyectados()

      # Convertir a MDP para el eje Y
      plot_ly(d) |>
        add_trace(
          x = ~año, y = ~round(ingresos / 1e6, 1),
          type = "scatter", mode = "lines+markers",
          name = "Ingresos",
          line   = list(color = COLOR_SCHEME$success, width = 3),
          marker = list(color = COLOR_SCHEME$success, size  = 7),
          hovertemplate = "Año %{x} · Ingresos: $%{y:.1f} MDP<extra></extra>"
        ) |>
        add_trace(
          x = ~año, y = ~round(egresos / 1e6, 1),
          type = "scatter", mode = "lines+markers",
          name = "Egresos",
          line   = list(color = COLOR_SCHEME$danger, width = 3),
          marker = list(color = COLOR_SCHEME$danger, size  = 7),
          hovertemplate = "Año %{x} · Egresos: $%{y:.1f} MDP<extra></extra>"
        ) |>
        add_trace(
          x = ~año, y = ~round(flujo_neto / 1e6, 1),
          type = "scatter", mode = "lines+markers",
          name = "Flujo Neto",
          line   = list(color = COLOR_SCHEME$primary, width = 3),
          marker = list(color = COLOR_SCHEME$primary, size  = 7),
          hovertemplate = "Año %{x} · Flujo neto: $%{y:.1f} MDP<extra></extra>"
        ) |>
        add_trace(
          x = ~año, y = ~round(flujo_acumulado / 1e6, 1),
          type = "scatter", mode = "lines",
          name = "Flujo Acumulado",
          line = list(color = COLOR_SCHEME$secondary, width = 2, dash = "dash"),
          hovertemplate = "Año %{x} · Acumulado: $%{y:.1f} MDP<extra></extra>"
        ) |>
        dark_plotly(
          xaxis = list(
            title    = "Año",
            tickvals = 1:10,
            ticktext = paste0("Año ", 1:10),
            gridcolor = "#252f45", color = "#64748b"
          ),
          yaxis = list(
            title     = "Millones de Pesos (MDP)",
            ticksuffix = "M",
            gridcolor = "#252f45", color = "#64748b"
          ),
          legend = list(
            orientation = "h", x = 0.5, xanchor = "center", y = 1.12,
            font = list(color = "#94a3b8", size = 10)
          ),
          hovermode = "x unified",
          # Línea cero como referencia de breakeven
          shapes = list(list(
            type = "line", y0 = 0, y1 = 0, x0 = 1, x1 = 10,
            line = list(color = "#f87171", dash = "dot", width = 1)
          ))
        )
    })

    output$grafica_sensibilidad <- plotly::renderPlotly({
      req(sensibilidad_results())
      df <- sensibilidad_results()

      variacion_pct <- round((df$multiplicador - 1) * 100, 1)
      tir_pct       <- round(df$irr * 100, 2)

      plot_ly(x = variacion_pct, y = tir_pct,
              type = "scatter", mode = "lines+markers",
              name = "TIR",
              line   = list(color = COLOR_SCHEME$primary, width = 3),
              marker = list(color = ifelse(tir_pct > 12, COLOR_SCHEME$success, COLOR_SCHEME$danger),
                            size = 10),
              hovertemplate = "Variación %{x}% → TIR %{y:.1f}%<extra></extra>") |>
        dark_plotly(
          xaxis = list(
            title       = paste0("Variación de ", input$param_sensibilidad, " (%)"),
            gridcolor   = "#252f45", color = "#64748b",
            zeroline    = TRUE, zerolinecolor = "#475569", zerolinewidth = 1
          ),
          yaxis = list(
            title     = "TIR (%)",
            ticksuffix = "%",
            gridcolor = "#252f45", color = "#64748b"
          ),
          # Benchmark WACC 12%
          shapes = list(list(
            type = "line",
            x0 = min(variacion_pct), x1 = max(variacion_pct),
            y0 = 12, y1 = 12,
            line = list(color = "#38bdf8", dash = "dot", width = 1)
          ))
        )
    })

    # ══════════════════════════════════════════════════════════════════════════
    # OUTPUTS — TABLA DE PROYECCIONES
    # ══════════════════════════════════════════════════════════════════════════

    output$tabla_proyecciones <- DT::renderDataTable({
      req(flujos_proyectados())
      d <- flujos_proyectados()

      fmt_mdp <- function(x) paste0("$", sprintf("%.1f", x / 1e6))

      # Color de flujo neto: verde si positivo, rojo si negativo
      fn_fmt <- mapply(function(v, raw) {
        col <- if (raw >= 0) "#4ade80" else "#f87171"
        sprintf('<span style="color:%s;font-weight:600;">%s</span>', col, v)
      }, fmt_mdp(d$flujo_neto), d$flujo_neto, SIMPLIFY = TRUE)

      fa_fmt <- mapply(function(v, raw) {
        col <- if (raw >= 0) "#4ade80" else "#f87171"
        sprintf('<span style="color:%s;font-weight:600;">%s</span>', col, v)
      }, fmt_mdp(d$flujo_acumulado), d$flujo_acumulado, SIMPLIFY = TRUE)

      tabla <- data.frame(
        Año             = d$año,
        Fase            = d$fase,
        `Ingresos (MDP)` = fmt_mdp(d$ingresos),
        `Egresos (MDP)`  = fmt_mdp(d$egresos),
        `Flujo Neto`     = fn_fmt,
        `Flujo Acum.`    = fa_fmt,
        stringsAsFactors = FALSE,
        check.names      = FALSE
      )

      DT::datatable(
        tabla,
        escape   = FALSE,
        rownames = FALSE,
        options  = list(
          dom         = "t",
          ordering    = FALSE,
          pageLength  = 10
        ),
        class = "display"
      )
    })

    # ══════════════════════════════════════════════════════════════════════════
    # OUTPUTS — COMPARACIÓN DE MODELOS
    # ══════════════════════════════════════════════════════════════════════════

    output$comparacion_modelos <- renderUI({
      req(comparacion_modelos_data())
      comp <- comparacion_modelos_data()

      modelo_card <- function(titulo, datos, recomendado = FALSE) {
        if (is.null(datos$metricas)) return(NULL)
        m <- datos$metricas

        tir_txt <- if (is.na(m$irr))  "N/A"   else format_percentage(m$irr, 1)
        vpn_txt <- if (is.na(m$npv))  "N/A"   else paste0("$", round(m$npv / 1e6, 0), " MDP")
        roi_txt <- if (is.na(m$roi))  "N/A"   else format_percentage(m$roi - 1, 1)
        pb_txt  <- if (is.na(m$payback_periodo)) ">10 años" else paste0(m$payback_periodo, " años")

        borde <- if (recomendado) "border: 1px solid rgba(245,166,35,.5);" else ""
        badge <- if (recomendado) tags$span(
          style = paste0("background:rgba(245,166,35,.15);border:1px solid rgba(245,166,35,.3);",
                         "border-radius:4px;padding:2px 8px;font-size:9px;font-weight:700;",
                         "color:#f5a623;margin-left:6px;"),
          "RECOMENDADO"
        ) else NULL

        tags$div(
          style = paste0("background:#1c2333;border-radius:8px;padding:12px;",
                         "margin-bottom:10px;", borde),
          tags$div(
            style = "font-size:11px;font-weight:700;color:#e2e8f0;margin-bottom:8px;",
            titulo, badge
          ),
          tags$div(style = "display:grid;grid-template-columns:1fr 1fr;gap:6px;",
            tags$div(
              tags$div(style = "font-size:9px;color:#64748b;text-transform:uppercase;", "TIR"),
              tags$div(style = "font-size:14px;font-weight:700;color:#f5a623;", tir_txt)
            ),
            tags$div(
              tags$div(style = "font-size:9px;color:#64748b;text-transform:uppercase;", "VPN"),
              tags$div(style = "font-size:14px;font-weight:700;color:#38bdf8;", vpn_txt)
            ),
            tags$div(
              tags$div(style = "font-size:9px;color:#64748b;text-transform:uppercase;", "ROI"),
              tags$div(style = "font-size:14px;font-weight:700;color:#4ade80;", roi_txt)
            ),
            tags$div(
              tags$div(style = "font-size:9px;color:#64748b;text-transform:uppercase;", "Payback"),
              tags$div(style = "font-size:14px;font-weight:700;color:#e2e8f0;", pb_txt)
            )
          )
        )
      }

      tagList(
        modelo_card("Solo Fase 1",              comp$fase1,       recomendado = FALSE),
        modelo_card("Escalonado 2 Fases",       comp$escalonado2, recomendado = FALSE),
        modelo_card("Escalonado 3 Fases",       comp$escalonado3, recomendado = TRUE)
      )
    })

    # ══════════════════════════════════════════════════════════════════════════
    # RETORNO
    # ══════════════════════════════════════════════════════════════════════════

    return(list(
      flujos_proyectados    = flujos_proyectados,
      metricas_financieras  = metricas_financieras,
      comparacion_modelos   = comparacion_modelos_data
    ))
  })
}
