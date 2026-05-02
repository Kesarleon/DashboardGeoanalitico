# ══════════════════════════════════════════════════════════════════════════════
# R/modules/mod_market_analysis.R
# Módulo Shiny: Análisis de Mercado (UI + Server)
# ══════════════════════════════════════════════════════════════════════════════

#' Market Analysis Module — UI
#'
#' Renderiza el panel de análisis competitivo del mercado hospitalario.
#' Incluye KPIs de concentración (HHI, Top3, Gini), gráfica de market share,
#' tabla de instalaciones y análisis de canibalización condicional.
#'
#' @param id character. Namespace ID del módulo Shiny.
#'
#' @return Un objeto de UI Shiny (`tagList`).
#'
#' @seealso [mod_market_analysis_server()]
#'
#' @export
mod_market_analysis_ui <- function(id) {
  ns <- NS(id)

  tagList(
    page_header(
      "③ ANÁLISIS",
      "Análisis de Mercado y Competencia",
      "Dinámica competitiva del sector hospitalario · Manzanillo, Colima",
      icon = "pie-chart-fill"
    ),

    # KPIs de concentración de mercado
    tags$div(
      class = "kpi-row",
      uiOutput(ns("kpi_hhi")),
      uiOutput(ns("kpi_top3")),
      uiOutput(ns("kpi_gini")),
      uiOutput(ns("kpi_competitors"))
    ),

    # Gráfica de market share (bar horizontal)
    card(
      card_header(
        tags$div(
          class = "card-title-with-icon",
          bsicons::bs_icon("pie-chart-fill"),
          "Participación de Mercado por Instalación"
        )
      ),
      card_body(
        plotly::plotlyOutput(ns("grafica_market_share"), height = "380px")
      )
    ),

    layout_columns(
      col_widths = c(6, 6),

      # Tabla de competidores
      card(
        card_header("Análisis por Instalación"),
        card_body(
          DT::dataTableOutput(ns("tabla_competidores"))
        )
      ),

      # Métricas de concentración
      card(
        card_header("Métricas de Concentración"),
        card_body(
          uiOutput(ns("metricas_detalle"))
        )
      )
    ),

    # Análisis de canibalización — solo visible con múltiples proyectos
    conditionalPanel(
      condition = "output.tiene_canibalizacion",
      ns = ns,
      card(
        card_header(
          tags$div(
            class = "card-title-with-icon",
            bsicons::bs_icon("arrow-left-right"),
            "Análisis de Canibalización entre Proyectos"
          )
        ),
        card_body(
          DT::dataTableOutput(ns("tabla_canibalizacion"))
        )
      )
    )
  )
}


#' Market Analysis Module — Server
#'
#' Lógica del servidor para el módulo de análisis de mercado.
#' Consume los resultados del modelo Huff del módulo de simulación y computa
#' KPIs de concentración, gráficas y tablas comparativas.
#'
#' @param id character. Namespace ID del módulo Shiny.
#' @param market_data list con reactivos provenientes de `mod_market_simulation_server`:
#'   \describe{
#'     \item{huff_results}{reactive. Lista con `resumen_hospitales` y `metricas`}
#'     \item{captacion_proyecto}{reactive. Resumen del proyecto propuesto}
#'   }
#'
#' @return list con reactivos: `competidores`, `metricas_mercado`
#'
#' @seealso [mod_market_analysis_ui()]
#'
#' @export
mod_market_analysis_server <- function(id, market_data) {
  moduleServer(id, function(input, output, session) {

    # === DATOS REACTIVOS ===

    competidores <- reactive({
      req(market_data$huff_results())
      market_data$huff_results()$resumen_hospitales %>%
        arrange(desc(total_pacientes))
    })

    metricas_mercado <- reactive({
      req(market_data$huff_results())
      market_data$huff_results()$metricas
    })

    tiene_canibalizacion <- reactive({
      req(competidores())
      sum(competidores()$tipo == "proyecto", na.rm = TRUE) > 1
    })

    # === KPIs ===

    output$kpi_hhi <- renderUI({
      req(metricas_mercado())
      hhi <- metricas_mercado()$hhi_index
      interpretacion <- if (is.na(hhi)) {
        "Sin datos"
      } else if (hhi < 1500) {
        "Mercado competitivo"
      } else if (hhi < 2500) {
        "Concentración moderada"
      } else {
        "Alta concentración"
      }
      kpi_card("HHI INDEX", format_number(round(hhi)), interpretacion,
               icon = "graph-up")
    })

    output$kpi_top3 <- renderUI({
      req(metricas_mercado())
      kpi_card(
        "TOP 3 SHARE",
        format_percentage(metricas_mercado()$top3_concentration, 1),
        "Concentración top 3 instalaciones",
        icon = "trophy-fill"
      )
    })

    output$kpi_gini <- renderUI({
      req(metricas_mercado())
      kpi_card(
        "GINI COEF.",
        round(metricas_mercado()$gini_coefficient, 3),
        "Desigualdad de distribución de mercado",
        icon = "graph-down"
      )
    })

    output$kpi_competitors <- renderUI({
      req(metricas_mercado())
      kpi_card(
        "COMPETIDORES",
        metricas_mercado()$n_competitors,
        "Instalaciones activas en el modelo",
        icon = "hospital"
      )
    })

    # === GRÁFICA DE MARKET SHARE ===

    output$grafica_market_share <- plotly::renderPlotly({
      req(competidores())

      paleta <- c("#f5a623", "#38bdf8", "#4ade80", "#f87171",
                  "#c084fc", "#fb923c", "#e2e8f0", "#67e8f9",
                  "#fde68a", "#a78bfa", "#34d399", "#f472b6")

      df <- competidores() %>%
        mutate(
          nombre_fmt   = stringr::str_to_title(tolower(nombre)),
          share_pct    = round(cuota_mercado * 100, 1),
          pacs_fmt     = format_number(round(total_pacientes * 12)),
          color        = paleta[((seq_len(n()) - 1L) %% length(paleta)) + 1L]
        )

      plot_ly(
        df,
        x           = ~share_pct,
        y           = ~reorder(nombre_fmt, share_pct),
        type        = "bar",
        orientation = "h",
        marker      = list(color = df$color, cornerradius = 3),
        customdata  = ~pacs_fmt,
        hovertemplate = paste0(
          "<b>%{y}</b><br>",
          "Market Share: %{x:.1f}%<br>",
          "Pacientes/año: %{customdata}<br>",
          "<extra></extra>"
        )
      ) |>
        dark_plotly() |>
        layout(
          xaxis = list(ticksuffix = "%", title = "Market Share (%)"),
          yaxis = list(title = ""),
          bargap = 0.35
        ) |>
        config(displayModeBar = FALSE)
    })

    # === TABLA DE COMPETIDORES ===

    output$tabla_competidores <- DT::renderDataTable({
      req(competidores())

      df <- competidores() %>%
        transmute(
          Instalación            = stringr::str_to_title(tolower(nombre)),
          Tipo                   = tipo,
          `Total Pac./año`       = format_number(round(total_pacientes * 12)),
          `Market Share`         = format_percentage(cuota_mercado, 1),
          `Dist. Prom. (km)`     = round(distancia_prom_pond, 1)
        )

      DT::datatable(
        df,
        options = list(
          pageLength = 15,
          dom        = "tp",
          order      = list(list(3L, "desc")),
          scrollX    = TRUE,
          columnDefs = list(
            list(className = "dt-center", targets = 1:4)
          )
        ),
        rownames = FALSE,
        escape   = FALSE
      )
    })

    # === MÉTRICAS DETALLADAS ===

    output$metricas_detalle <- renderUI({
      req(metricas_mercado())
      m <- metricas_mercado()

      tags$div(
        class = "metrics-detail",
        metric_row("Total de Mercado",
                   paste0(format_number(round(m$total_market * 12)), " pac/año")),
        tags$hr(),
        metric_row(
          "HHI (Herfindahl-Hirschman)",
          format_number(round(m$hhi_index)),
          "< 1,500: Competitivo · 1,500–2,500: Moderado · > 2,500: Concentrado"
        ),
        tags$hr(),
        metric_row("Concentración Top 3",
                   format_percentage(m$top3_concentration, 1)),
        tags$hr(),
        metric_row(
          "Coeficiente de Gini",
          round(m$gini_coefficient, 3),
          "0 = Distribución equitativa · 1 = Desigualdad máxima"
        ),
        tags$hr(),
        metric_row("Número de Instalaciones", m$n_competitors)
      )
    })

    # === TABLA DE CANIBALIZACIÓN ===

    output$tabla_canibalizacion <- DT::renderDataTable({
      req(tiene_canibalizacion(), competidores())

      proyectos <- competidores() %>%
        filter(tipo == "proyecto")

      if (nrow(proyectos) <= 1L) return(NULL)

      proyectos %>%
        transmute(
          Proyecto               = stringr::str_to_title(tolower(nombre)),
          `Pacientes Captados`   = format_number(round(total_pacientes * 12)),
          `Market Share`         = format_percentage(cuota_mercado, 1),
          `Área Influencia (km)` = round(distancia_prom_pond * 1.5, 1)
        ) %>%
        DT::datatable(
          options = list(pageLength = 10, dom = "t"),
          rownames = FALSE
        )
    })

    # Expone el booleano al conditionalPanel
    output$tiene_canibalizacion <- reactive({ tiene_canibalizacion() })
    outputOptions(output, "tiene_canibalizacion", suspendWhenHidden = FALSE)

    # === RETORNO ===
    list(
      competidores     = competidores,
      metricas_mercado = metricas_mercado
    )
  })
}


# ── Helpers internos del módulo ───────────────────────────────────────────────

#' Render a metric row for the detail panel
#' @keywords internal
metric_row <- function(label, value, help_text = NULL) {
  tags$div(
    class = "metric-item",
    tags$div(class = "metric-label", label),
    tags$div(class = "metric-value", value),
    if (!is.null(help_text)) tags$small(class = "metric-help", help_text)
  )
}
