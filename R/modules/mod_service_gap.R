# ══════════════════════════════════════════════════════════════════════════════
# R/modules/mod_service_gap.R
# Módulo Shiny: Brecha de Servicios (UI + Server)
# Analiza la brecha entre oferta local y demanda por especialidad médica,
# usando fuga_absoluta y fuga_pct del código fuente (líneas 133-134).
# ══════════════════════════════════════════════════════════════════════════════

#' Service Gap Module — UI
#'
#' Renderiza el panel de análisis de brecha de servicios médicos por especialidad.
#' Incluye KPIs de cobertura, gráfica oferta vs demanda, tabla semáforo,
#' benchmarks de estándares y tarjetas de especialidades críticas.
#'
#' @param id character. Namespace ID del módulo Shiny.
#'
#' @return Un objeto de UI Shiny (`tagList`).
#'
#' @seealso [mod_service_gap_server()]
#'
#' @export
mod_service_gap_ui <- function(id) {
  ns <- NS(id)

  tagList(
    page_header(
      "④ BRECHA",
      "Análisis de Brecha de Servicios",
      "Oferta vs Demanda por Especialidad Médica · Clasificación semáforo de criticidad",
      icon = "clipboard-pulse"
    ),

    # KPIs de cobertura general
    tags$div(
      class = "kpi-row",
      uiOutput(ns("kpi_cobertura_total")),
      uiOutput(ns("kpi_deficit_total")),
      uiOutput(ns("kpi_fuga_total")),
      uiOutput(ns("kpi_especialidades_criticas"))
    ),

    # Gráfica comparativa oferta vs demanda
    card(
      card_header(
        tags$div(
          class = "card-title-with-icon",
          bsicons::bs_icon("bar-chart-fill"),
          "Oferta vs Demanda por Especialidad (Pacientes/Año)"
        )
      ),
      card_body(
        plotly::plotlyOutput(ns("grafica_brechas"), height = "420px")
      )
    ),

    layout_columns(
      col_widths = c(8, 4),

      # Tabla detallada de brechas
      card(
        card_header(
          tags$div(
            class = "card-title-with-icon",
            bsicons::bs_icon("table"),
            "Análisis Detallado por Especialidad"
          )
        ),
        card_body(
          DT::dataTableOutput(ns("tabla_brechas"))
        )
      ),

      # Benchmark de cobertura (camas/1,000 hab)
      card(
        card_header("Estándares de Cobertura — Camas/1,000 hab."),
        card_body(
          uiOutput(ns("bench_cobertura"))
        )
      )
    ),

    # Especialidades críticas
    card(
      card_header(
        tags$div(
          class = "card-title-with-icon",
          bsicons::bs_icon("exclamation-triangle-fill"),
          "Especialidades Críticas (Mayor Déficit de Atención)"
        )
      ),
      card_body(
        uiOutput(ns("especialidades_criticas"))
      )
    )
  )
}


#' Service Gap Module — Server
#'
#' Lógica de servidor para el análisis de brecha de servicios por especialidad.
#' Combina datos de fuga de pacientes (fuga_absoluta, fuga_pct) con capacidad
#' instalada (actual_c1000, estandar_c1000) para calcular déficits y cobertura.
#'
#' Fuente de datos: vectores globales del app_canibalizacion_lovable_3.R
#'   - fuga_absoluta  (línea 134): pacientes que salen de Manzanillo/año
#'   - fuga_pct       (línea 133): % de pacientes que salen
#'   - actual_c1000   (línea 130): camas actuales por 1,000 habitantes
#'   - estandar_c1000 (línea 131): estándar nacional de camas por 1,000
#'
#' @param id character. Namespace ID del módulo Shiny.
#' @param market_data reactive opcional. Resultados del módulo de simulación de
#'   mercado. Si se proporciona, puede usarse para enriquecer el análisis.
#'
#' @return list con reactivos: `brechas_analysis` y `metricas_totales`.
#'
#' @seealso [mod_service_gap_ui()]
#'
#' @export
mod_service_gap_server <- function(id, market_data = NULL) {
  moduleServer(id, function(input, output, session) {

    # ══════════════════════════════════════════════════════════════════════════
    # DATOS BASE — extraídos de app_canibalizacion_lovable_3.R líneas 128-135
    # ══════════════════════════════════════════════════════════════════════════

    # Vectores paralelos de 10 especialidades
    .especialidades  <- c("Medicina Interna", "Cirugía General", "Pediatría",
                          "Ginecología", "Traumatología", "Cardiología",
                          "Oncología", "Nefrología", "Urgencias", "Oftalmología")

    # Camas por 1,000 habitantes (capacidad instalada vs estándar nacional)
    .actual_c1000    <- c(2.1, 1.8, 2.3, 2.0, 0.8, 0.5, 0.2, 0.4, 3.2, 1.0)
    .estandar_c1000  <- c(3.0, 2.5, 2.5, 2.2, 2.0, 1.5, 1.0, 0.8, 3.0, 1.0)

    # Fuga de pacientes (líneas 133-134 del app original)
    .fuga_pct        <- c(22, 18, 12, 13, 35, 42, 65, 28,  3,  5)
    .fuga_absoluta   <- c(310, 280, 195, 240, 450, 380, 320, 145, 95, 85)

    # Oferta local derivada: pacientes atendidos en Manzanillo
    # Fórmula: fuga_pct = fuga_absoluta / total_demanda → oferta = fuga * (100-pct)/pct
    .oferta_pac      <- round(.fuga_absoluta * (100 - .fuga_pct) / .fuga_pct)
    # Resultado: c(1099, 1276, 1430, 1606, 836, 525, 172, 374, 3072, 1615)

    # ══════════════════════════════════════════════════════════════════════════
    # REACTIVOS DE DATOS
    # ══════════════════════════════════════════════════════════════════════════

    # Análisis completo de brechas por especialidad
    brechas_analysis <- reactive({
      brecha_c1000 <- .actual_c1000 - .estandar_c1000

      data.frame(
        especialidad    = .especialidades,
        actual_c1000    = .actual_c1000,
        estandar_c1000  = .estandar_c1000,
        brecha_c1000    = round(brecha_c1000, 1),
        oferta_pac      = .oferta_pac,
        fuga_pac        = .fuga_absoluta,
        total_demanda   = .oferta_pac + .fuga_absoluta,
        fuga_pct        = .fuga_pct,
        cobertura_pct   = 100 - .fuga_pct,
        criticidad      = dplyr::case_when(
          brecha_c1000 < -0.5 ~ "Crítica",
          brecha_c1000 <  0   ~ "Moderada",
          TRUE                ~ "Cubierta"
        ),
        stringsAsFactors = FALSE
      )
    })

    # Métricas agregadas para KPIs
    metricas_totales <- reactive({
      df <- brechas_analysis()
      list(
        fuga_total        = sum(df$fuga_pac),
        oferta_total      = sum(df$oferta_pac),
        demanda_total     = sum(df$total_demanda),
        cobertura_promedio = mean(df$cobertura_pct),
        n_criticas        = sum(df$criticidad == "Crítica"),
        n_moderadas       = sum(df$criticidad == "Moderada"),
        n_cubiertas       = sum(df$criticidad == "Cubierta")
      )
    })

    # ══════════════════════════════════════════════════════════════════════════
    # KPIs
    # ══════════════════════════════════════════════════════════════════════════

    output$kpi_cobertura_total <- renderUI({
      m <- metricas_totales()
      kpi_card(
        "COBERTURA PROMEDIO",
        paste0(round(m$cobertura_promedio, 1), "%"),
        "Pacientes atendidos localmente",
        icon = "shield-check"
      )
    })

    output$kpi_deficit_total <- renderUI({
      m <- metricas_totales()
      kpi_card(
        "FUGA TOTAL",
        format_number(m$fuga_total),
        "Pacientes/año fuera de Manzanillo",
        sub_cls = "kpi-down",
        icon = "arrow-right-circle"
      )
    })

    output$kpi_fuga_total <- renderUI({
      m <- metricas_totales()
      kpi_card(
        "DEMANDA TOTAL",
        format_number(m$demanda_total),
        paste0("Oferta local: ", format_number(m$oferta_total)),
        icon = "people-fill"
      )
    })

    output$kpi_especialidades_criticas <- renderUI({
      m <- metricas_totales()
      kpi_card(
        "ESPECIALIDADES CRÍTICAS",
        m$n_criticas,
        paste0(m$n_moderadas, " moderadas · ", m$n_cubiertas, " cubiertas"),
        sub_cls = "kpi-down",
        icon = "flag-fill"
      )
    })

    # ══════════════════════════════════════════════════════════════════════════
    # GRÁFICA — Oferta vs Fuga (barras agrupadas)
    # ══════════════════════════════════════════════════════════════════════════

    output$grafica_brechas <- plotly::renderPlotly({
      df <- brechas_analysis() |>
        dplyr::arrange(dplyr::desc(fuga_pac))

      plotly::plot_ly(df) |>
        plotly::add_trace(
          x    = ~especialidad,
          y    = ~oferta_pac,
          type = "bar",
          name = "Oferta Local (atendidos)",
          marker = list(color = "#4ade80", opacity = 0.85)
        ) |>
        plotly::add_trace(
          x    = ~especialidad,
          y    = ~fuga_pac,
          type = "bar",
          name = "Fuga (sin atención local)",
          marker = list(color = "#f87171", opacity = 0.85)
        ) |>
        plotly::layout(
          barmode = "group",
          xaxis = list(
            title      = "",
            tickangle  = -40,
            color      = "#e2e8f0",
            tickfont   = list(size = 11)
          ),
          yaxis = list(
            title    = "Pacientes / Año",
            color    = "#e2e8f0",
            gridcolor = "#252f45"
          ),
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor  = "rgba(0,0,0,0)",
          font   = list(family = "Inter, sans-serif", color = "#e2e8f0"),
          legend = list(
            orientation = "h",
            x = 0.5, xanchor = "center", y = 1.08,
            font = list(size = 11)
          ),
          hovermode = "x unified",
          margin = list(t = 50, b = 10)
        ) |>
        plotly::config(displayModeBar = FALSE)
    })

    # ══════════════════════════════════════════════════════════════════════════
    # TABLA — Análisis por especialidad con semáforo HTML
    # ══════════════════════════════════════════════════════════════════════════

    output$tabla_brechas <- DT::renderDataTable({
      df <- brechas_analysis()

      color_criticidad <- dplyr::case_when(
        df$criticidad == "Crítica"  ~ "#f87171",
        df$criticidad == "Moderada" ~ "#fb923c",
        TRUE                        ~ "#4ade80"
      )

      color_brecha <- ifelse(df$brecha_c1000 < 0, "#f87171", "#4ade80")

      display <- data.frame(
        Especialidad    = df$especialidad,
        `Actual c/1000` = df$actual_c1000,
        `Estándar`      = df$estandar_c1000,
        `Brecha`        = paste0(
          '<span style="color:', color_brecha, ';font-weight:600;">',
          df$brecha_c1000, '</span>'
        ),
        `Oferta Local`  = format_number(df$oferta_pac),
        `Fuga Pac.`     = format_number(df$fuga_pac),
        `Cobertura`     = paste0(df$cobertura_pct, "%"),
        Estado          = paste0(
          '<span style="color:', color_criticidad,
          ';font-weight:700;font-size:13px;">● ', df$criticidad, '</span>'
        ),
        check.names = FALSE,
        stringsAsFactors = FALSE
      )

      DT::datatable(
        display,
        escape   = FALSE,
        rownames = FALSE,
        options  = list(
          dom         = "t",
          ordering    = TRUE,
          pageLength  = 12,
          columnDefs  = list(
            list(className = "dt-center", targets = 1:7)
          )
        ),
        class = "display"
      )
    })

    # ══════════════════════════════════════════════════════════════════════════
    # BENCHMARK — Estándares de camas/1,000 hab.
    # ══════════════════════════════════════════════════════════════════════════

    output$bench_cobertura <- renderUI({
      # Población base del estudio (línea 40 del app)
      pop <- 191031

      tagList(
        tags$div(
          style = "display:flex;flex-direction:column;gap:8px;",
          bench_card("3.4",  "Estándar OCDE",
                     paste0("Faltan ", format_number(ceiling(pop * 3.4 / 1000) - 221), " camas"),
                     "#f5a623"),
          bench_card("2.5",  "Meta OMS",
                     paste0("Mínimo recomendado · Faltan ", format_number(ceiling(pop * 2.5 / 1000) - 221), " camas"),
                     "#fb923c"),
          bench_card("1.5",  "Promedio Nacional MX",
                     paste0("Faltan ", format_number(ceiling(pop * 1.5 / 1000) - 221), " camas"),
                     "#38bdf8"),
          bench_card("1.12", "Manzanillo Actual",
                     "221 camas totales (público + privado)",
                     "#f87171")
        )
      )
    })

    # ══════════════════════════════════════════════════════════════════════════
    # ESPECIALIDADES CRÍTICAS — tarjetas de las top 5 por déficit
    # ══════════════════════════════════════════════════════════════════════════

    output$especialidades_criticas <- renderUI({
      criticas <- brechas_analysis() |>
        dplyr::filter(criticidad %in% c("Crítica", "Moderada")) |>
        dplyr::arrange(dplyr::desc(fuga_pac)) |>
        head(5)

      if (nrow(criticas) == 0) {
        return(tags$p(
          style = "color:#64748b;",
          "No hay especialidades con déficit identificado."
        ))
      }

      color_crit <- function(c) {
        switch(c, "Crítica" = "#f87171", "Moderada" = "#fb923c", "#4ade80")
      }

      tarjetas <- lapply(seq_len(nrow(criticas)), function(i) {
        esp <- criticas[i, ]
        tags$div(
          style = paste0(
            "background:#1e293b;border-left:4px solid ", color_crit(esp$criticidad),
            ";border-radius:6px;padding:14px 18px;margin-bottom:10px;"
          ),
          tags$div(
            style = "display:flex;justify-content:space-between;align-items:center;margin-bottom:8px;",
            tags$strong(
              style = "font-size:14px;color:#e2e8f0;",
              paste0(i, ". ", esp$especialidad)
            ),
            tags$span(
              style = paste0(
                "background:", color_crit(esp$criticidad),
                ";color:#0f172a;font-size:11px;font-weight:700;",
                "padding:2px 8px;border-radius:10px;"
              ),
              esp$criticidad
            )
          ),
          tags$div(
            style = "display:grid;grid-template-columns:repeat(3,1fr);gap:8px;",
            tags$div(
              tags$div(style = "font-size:10px;color:#64748b;text-transform:uppercase;", "Fuga pac./año"),
              tags$div(style = "font-size:16px;font-weight:700;color:#f87171;",
                       format_number(esp$fuga_pac))
            ),
            tags$div(
              tags$div(style = "font-size:10px;color:#64748b;text-transform:uppercase;", "Cobertura local"),
              tags$div(style = "font-size:16px;font-weight:700;color:#e2e8f0;",
                       paste0(esp$cobertura_pct, "%"))
            ),
            tags$div(
              tags$div(style = "font-size:10px;color:#64748b;text-transform:uppercase;", "Brecha c/1,000"),
              tags$div(style = paste0("font-size:16px;font-weight:700;color:", color_crit(esp$criticidad), ";"),
                       paste0(esp$brecha_c1000, " camas"))
            )
          )
        )
      })

      tagList(tarjetas)
    })

    # ══════════════════════════════════════════════════════════════════════════
    # RETORNO — reactivos exportados para uso en otros módulos
    # ══════════════════════════════════════════════════════════════════════════

    return(list(
      brechas_analysis = brechas_analysis,
      metricas_totales = metricas_totales
    ))
  })
}
