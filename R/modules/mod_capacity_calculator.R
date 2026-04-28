# ══════════════════════════════════════════════════════════════════════════════
# R/modules/mod_capacity_calculator.R
# Módulo Shiny: Calculadora de Capacidad Hospitalaria (UI + Server)
# ══════════════════════════════════════════════════════════════════════════════

#' Capacity Calculator Module — UI
#'
#' Renderiza el panel de calculadora de capacidad hospitalaria usando la
#' metodología Hill-Burton adaptada al contexto mexicano. Incluye sidebar
#' de parámetros, KPIs de infraestructura, tabla por especialidad,
#' comparación con estándares internacionales y propuesta por fases.
#'
#' @param id character. Namespace ID del módulo Shiny.
#'
#' @return Un objeto de UI Shiny (`layout_sidebar`).
#'
#' @seealso [mod_capacity_calculator_server()]
#'
#' @export
mod_capacity_calculator_ui <- function(id) {
  ns <- NS(id)

  layout_sidebar(
    sidebar = sidebar(
      width = 320,
      tags$div(
        class = "sidebar-header",
        tags$h4("Parámetros de Cálculo")
      ),
      numericInput(ns("poblacion"), "Población objetivo:", value = 191031, min = 1000, step = 1000),
      sliderInput(ns("tasa_egresos"), "Tasa de egresos (por 1000 hab):",
                  min = 40, max = 120, value = 80, step = 5, post = " / 1000"),
      sliderInput(ns("dias_estancia"), "Días de estancia promedio:",
                  min = 2, max = 8, value = 4.2, step = 0.1, post = " días"),
      sliderInput(ns("ocupacion"), "Tasa de ocupación objetivo:",
                  min = 0.70, max = 0.95, value = 0.85, step = 0.05, post = "%"),
      sliderInput(ns("k_calibracion"), "Factor de calibración:",
                  min = 0.8, max = 1.5, value = 1.0, step = 0.05),
      tags$hr(),
      tags$div(
        class = "info-box",
        tags$small(
          tags$b("Metodología:"), " Hill-Burton", tags$br(),
          "Adaptada para contexto mexicano"
        )
      )
    ),
    page_header(
      "② CAPACIDAD",
      "Calculadora de Infraestructura",
      "Cálculo basado en fórmula Hill-Burton",
      icon = "calculator-fill"
    ),
    tags$div(
      class = "kpi-row",
      uiOutput(ns("kpi_camas")),
      uiOutput(ns("kpi_consultorios")),
      uiOutput(ns("kpi_quirofanos")),
      uiOutput(ns("kpi_uci"))
    ),
    tags$div(
      class = "kpi-row",
      uiOutput(ns("kpi_camas_por_1000")),
      uiOutput(ns("kpi_indice_rotacion")),
      uiOutput(ns("kpi_ucin")),
      uiOutput(ns("kpi_urgencias"))
    ),
    card(
      card_header(
        tags$div(class = "card-title-with-icon",
                 bsicons::bs_icon("hospital-fill"), "Distribución por Especialidad")
      ),
      card_body(DT::dataTableOutput(ns("tabla_especialidades")))
    ),
    card(
      card_header(
        tags$div(class = "card-title-with-icon",
                 bsicons::bs_icon("bar-chart-fill"), "Comparación con Estándares Internacionales")
      ),
      card_body(
        uiOutput(ns("bench_cards")),
        tags$hr(),
        DT::dataTableOutput(ns("tabla_estandares"))
      )
    ),
    card(
      card_header("Propuesta de Desarrollo Escalonado"),
      card_body(uiOutput(ns("desarrollo_fases")))
    )
  )
}
