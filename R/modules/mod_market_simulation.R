# ══════════════════════════════════════════════════════════════════════════════
# R/modules/mod_market_simulation.R
# Módulo Shiny: Simulación de Mercado (UI + Server)
# ══════════════════════════════════════════════════════════════════════════════

#' Market Simulation Module — UI
#'
#' Renderiza el panel de simulación de mercado usando el modelo gravitacional
#' de Huff. Incluye sidebar de controles, KPIs dinámicos, mapa Leaflet,
#' diagrama Sankey de canibalización y tabla de resultados.
#'
#' @param id character. Namespace ID del módulo Shiny.
#'
#' @return Un objeto de UI Shiny (`layout_sidebar`).
#'
#' @seealso [mod_market_simulation_server()]
#'
#' @export
mod_market_simulation_ui <- function(id) {
  ns <- NS(id)

  layout_sidebar(
    sidebar = sidebar(
      width = 320,
      tags$div(class = "sidebar-header", tags$h4("Panel de Simulación")),
      checkboxGroupInput(
        ns("proyectos_activos"), "Proyectos a incluir:",
        choices  = c("Hospital Propuesto" = "PROYECTO"),
        selected = "PROYECTO"
      ),
      sliderInput(
        ns("sensibilidad"), "Sensibilidad a distancia (λ):",
        min = 1, max = 3, value = 2, step = 0.1
      ),
      sliderInput(
        ns("peso_camas"), "Peso de camas en atractivo:",
        min = 0, max = 1, value = 0.1, step = 0.05
      ),
      sliderInput(
        ns("peso_esp"), "Peso de especialidades:",
        min = 0, max = 1, value = 0.9, step = 0.05
      ),
      actionButton(
        ns("actualizar"), "Actualizar Simulación",
        class = "btn-primary w-100"
      )
    ),
    page_header(
      "① SIMULACIÓN",
      "Modelo de Captación de Mercado",
      "Modelo Huff gravitacional calibrado"
    ),
    tags$div(
      class = "kpi-row",
      uiOutput(ns("kpi_captacion")),
      uiOutput(ns("kpi_share")),
      uiOutput(ns("kpi_distancia"))
    ),
    card(
      card_header(
        tags$div(
          class = "card-title-with-icon",
          bsicons::bs_icon("geo-alt-fill"),
          "Mapa de Captación por AGEB"
        )
      ),
      card_body(leafletOutput(ns("mapa"), height = "520px"))
    ),
    card(
      card_header(
        tags$div(
          class = "card-title-with-icon",
          bsicons::bs_icon("diagram-3-fill"),
          "Flujos de Pacientes"
        )
      ),
      card_body(sankeyNetworkOutput(ns("sankey"), height = "340px"))
    ),
    card(
      card_header("Resultados Detallados por Instalación"),
      card_body(DT::dataTableOutput(ns("tabla_resultados")))
    )
  )
}
