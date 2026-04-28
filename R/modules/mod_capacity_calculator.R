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


#' Capacity Calculator Module — Server
#'
#' Lógica del servidor para la calculadora de capacidad hospitalaria.
#' Calcula camas censables e infraestructura complementaria usando la
#' metodología Hill-Burton y compara con estándares OMS / OCDE / MX.
#'
#' @param id character. Namespace ID del módulo Shiny.
#'
#' @return list con reactivos:
#'   \describe{
#'     \item{capacidad_results}{Resultado completo de `calcular_capacidad_hospitalaria()`}
#'     \item{camas_censables}{Reactivo con el total de camas censables}
#'   }
#'
#' @seealso [mod_capacity_calculator_ui()]
#'
#' @export
mod_capacity_calculator_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    # === REACTIVOS ===

    capacidad_results <- reactive({
      req(input$poblacion, input$tasa_egresos, input$dias_estancia,
          input$ocupacion, input$k_calibracion)

      tasa_decimal <- input$tasa_egresos / 1000  # Convertir de "por 1000" a decimal

      calcular_capacidad_hospitalaria(
        poblacion                = input$poblacion,
        tasa_egresos             = tasa_decimal,
        dias_estancia_promedio   = input$dias_estancia,
        tasa_ocupacion_objetivo  = input$ocupacion,
        k_calibracion            = input$k_calibracion,
        incluir_complementos     = TRUE
      )
    })

    comparacion_estandares <- reactive({
      req(capacidad_results())
      comparar_con_estandares(
        camas_censables = capacidad_results()$camas_censables,
        poblacion       = input$poblacion
      )
    })

    desarrollo_fases <- reactive({
      req(capacidad_results())
      calcular_desarrollo_por_fases(
        capacidad_total = capacidad_results(),
        fases           = 3,
        distribucion    = c(0.4, 0.35, 0.25)
      )
    })

    # === OUTPUTS - 8 KPIs ===

    output$kpi_camas <- renderUI({
      req(capacidad_results())
      kpi_card("CAMAS CENSABLES", capacidad_results()$camas_censables,
               "Hospitalización general", icon = "hospital")
    })

    output$kpi_consultorios <- renderUI({
      req(capacidad_results())
      kpi_card("CONSULTORIOS", capacidad_results()$consultorios,
               "Atención ambulatoria", icon = "clipboard2-pulse")
    })

    output$kpi_quirofanos <- renderUI({
      req(capacidad_results())
      kpi_card("QUIRÓFANOS", capacidad_results()$quirofanos,
               "Salas de cirugía", icon = "scissors")
    })

    output$kpi_uci <- renderUI({
      req(capacidad_results())
      kpi_card("CAMAS UCI", capacidad_results()$camas_uci,
               "Cuidados intensivos adultos", icon = "heart-pulse-fill")
    })

    output$kpi_camas_por_1000 <- renderUI({
      req(capacidad_results())
      kpi_card("CAMAS / 1000 HAB",
               round(capacidad_results()$metricas$camas_por_1000_hab, 2),
               "Índice de cobertura", icon = "graph-up")
    })

    output$kpi_indice_rotacion <- renderUI({
      req(capacidad_results())
      kpi_card("ÍNDICE ROTACIÓN",
               round(capacidad_results()$metricas$indice_rotacion, 1),
               "Pacientes/cama/año", icon = "arrow-repeat")
    })

    output$kpi_ucin <- renderUI({
      req(capacidad_results())
      kpi_card("CAMAS UCIN", capacidad_results()$camas_ucin,
               "Cuidados intensivos neonatales", icon = "heart-fill")
    })

    output$kpi_urgencias <- renderUI({
      req(capacidad_results())
      kpi_card("CAMAS URGENCIAS", capacidad_results()$camas_urgencias,
               "Atención de emergencias", icon = "exclamation-triangle-fill")
    })

    # CONTINÚA EN PROMPT 3 — tablas, bench_cards, fases y return()

  })
}
