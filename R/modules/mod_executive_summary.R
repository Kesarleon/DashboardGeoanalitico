# ══════════════════════════════════════════════════════════════════════════════
# R/modules/mod_executive_summary.R
# Módulo Shiny: Resumen Ejecutivo (UI + Server)
# Dashboard consolidado que agrega KPIs de los 5 módulos:
#   market_simulation · capacity_calculator · market_analysis
#   service_gap · financial_analysis
# Usa valores por defecto del proyecto Manzanillo si los módulos no están
# disponibles (emula la pestaña "Recomendación" de app_canibalizacion_lovable_3.R).
# ══════════════════════════════════════════════════════════════════════════════


# ── 1. mod_executive_summary_ui ──────────────────────────────────────────────

#' Executive Summary Module — UI
#'
#' Dashboard consolidado con KPIs de todos los análisis, comparación de modelos
#' de negocio, análisis de brechas, recomendación ejecutiva y semáforo de
#' viabilidad.
#'
#' @param id character. Namespace ID del módulo Shiny.
#'
#' @return Un objeto de UI Shiny (`tagList`).
#'
#' @seealso [mod_executive_summary_server()]
#'
#' @export
mod_executive_summary_ui <- function(id) {
  ns <- NS(id)

  tagList(

    # CSS adicional para semáforo y elementos específicos del resumen ejecutivo
    tags$style(HTML("
      .exec-semaforo { display: flex; flex-direction: column; gap: 10px; }
      .semaforo-item {
        display: flex; align-items: flex-start; gap: 14px;
        background: #161b27; border: 1px solid #252f45;
        border-radius: 8px; padding: 12px 16px;
      }
      .semaforo-left  { flex-shrink: 0; display: flex; flex-direction: column; align-items: center; gap: 4px; min-width: 90px; }
      .semaforo-label { font-size: 10px; font-weight: 600; color: #e2e8f0; text-transform: uppercase; letter-spacing: .06em; }
      .semaforo-badge {
        display: inline-flex; align-items: center; gap: 5px;
        border-radius: 5px; padding: 3px 10px;
        font-size: 10px; font-weight: 700; letter-spacing: .04em;
      }
      .semaforo-verde   { background: rgba(74,222,128,.12); border: 1px solid rgba(74,222,128,.35); color: #4ade80; }
      .semaforo-amarillo{ background: rgba(251,146,60,.12);  border: 1px solid rgba(251,146,60,.35);  color: #fb923c; }
      .semaforo-rojo    { background: rgba(248,113,113,.12); border: 1px solid rgba(248,113,113,.35); color: #f87171; }
      .semaforo-right { flex: 1; }
      .semaforo-title { font-size: 12px; font-weight: 600; color: #e2e8f0; margin-bottom: 3px; }
      .semaforo-detail{ font-size: 11px; color: #64748b; line-height: 1.55; }
      .semaforo-veredicto {
        background: rgba(74,222,128,.08); border: 1px solid rgba(74,222,128,.3);
        border-radius: 10px; padding: 16px 20px; margin-top: 4px;
        text-align: center;
      }
      .semaforo-veredicto h4 { color: #4ade80; font-size: 15px; font-weight: 700; margin: 0 0 6px; }
      .semaforo-veredicto p  { font-size: 12px; color: #94a3b8; margin: 0; line-height: 1.6; }
      .brecha-item {
        display: flex; justify-content: space-between; align-items: center;
        padding: 8px 0; border-bottom: 1px solid #252f45;
      }
      .brecha-item:last-child { border-bottom: none; }
      .brecha-name  { font-size: 12px; font-weight: 600; color: #e2e8f0; }
      .brecha-badge { font-size: 10px; font-weight: 700; padding: 2px 8px; border-radius: 4px; }
      .brecha-critica  { background: rgba(248,113,113,.12); color: #f87171; border: 1px solid rgba(248,113,113,.3); }
      .brecha-moderada { background: rgba(251,146,60,.12);  color: #fb923c; border: 1px solid rgba(251,146,60,.3); }
      .brecha-metrics  { font-size: 11px; color: #64748b; }
      .mercado-metric { display: flex; justify-content: space-between; align-items: baseline; padding: 7px 0; border-bottom: 1px solid #252f45; }
      .mercado-metric:last-child { border-bottom: none; }
      .mercado-key { font-size: 11px; color: #94a3b8; }
      .mercado-val { font-size: 13px; font-weight: 700; color: #e2e8f0; }
      .mercado-note{ font-size: 10px; color: #64748b; margin-top: 2px; }
      .card-title-with-icon { display: flex; align-items: center; gap: 8px; font-size: 13px; font-weight: 600; color: #38bdf8; }
    ")),

    # ── Encabezado ─────────────────────────────────────────────────────────────
    div(style = "padding: 20px 24px 0;",
      page_hdr(
        "⑤ SÍNTESIS",
        "Resumen Ejecutivo",
        "Dashboard consolidado — KPIs de todos los análisis · Manzanillo, Colima"
      )
    ),

    div(style = "padding: 0 24px 24px;",

      # ── SECCIÓN 1: KPIs principales ─────────────────────────────────────────
      div(style = "font-size:11px;font-weight:700;color:#64748b;text-transform:uppercase;
                   letter-spacing:.06em;margin-bottom:10px;",
          "Métricas Clave del Proyecto"),

      div(class = "kpi-row",
          uiOutput(ns("kpi_poblacion")),
          uiOutput(ns("kpi_captacion")),
          uiOutput(ns("kpi_market_share")),
          uiOutput(ns("kpi_camas"))
      ),
      div(class = "kpi-row",
          uiOutput(ns("kpi_irr")),
          uiOutput(ns("kpi_roi")),
          uiOutput(ns("kpi_payback")),
          uiOutput(ns("kpi_capex"))
      ),

      tags$hr(style = "border-color:#252f45; margin: 6px 0 18px;"),

      # ── SECCIÓN 2: Comparación de modelos ───────────────────────────────────
      card(
        card_header(
          div(class = "card-title-with-icon",
              bsicons::bs_icon("bar-chart-fill"),
              "Comparación de Modelos de Negocio")
        ),
        card_body(
          DT::dataTableOutput(ns("tabla_comparacion_modelos"))
        )
      ),

      # ── SECCIÓN 3: Brechas + Mercado ────────────────────────────────────────
      layout_columns(
        col_widths = c(6, 6),

        card(
          card_header(
            div(class = "card-title-with-icon",
                bsicons::bs_icon("clipboard-x-fill"),
                "Especialidades Críticas (Mayor Déficit)")
          ),
          card_body(uiOutput(ns("top_brechas")))
        ),

        card(
          card_header(
            div(class = "card-title-with-icon",
                bsicons::bs_icon("pie-chart-fill"),
                "Concentración de Mercado")
          ),
          card_body(uiOutput(ns("metricas_mercado")))
        )
      ),

      # ── SECCIÓN 4: Recomendación ejecutiva ──────────────────────────────────
      card(
        card_header(
          div(class = "card-title-with-icon",
              bsicons::bs_icon("check-circle-fill"),
              "Recomendación Ejecutiva")
        ),
        card_body(uiOutput(ns("recomendacion")))
      ),

      # ── SECCIÓN 5: Semáforo de viabilidad ───────────────────────────────────
      card(
        card_header(
          div(class = "card-title-with-icon",
              bsicons::bs_icon("traffic-light-fill"),
              "Semáforo de Viabilidad")
        ),
        card_body(uiOutput(ns("semaforo_viabilidad")))
      )
    )
  )
}


# ── 2. mod_executive_summary_server ──────────────────────────────────────────

#' Executive Summary Module — Server
#'
#' Consolida resultados de los 5 módulos de análisis. Cada argumento es
#' opcional: si es `NULL` el módulo usa valores por defecto del proyecto
#' Manzanillo (idénticos a la pestaña "Recomendación" original).
#'
#' @param id character. Namespace ID del módulo Shiny.
#' @param market_sim list o NULL. Return value de `mod_market_simulation_server()`.
#'   Debe exponer `$captacion_proyecto` (reactivo con `$total` mensual y `$share`)
#'   y `$huff_results`.
#' @param capacity_calc list o NULL. Return value de `mod_capacity_calculator_server()`.
#'   Debe exponer `$camas_censables` (reactivo numérico).
#' @param market_analysis list o NULL. Return value de `mod_market_analysis_server()`.
#'   Debe exponer `$metricas_mercado` (reactivo con `$hhi_index`, `$top3_concentration`,
#'   `$gini_coefficient`, `$n_competitors`).
#' @param service_gap list o NULL. Return value de `mod_service_gap_server()`.
#'   Debe exponer `$brechas_analysis` (reactivo data.frame con columnas
#'   `especialidad`, `fuga_pac`, `cobertura_pct`, `criticidad`).
#' @param financial list o NULL. Return value de `mod_financial_analysis_server()`.
#'   Debe exponer `$metricas_financieras` (reactivo con `$irr`, `$roi`,
#'   `$payback_periodo`, `$npv`) y `$comparacion_modelos`.
#'
#' @return NULL (módulo de solo visualización).
#'
#' @export
mod_executive_summary_server <- function(
  id,
  market_sim      = NULL,
  capacity_calc   = NULL,
  market_analysis = NULL,
  service_gap     = NULL,
  financial       = NULL
) {
  moduleServer(id, function(input, output, session) {

    # ── Helpers internos ───────────────────────────────────────────────────────

    # Intenta evaluar expr(); devuelve NULL sin error si algo falla
    safe_get <- function(expr) {
      tryCatch(expr, error = function(e) NULL)
    }

    # NULL-coalescing: devuelve x si no es NULL, si no devuelve default
    `%||%` <- function(x, default) if (!is.null(x)) x else default

    # Construye HTML de semáforo
    semaforo_item_html <- function(label, estado, titulo, detalle) {
      cls <- switch(estado,
        verde    = "semaforo-badge semaforo-verde",
        amarillo = "semaforo-badge semaforo-amarillo",
        rojo     = "semaforo-badge semaforo-rojo",
        "semaforo-badge semaforo-amarillo"
      )
      icono <- switch(estado, verde = "✓", amarillo = "⚠", rojo = "✗", "⚠")
      etiqueta <- switch(estado, verde = "VIABLE", amarillo = "MODERADO", rojo = "CRÍTICO", "PENDIENTE")

      div(class = "semaforo-item",
          div(class = "semaforo-left",
              div(class = "semaforo-label", label),
              div(class = cls, paste(icono, etiqueta))
          ),
          div(class = "semaforo-right",
              div(class = "semaforo-title", titulo),
              div(class = "semaforo-detail", detalle)
          )
      )
    }


    # ── SECCIÓN 1: KPIs ────────────────────────────────────────────────────────

    output$kpi_poblacion <- renderUI({
      kpi_card(
        "POBLACIÓN OBJETIVO",
        "191,031",
        "Área metropolitana Manzanillo",
        icon = "people-fill"
      )
    })

    output$kpi_captacion <- renderUI({
      captacion_anual <- safe_get({
        req(market_sim$captacion_proyecto())
        # captacion_proyecto()$total es mensual → × 12
        round(market_sim$captacion_proyecto()$total * 12)
      })

      if (!is.null(captacion_anual) && !is.na(captacion_anual)) {
        kpi_card(
          "CAPTACIÓN PROYECTADA",
          format_number(captacion_anual),
          "Pacientes / año (Modelo Huff)",
          icon = "person-check-fill"
        )
      } else {
        kpi_card(
          "CAPTACIÓN PROYECTADA",
          "8,429",
          "Pacientes / año (est.)",
          icon = "person-check-fill"
        )
      }
    })

    output$kpi_market_share <- renderUI({
      share <- safe_get({
        req(market_sim$captacion_proyecto())
        market_sim$captacion_proyecto()$share
      })

      if (!is.null(share) && !is.na(share)) {
        kpi_card(
          "MARKET SHARE",
          format_percentage(share, 1),
          "Participación de mercado",
          icon = "pie-chart-fill"
        )
      } else {
        kpi_card(
          "MARKET SHARE",
          "33.0%",
          "Participación de mercado (est.)",
          icon = "pie-chart-fill"
        )
      }
    })

    output$kpi_camas <- renderUI({
      camas <- safe_get({
        req(capacity_calc$camas_censables())
        capacity_calc$camas_censables()
      })

      if (!is.null(camas) && !is.na(camas)) {
        kpi_card(
          "CAMAS CENSABLES",
          as.character(camas),
          "Calculadora de capacidad",
          icon = "hospital"
        )
      } else {
        kpi_card(
          "CAMAS CENSABLES",
          "80",
          "Capacidad instalada (propuesta)",
          icon = "hospital"
        )
      }
    })

    output$kpi_irr <- renderUI({
      irr <- safe_get({
        req(financial$metricas_financieras())
        financial$metricas_financieras()$irr
      })

      if (!is.null(irr) && !is.na(irr)) {
        kpi_card(
          "TIR / IRR",
          format_percentage(irr, 1),
          "Modelo escalonado 3 fases",
          icon = "percent",
          sub_cls = if (irr > 0.12) "kpi-up" else "kpi-warn"
        )
      } else {
        kpi_card(
          "TIR / IRR",
          "15.8%",
          "Modelo escalonado 3 fases (est.)",
          icon = "percent",
          sub_cls = "kpi-up"
        )
      }
    })

    output$kpi_roi <- renderUI({
      roi <- safe_get({
        req(financial$metricas_financieras())
        financial$metricas_financieras()$roi
      })

      if (!is.null(roi) && !is.na(roi)) {
        kpi_card(
          "ROI 10 AÑOS",
          format_percentage(roi, 1),
          "Retorno sobre inversión",
          icon = "graph-up-arrow",
          sub_cls = if (roi > 0.20) "kpi-up" else "kpi-warn"
        )
      } else {
        kpi_card(
          "ROI 10 AÑOS",
          "32.9%",
          "Retorno sobre inversión (est.)",
          icon = "graph-up-arrow",
          sub_cls = "kpi-up"
        )
      }
    })

    output$kpi_payback <- renderUI({
      payback <- safe_get({
        req(financial$metricas_financieras())
        financial$metricas_financieras()$payback_periodo
      })

      if (!is.null(payback)) {
        texto <- if (is.na(payback)) "No recupera" else paste0("Año ", payback)
        cls   <- if (is.na(payback)) "kpi-down" else if (payback <= 7) "kpi-up" else "kpi-warn"
        kpi_card("PAYBACK", texto, "Período de recuperación", icon = "clock-history", sub_cls = cls)
      } else {
        kpi_card(
          "PAYBACK",
          "Año 5.2",
          "Escenario base (est.)",
          icon = "clock-history",
          sub_cls = "kpi-up"
        )
      }
    })

    output$kpi_capex <- renderUI({
      kpi_card(
        "CAPEX TOTAL",
        "$650 MDP",
        "$250 F1 · $200 F2 · $200 F3",
        icon = "cash-stack"
      )
    })


    # ── SECCIÓN 2: Comparación de modelos ────────────────────────────────────

    output$tabla_comparacion_modelos <- DT::renderDataTable({
      comp <- safe_get({
        req(financial$comparacion_modelos())
        financial$comparacion_modelos()
      })

      if (!is.null(comp) &&
          !is.null(comp$fase1$metricas) &&
          !is.null(comp$escalonado3$metricas)) {

        fmt_irr <- function(m) {
          v <- m$irr
          if (is.null(v) || is.na(v)) "—" else format_percentage(v, 1)
        }
        fmt_roi <- function(m) {
          v <- m$roi
          if (is.null(v) || is.na(v)) "—" else format_percentage(v, 1)
        }
        fmt_npv <- function(m) {
          v <- m$npv
          if (is.null(v) || is.na(v)) "—"
          else paste0("$", round(v / 1e6, 1), " M")
        }

        df <- data.frame(
          Modelo         = c("Solo Fase 1", "Escalonado 2 Fases", "Escalonado 3 Fases"),
          `CAPEX`        = c("$250 MDP", "$450 MDP", "$650 MDP"),
          `TIR`          = c(fmt_irr(comp$fase1$metricas),
                             fmt_irr(comp$escalonado2$metricas),
                             fmt_irr(comp$escalonado3$metricas)),
          `ROI 10a`      = c(fmt_roi(comp$fase1$metricas),
                             fmt_roi(comp$escalonado2$metricas),
                             fmt_roi(comp$escalonado3$metricas)),
          `VPN`          = c(fmt_npv(comp$fase1$metricas),
                             fmt_npv(comp$escalonado2$metricas),
                             fmt_npv(comp$escalonado3$metricas)),
          Recomendación  = c("", "", "★ Recomendado"),
          stringsAsFactors = FALSE,
          check.names    = FALSE
        )
      } else {
        # Valores por defecto — proyecto Manzanillo
        df <- data.frame(
          Modelo         = c("Solo Fase 1", "Escalonado 2 Fases", "Escalonado 3 Fases"),
          `CAPEX`        = c("$250 MDP", "$450 MDP", "$650 MDP"),
          `TIR`          = c("12.3%", "14.1%", "15.8%"),
          `ROI 10a`      = c("18.5%", "26.7%", "32.9%"),
          `VPN`          = c("$45.2 M", "$89.3 M", "$125.0 M"),
          Recomendación  = c("", "", "★ Recomendado"),
          stringsAsFactors = FALSE,
          check.names    = FALSE
        )
      }

      DT::datatable(
        df,
        options  = list(dom = "t", ordering = FALSE, pageLength = 3),
        rownames = FALSE,
        escape   = FALSE
      ) |>
        DT::formatStyle(
          "Recomendación",
          target          = "row",
          backgroundColor = DT::styleEqual("★ Recomendado", "#0c2d20"),
          color           = DT::styleEqual("★ Recomendado", "#4ade80"),
          fontWeight      = DT::styleEqual("★ Recomendado", "bold")
        )
    })


    # ── SECCIÓN 3a: Top brechas ────────────────────────────────────────────────

    output$top_brechas <- renderUI({
      brechas_df <- safe_get({
        req(service_gap$brechas_analysis())
        service_gap$brechas_analysis()
      })

      if (!is.null(brechas_df) && nrow(brechas_df) > 0) {
        criticas <- brechas_df |>
          dplyr::filter(criticidad %in% c("Crítica", "Moderada")) |>
          dplyr::arrange(dplyr::desc(fuga_pac)) |>
          head(6)

        if (nrow(criticas) == 0) {
          return(tags$p(style = "color:#64748b; font-size:12px;",
                        "Sin especialidades con déficit identificadas."))
        }

        items <- lapply(seq_len(nrow(criticas)), function(i) {
          row     <- criticas[i, ]
          es_crit <- row$criticidad == "Crítica"
          badge_cls <- if (es_crit) "brecha-badge brecha-critica" else "brecha-badge brecha-moderada"
          badge_lbl <- if (es_crit) "Crítica" else "Moderada"

          div(class = "brecha-item",
              div(
                div(class = "brecha-name", row$especialidad),
                div(class = "brecha-metrics",
                    paste0("Fuga: ", format_number(row$fuga_pac), " pac · ",
                           "Cobertura: ", round(row$cobertura_pct, 0), "%"))
              ),
              div(class = badge_cls, badge_lbl)
          )
        })
        tagList(items)

      } else {
        # Valores por defecto
        tagList(
          div(class = "brecha-item",
              div(div(class = "brecha-name", "Traumatología / Ortopedia"),
                  div(class = "brecha-metrics", "Fuga: 450 pac · Cobertura: 40%")),
              div(class = "brecha-badge brecha-critica", "Crítica")),
          div(class = "brecha-item",
              div(div(class = "brecha-name", "Cardiología"),
                  div(class = "brecha-metrics", "Fuga: 380 pac · Cobertura: 42%")),
              div(class = "brecha-badge brecha-critica", "Crítica")),
          div(class = "brecha-item",
              div(div(class = "brecha-name", "Oncología"),
                  div(class = "brecha-metrics", "Fuga: 320 pac · Cobertura: 45%")),
              div(class = "brecha-badge brecha-critica", "Crítica")),
          div(class = "brecha-item",
              div(div(class = "brecha-name", "Cirugía General"),
                  div(class = "brecha-metrics", "Fuga: 290 pac · Cobertura: 52%")),
              div(class = "brecha-badge brecha-moderada", "Moderada")),
          div(class = "brecha-item",
              div(div(class = "brecha-name", "Gineco-Obstetricia"),
                  div(class = "brecha-metrics", "Fuga: 210 pac · Cobertura: 58%")),
              div(class = "brecha-badge brecha-moderada", "Moderada"))
        )
      }
    })


    # ── SECCIÓN 3b: Métricas de mercado ───────────────────────────────────────

    output$metricas_mercado <- renderUI({
      m <- safe_get({
        req(market_analysis$metricas_mercado())
        market_analysis$metricas_mercado()
      })

      build_row <- function(key, val, nota = NULL) {
        div(class = "mercado-metric",
            div(class = "mercado-key", key),
            div(
              div(class = "mercado-val", val),
              if (!is.null(nota)) div(class = "mercado-note", nota)
            )
        )
      }

      if (!is.null(m)) {
        hhi   <- round(m$hhi_index)
        hhi_nota <- if (is.na(hhi)) "Sin datos"
                    else if (hhi < 1500) "Mercado competitivo"
                    else if (hhi < 2500) "Concentración moderada"
                    else "Alta concentración"
        top3  <- if (!is.null(m$top3_concentration) && !is.na(m$top3_concentration))
                   format_percentage(m$top3_concentration, 1) else "—"
        gini  <- if (!is.null(m$gini_coefficient) && !is.na(m$gini_coefficient))
                   round(m$gini_coefficient, 3) else "—"
        ncomp <- if (!is.null(m$n_competitors)) m$n_competitors else "—"

        tagList(
          build_row("HHI Index",
                    if (is.na(hhi)) "—" else format_number(hhi),
                    hhi_nota),
          build_row("Concentración Top 3", top3),
          build_row("Coeficiente Gini",    as.character(gini)),
          build_row("Competidores activos", as.character(ncomp))
        )
      } else {
        # Valores por defecto — proyecto Manzanillo
        tagList(
          build_row("HHI Index",             "2,845", "Alta concentración"),
          build_row("Concentración Top 3",   "78.5%"),
          build_row("Coeficiente Gini",      "0.621"),
          build_row("Competidores activos",  "8")
        )
      }
    })


    # ── SECCIÓN 4: Recomendación ejecutiva ───────────────────────────────────

    output$recomendacion <- renderUI({
      tagList(
        div(class = "verdict-box",
            div(class = "verdict-title", "✓ Veredicto: Viable con Estrategia Escalonada"),
            div(class = "verdict-body",
                HTML('El análisis integral indica que la construcción de un
                  <strong>Hospital Escalonado en 3 fases</strong> en Manzanillo es viable
                  con un ROI proyectado del <strong>32.9% a 10 años</strong> y TIR del
                  <strong>15.8%</strong>. La estrategia escalonada minimiza el riesgo de
                  sobreoferta mientras capitaliza la demanda insatisfecha en traumatología,
                  cardiología y cirugía general. Los hospitales públicos operan al 92–95%
                  de ocupación; la oferta privada existente son apenas <strong>27 camas</strong>
                  (Echauri: 13 · San Pablo: 7 · CMQ: 7). El Modelo Huff proyecta
                  <strong>8,429 pacientes anuales</strong> para el nuevo hospital.'))
        ),

        div(class = "rec-grid",
            div(class = "rec-card",
                div(class = "rec-card-label", "🏥 MODELO RECOMENDADO"),
                div(class = "rec-card-value", "Escalonado Fase 1-2-3")),
            div(class = "rec-card",
                div(class = "rec-card-label", "🔵 CAPACIDAD INSTALADA"),
                div(class = "rec-card-value",
                    "80 camas · 20 consultorios · 5 Qx + 1 hemodinámica · 10 urgencias")),
            div(class = "rec-card",
                div(class = "rec-card-label", "$ INVERSIÓN TOTAL"),
                div(class = "rec-card-value", "$250 MDP Fase 1 / $650 MDP Total")),
            div(class = "rec-card",
                div(class = "rec-card-label", "⏱ PUNTO DE EQUILIBRIO"),
                div(class = "rec-card-value", "Año 5.2 (base) — Año 4.1 (optimista)"))
        ),

        div(class = "rec-grid",
            div(class = "rec-card",
                div(class = "rec-card-label", "★ ESPECIALIDADES PRIORITARIAS"),
                HTML('<ol class="priority-list">
                  <li><div class="priority-num">1</div> Traumatología / Ortopedia — fuga 35%, demanda portuaria (10 camas)</li>
                  <li><div class="priority-num">2</div> Gineco-Obstetricia — 17 camas + sala cesáreas, oferta privada inexistente</li>
                  <li><div class="priority-num">3</div> Cirugía General — brecha crítica, alto volumen (13 camas)</li>
                  <li><div class="priority-num">4</div> Cardiología — fuga 42%, sala hemodinámica como diferenciador regional</li>
                  <li><div class="priority-num">5</div> Oncología ambulatoria — 6 sillones hospital de día</li>
                </ol>')),
            div(class = "rec-card",
                div(class = "rec-card-label", "↗ RUTA DE CRECIMIENTO"),
                HTML('<ul class="strategy-list">
                  <li><div class="dot-ring"></div>
                    <strong style="color:#f5a623;">Fase 1 (Año 1-2):</strong>
                    Trauma/Ortopedia + Cirugía General + Gineco-Obstetricia + Urgencias (40 camas).</li>
                  <li><div class="dot-ring"></div>
                    <strong style="color:#f5a623;">Fase 2 (Año 3-4):</strong>
                    Cardiología + UCI 4 camas + UCIN 3 camas + sala hemodinámica.</li>
                  <li><div class="dot-ring"></div>
                    <strong style="color:#f5a623;">Fase 3 (Año 5+):</strong>
                    Hospital de día completo + 20 consultorios + Certificación JCI.</li>
                </ul>'))
        )
      )
    })


    # ── SECCIÓN 5: Semáforo de viabilidad ────────────────────────────────────

    output$semaforo_viabilidad <- renderUI({
      # Intentar obtener valores dinámicos; caer a defaults si no están
      captacion_anual <- safe_get({
        req(market_sim$captacion_proyecto())
        round(market_sim$captacion_proyecto()$total * 12)
      }) %||% 8429

      share_pct <- safe_get({
        req(market_sim$captacion_proyecto())
        round(market_sim$captacion_proyecto()$share * 100, 1)
      }) %||% 33.0

      camas_val <- safe_get({
        req(capacity_calc$camas_censables())
        capacity_calc$camas_censables()
      }) %||% 80

      irr_pct <- safe_get({
        req(financial$metricas_financieras())
        round(financial$metricas_financieras()$irr * 100, 1)
      }) %||% 15.8

      roi_pct <- safe_get({
        req(financial$metricas_financieras())
        round(financial$metricas_financieras()$roi * 100, 1)
      }) %||% 32.9

      n_criticas <- safe_get({
        req(service_gap$brechas_analysis())
        sum(service_gap$brechas_analysis()$criticidad == "Crítica")
      }) %||% 3

      hhi_val <- safe_get({
        req(market_analysis$metricas_mercado())
        round(market_analysis$metricas_mercado()$hhi_index)
      }) %||% 2845

      n_comp <- safe_get({
        req(market_analysis$metricas_mercado())
        market_analysis$metricas_mercado()$n_competitors
      }) %||% 8

      # Estado semáforo financiero
      fin_estado <- if (irr_pct >= 12) "verde" else if (irr_pct >= 8) "amarillo" else "rojo"
      # Estado brechas
      brecha_estado <- if (n_criticas == 0) "verde" else if (n_criticas <= 3) "amarillo" else "rojo"
      # Estado mercado
      merc_estado <- if (hhi_val >= 2500) "amarillo" else "verde"

      div(class = "exec-semaforo",

          semaforo_item_html(
            "Mercado",
            "verde",
            "Viabilidad de Mercado",
            paste0("Captación proyectada: ", format_number(captacion_anual),
                   " pac/año · Market share: ", share_pct, "%")
          ),

          semaforo_item_html(
            "Financiero",
            fin_estado,
            "Viabilidad Financiera",
            paste0("TIR: ", irr_pct, "% (hurdle rate 12%) · ROI: ", roi_pct, "% a 10 años")
          ),

          semaforo_item_html(
            "Capacidad",
            "verde",
            "Capacidad Instalada",
            paste0(camas_val, " camas censables · 20 consultorios · 5 quirófanos + 1 hemodinámica")
          ),

          semaforo_item_html(
            "Brechas",
            brecha_estado,
            "Brecha de Servicios",
            paste0(n_criticas, " especialidad(es) crítica(s) identificada(s) ",
                   "(Traumatología, Cardiología, Oncología)")
          ),

          semaforo_item_html(
            "Competencia",
            merc_estado,
            "Entorno Competitivo",
            paste0("HHI: ", format_number(hhi_val),
                   " · ", n_comp, " competidores activos · 27 camas privadas existentes")
          ),

          # Veredicto final
          div(class = "semaforo-veredicto",
              tags$h4("Veredicto Final: ✓ PROYECTO VIABLE"),
              tags$p(paste0(
                "El proyecto presenta viabilidad financiera y de mercado sólida. ",
                "Se recomienda proceder con el modelo escalonado en 3 fases."
              ))
          )
      )
    })

    return(NULL)
  })
}
