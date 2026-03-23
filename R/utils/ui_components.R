# R/utils/ui_components.R
# Componentes UI reutilizables para el Dashboard Geoanalitico
# Extraído de app_canibalizacion_lovable_3.R
# ══════════════════════════════════════════════════════════════════════════════

#' KPI Card Component
#'
#' Genera una tarjeta de indicador clave de rendimiento (KPI) como HTML.
#' Los valores numéricos se formatean automáticamente con separador de miles.
#'
#' @param label Character. Etiqueta superior de la tarjeta.
#' @param value Character o numeric. Valor principal a mostrar. Los numéricos
#'   se formatean con \code{scales::comma()}.
#' @param sub Character o NULL. Subtítulo opcional.
#' @param sub_cls Character. Clase CSS adicional para el subtítulo. Default: "".
#' @param icon Character o NULL. Nombre de icono de \code{bsicons} (sin prefijo
#'   "bi-"). Si se proporciona, se renderiza junto al valor. Default: NULL.
#'
#' @return Un objeto \code{shiny::HTML} con la tarjeta renderizada.
#' @export
#'
#' @examples
#' kpi_card("Población", 191031)
#' kpi_card("Ocupación", "87%", sub = "+3 pp vs año anterior", sub_cls = "kpi-up")
#' kpi_card("Camas", 120, icon = "hospital")
kpi_card <- function(label, value, sub = NULL, sub_cls = "", icon = NULL) {
  # Formatear numéricos con separador de miles
  if (is.numeric(value)) {
    value <- scales::comma(value)
  }

  sub_html <- if (!is.null(sub)) {
    sprintf('<div class="kpi-sub %s">%s</div>', sub_cls, sub)
  } else {
    ""
  }

  icon_html <- if (!is.null(icon)) {
    sprintf('<span class="kpi-icon">%s</span>',
            as.character(bsicons::bs_icon(icon)))
  } else {
    ""
  }

  shiny::HTML(sprintf(
    '<div class="kpi-card">
    <div class="kpi-label">%s</div>
    <div class="kpi-value">%s%s</div>%s
  </div>',
    label, icon_html, value, sub_html
  ))
}


#' Benchmark Card Component
#'
#' Genera una tarjeta de benchmark/comparación como HTML.
#' Los valores numéricos se formatean automáticamente con separador de miles.
#'
#' @param value Character o numeric. Valor principal a mostrar. Los numéricos
#'   se formatean con \code{scales::comma()}.
#' @param label Character. Etiqueta descriptiva debajo del valor.
#' @param sub Character o NULL. Subtítulo opcional.
#' @param color Character. Color CSS para el valor principal. Default: "#e2e8f0".
#'
#' @return Un objeto \code{shiny::HTML} con la tarjeta renderizada.
#' @export
#'
#' @examples
#' bench_card("4.2", "Camas / 1,000 hab.", color = "#38bdf8")
#' bench_card(3.8, "Promedio nacional", sub = "OCDE 2023", color = "#94a3b8")
bench_card <- function(value, label, sub = NULL, color = "#e2e8f0") {
  if (is.numeric(value)) {
    value <- scales::comma(value)
  }

  sub_html <- if (!is.null(sub)) {
    sprintf('<div class="bsub">%s</div>', sub)
  } else {
    ""
  }

  shiny::HTML(sprintf(
    '<div class="bench-card">
    <div class="bval" style="color:%s;">%s</div>
    <div class="blbl">%s</div>%s
  </div>',
    color, value, label, sub_html
  ))
}


#' Page Header Component
#'
#' Genera un encabezado de sección de página con badge, título y subtítulo,
#' usando la estructura CSS estándar del dashboard.
#'
#' @param badge Character. Texto del badge/etiqueta de sección (ej. "ANÁLISIS").
#' @param title Character. Título principal de la sección.
#' @param sub Character. Subtítulo o descripción breve de la sección.
#'
#' @return Un objeto \code{shiny::tag} (div) con el encabezado renderizado.
#' @export
#'
#' @examples
#' page_header("DEMANDA", "Análisis de Demanda Hospitalaria",
#'             "Proyecciones 2025-2030 para el área metropolitana")
page_header <- function(badge, title, sub) {
  shiny::div(
    class = "page-section",
    shiny::div(class = "section-badge", badge),
    shiny::div(class = "section-title", title),
    shiny::div(class = "section-sub",   sub)
  )
}


#' Page Header (alias de compatibilidad)
#'
#' Alias de \code{\link{page_header}} para compatibilidad con código existente
#' que usa el nombre \code{page_hdr}.
#'
#' @inheritParams page_header
#' @return Ver \code{\link{page_header}}.
#' @export
page_hdr <- page_header
