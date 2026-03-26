# ══════════════════════════════════════════════════════════════════════════════
# R/models/financial_model.R
# Modelo Financiero — Hospital Manzanillo
#
# Funciones puras (sin dependencias Shiny) para:
#   - calcular_irr                  : TIR por método Newton-Raphson
#   - proyectar_flujos_caja         : Proyecciones a N años
#   - calcular_metricas_financieras : VPN, ROI, payback, margen
#   - analisis_sensibilidad         : Variación de parámetros clave
#   - validate_financial_inputs     : Validación de entradas (auxiliar)
#
# Fuente original: app_canibalizacion_lovable_3.R (líneas 247-257, 1749-1849)
# ══════════════════════════════════════════════════════════════════════════════


# ── 1. calcular_irr ────────────────────────────────────────────────────────────

#' Calculate Internal Rate of Return (IRR/TIR)
#'
#' Calculates the discount rate that makes NPV = 0 using Newton-Raphson method.
#' Falls back to bisection when Newton-Raphson fails to converge.
#'
#' @param flujos numeric vector. Cash flows where flujos[1] is the initial
#'   investment (should be negative) and flujos[2:n] are periodic returns.
#' @param tasa_inicial numeric. Initial guess for IRR (default: 0.1 = 10%).
#' @param max_iter integer. Maximum iterations for convergence (default: 1000).
#' @param tolerancia numeric. Convergence tolerance (default: 1e-6).
#'
#' @return numeric. IRR as decimal (e.g., 0.158 = 15.8%).
#'   Returns \code{NA_real_} if no convergence is achieved.
#'
#' @examples
#' flujos <- c(-1000, 200, 300, 400, 500)
#' irr <- calcular_irr(flujos)
#' print(paste0("TIR: ", round(irr * 100, 2), "%"))
#'
#' @export
calcular_irr <- function(flujos, tasa_inicial = 0.1, max_iter = 1000, tolerancia = 1e-6) {

  # Validación
  if (!is.numeric(flujos))    stop("flujos must be numeric")
  if (length(flujos) < 2)     stop("flujos must have at least 2 values")
  if (anyNA(flujos))          return(NA_real_)
  if (flujos[1] >= 0) warning("First flow should be negative (initial investment)")

  periodos <- seq_along(flujos) - 1

  # VPN y su derivada respecto a r
  npv_fn  <- function(r) sum(flujos / (1 + r)^periodos)
  dnpv_fn <- function(r) -sum(periodos * flujos / (1 + r)^(periodos + 1))

  # ── Newton-Raphson ────────────────────────────────────────────────────────
  r <- tasa_inicial
  for (i in seq_len(max_iter)) {
    npv  <- npv_fn(r)
    dnpv <- dnpv_fn(r)
    if (is.na(npv) || is.na(dnpv) || dnpv == 0) break
    r_new <- r - npv / dnpv
    if (abs(r_new - r) < tolerancia) return(r_new)
    r <- r_new
  }

  # ── Fallback: bisección (robusto ante no-convergencia) ───────────────────
  r_min <- -0.9999
  r_max <- 10.0
  if (is.na(npv_fn(r_min)) || is.na(npv_fn(r_max))) return(NA_real_)
  if (npv_fn(r_min) * npv_fn(r_max) > 0)            return(NA_real_)

  for (i in seq_len(max_iter)) {
    r_mid <- (r_min + r_max) / 2
    if (abs(r_max - r_min) < tolerancia) return(r_mid)
    if (npv_fn(r_min) * npv_fn(r_mid) < 0) r_max <- r_mid else r_min <- r_mid
  }

  (r_min + r_max) / 2
}


# ── 2. validate_financial_inputs ──────────────────────────────────────────────

#' Validate financial model inputs
#'
#' @param capex_fase1 numeric. Phase 1 CAPEX.
#' @param capex_fase2 numeric. Phase 2 CAPEX.
#' @param capex_fase3 numeric. Phase 3 CAPEX.
#' @param año_fase2 integer or NULL. Year when phase 2 starts.
#' @param año_fase3 integer or NULL. Year when phase 3 starts.
#' @param años integer. Projection horizon.
#'
#' @return Invisible NULL. Stops with an error message if validation fails.
#'
#' @keywords internal
validate_financial_inputs <- function(capex_fase1, capex_fase2, capex_fase3,
                                      año_fase2, año_fase3, años) {
  if (capex_fase1 <= 0) stop("capex_fase1 must be positive")
  if (capex_fase2 <  0) stop("capex_fase2 cannot be negative")
  if (capex_fase3 <  0) stop("capex_fase3 cannot be negative")
  if (años < 1)         stop("años must be at least 1")

  if (!is.null(año_fase2)) {
    if (!is.numeric(año_fase2) || año_fase2 < 2 || año_fase2 > años) {
      stop(paste("año_fase2 must be between 2 and", años))
    }
  }

  if (!is.null(año_fase3)) {
    if (is.null(año_fase2)) {
      stop("año_fase2 must be specified before año_fase3")
    }
    if (!is.numeric(año_fase3) || año_fase3 <= año_fase2 || año_fase3 > años) {
      stop(paste("año_fase3 must be after año_fase2 and at most", años))
    }
  }

  invisible(NULL)
}


# ── 3. proyectar_flujos_caja ──────────────────────────────────────────────────

#' Project Cash Flows for Hospital Feasibility
#'
#' Projects revenues, expenses, and net cash flows over a specified horizon,
#' supporting up to three investment phases with independent CAPEX events.
#'
#' @param capex_fase1 numeric. Phase 1 CAPEX investment (positive, MXN or MDP).
#' @param capex_fase2 numeric. Phase 2 CAPEX investment (default: 0).
#' @param capex_fase3 numeric. Phase 3 CAPEX investment (default: 0).
#' @param año_fase2 integer or NULL. Year when phase 2 CAPEX is disbursed
#'   (default: NULL = no phase 2).
#' @param año_fase3 integer or NULL. Year when phase 3 CAPEX is disbursed
#'   (default: NULL = no phase 3).
#' @param captacion_anual numeric. Annual patient volume at full capacity.
#' @param tarifa_promedio numeric. Average revenue per patient (same units as
#'   CAPEX).
#' @param tasa_ocupacion numeric. Occupancy / utilisation rate in [0, 1]
#'   (default: 0.85).
#' @param costo_variable_paciente numeric. Variable cost per patient (same
#'   units as tarifa_promedio).
#' @param costos_fijos_anuales numeric. Annual fixed operating costs.
#' @param tasa_crecimiento numeric. Annual nominal revenue growth rate
#'   (default: 0.03 = 3\%).
#' @param años integer. Projection horizon in years (default: 10).
#'
#' @return \code{data.frame} with columns:
#'   \describe{
#'     \item{año}{Year number (1 … años).}
#'     \item{ingresos}{Total revenues for the year.}
#'     \item{egresos}{Total expenses for the year (including any CAPEX event).}
#'     \item{flujo_neto}{Net cash flow (ingresos − egresos).}
#'     \item{flujo_acumulado}{Cumulative cash flow including year-0 CAPEX.}
#'     \item{fase}{Active investment phase ("Fase 1", "Fase 2", "Fase 3").}
#'   }
#'
#' @examples
#' df <- proyectar_flujos_caja(
#'   capex_fase1            = 250,
#'   captacion_anual        = 8429,
#'   tarifa_promedio        = 3500,
#'   tasa_ocupacion         = 0.85,
#'   costo_variable_paciente = 1500,
#'   costos_fijos_anuales   = 12
#' )
#' head(df)
#'
#' @export
proyectar_flujos_caja <- function(
    capex_fase1,
    capex_fase2             = 0,
    capex_fase3             = 0,
    año_fase2               = NULL,
    año_fase3               = NULL,
    captacion_anual,
    tarifa_promedio,
    tasa_ocupacion          = 0.85,
    costo_variable_paciente,
    costos_fijos_anuales,
    tasa_crecimiento        = 0.03,
    años                    = 10
) {
  validate_financial_inputs(capex_fase1, capex_fase2, capex_fase3,
                             año_fase2, año_fase3, años)

  resultado <- data.frame(
    año             = seq_len(años),
    ingresos        = numeric(años),
    egresos         = numeric(años),
    flujo_neto      = numeric(años),
    flujo_acumulado = numeric(años),
    fase            = character(años),
    stringsAsFactors = FALSE
  )

  for (i in seq_len(años)) {

    # Fase activa
    fase_actual <- "Fase 1"
    if (!is.null(año_fase2) && i >= año_fase2) fase_actual <- "Fase 2"
    if (!is.null(año_fase3) && i >= año_fase3) fase_actual <- "Fase 3"
    resultado$fase[i] <- fase_actual

    # CAPEX adicional en el año de arranque de cada fase
    capex_año <- 0
    if (!is.null(año_fase2) && i == año_fase2) capex_año <- capex_fase2
    if (!is.null(año_fase3) && i == año_fase3) capex_año <- capex_fase3

    # Ingresos con crecimiento compuesto
    ingresos_base        <- captacion_anual * tarifa_promedio * tasa_ocupacion
    resultado$ingresos[i] <- ingresos_base * (1 + tasa_crecimiento)^(i - 1)

    # Egresos operativos + CAPEX del año
    costos_variables      <- captacion_anual * costo_variable_paciente * tasa_ocupacion
    resultado$egresos[i]  <- costos_variables + costos_fijos_anuales + capex_año

    # Flujo neto del año
    resultado$flujo_neto[i] <- resultado$ingresos[i] - resultado$egresos[i]

    # Flujo acumulado (año 0: −capex_fase1)
    acum_anterior <- if (i == 1) -capex_fase1 else resultado$flujo_acumulado[i - 1]
    resultado$flujo_acumulado[i] <- acum_anterior + resultado$flujo_neto[i]
  }

  resultado
}


# ── 4. calcular_metricas_financieras ─────────────────────────────────────────

#' Calculate Financial Metrics from Cash Flow Projections
#'
#' Derives the core investment metrics (IRR, NPV, ROI, payback, margin) from
#' the output of \code{\link{proyectar_flujos_caja}}.
#'
#' @param flujos_df \code{data.frame} returned by \code{proyectar_flujos_caja}.
#'   Must contain columns \code{flujo_neto}, \code{flujo_acumulado},
#'   \code{ingresos}, and \code{egresos}.
#' @param capex_inicial numeric. Initial CAPEX as a \strong{negative} value
#'   (e.g., \code{-250} for 250 MDP).
#' @param tasa_descuento numeric. Annual discount rate for NPV
#'   (default: 0.12 = 12\%).
#'
#' @return Named \code{list} with:
#'   \describe{
#'     \item{irr}{Internal Rate of Return (decimal).}
#'     \item{npv}{Net Present Value at \code{tasa_descuento}.}
#'     \item{roi}{Simple Return on Investment at the final year (ratio).}
#'     \item{payback_periodo}{First year in which cumulative cash flow turns
#'       positive. \code{NA} if investment is not recovered within the horizon.}
#'     \item{margen_promedio}{Average annual operating profit margin.}
#'   }
#'
#' @examples
#' df  <- proyectar_flujos_caja(250, captacion_anual = 8429,
#'          tarifa_promedio = 3500, tasa_ocupacion = 0.85,
#'          costo_variable_paciente = 1500, costos_fijos_anuales = 12)
#' met <- calcular_metricas_financieras(df, capex_inicial = -250)
#' cat("TIR:", round(met$irr * 100, 1), "%\n")
#' cat("VPN:", round(met$npv, 1), "MDP\n")
#'
#' @export
calcular_metricas_financieras <- function(flujos_df, capex_inicial,
                                          tasa_descuento = 0.12) {
  if (!is.data.frame(flujos_df))       stop("flujos_df must be a data.frame")
  if (!("flujo_neto" %in% names(flujos_df))) {
    stop("flujos_df must contain a 'flujo_neto' column")
  }
  if (capex_inicial >= 0) warning("capex_inicial should be negative")

  n      <- nrow(flujos_df)
  flujos <- c(capex_inicial, flujos_df$flujo_neto)

  # TIR
  irr <- calcular_irr(flujos)

  # VPN
  npv <- sum(flujos / (1 + tasa_descuento)^(0:n))

  # ROI simple al año final
  inversion_total <- abs(capex_inicial) + sum(pmax(-flujos_df$flujo_neto, 0))
  retorno_total   <- flujos_df$flujo_acumulado[n] + inversion_total
  roi             <- if (inversion_total > 0) retorno_total / inversion_total else NA_real_

  # Período de payback (primer año con flujo_acumulado > 0)
  payback_periodo <- {
    idx <- which(flujos_df$flujo_acumulado > 0)[1]
    if (is.na(idx)) NA_integer_ else idx
  }

  # Margen operativo promedio
  margen_promedio <- mean(
    (flujos_df$ingresos - flujos_df$egresos) / flujos_df$ingresos,
    na.rm = TRUE
  )

  list(
    irr             = irr,
    npv             = npv,
    roi             = roi,
    payback_periodo = payback_periodo,
    margen_promedio = margen_promedio
  )
}


# ── 5. analisis_sensibilidad ──────────────────────────────────────────────────

#' Sensitivity Analysis for Key Financial Parameters
#'
#' Systematically varies one parameter of \code{proyectar_flujos_caja} across
#' a range of multipliers and returns the resulting IRR and NPV for each value.
#'
#' @param params_base named \code{list}. Base-case parameters passed directly
#'   to \code{proyectar_flujos_caja} (e.g.,
#'   \code{list(capex_fase1 = 250, captacion_anual = 8429, ...)}).
#' @param variable character. Name of the parameter to vary; must be a key in
#'   \code{params_base} (e.g., \code{"captacion_anual"}, \code{"tarifa_promedio"}).
#' @param rango numeric vector. Multipliers applied to the base value
#'   (default: \code{seq(0.7, 1.3, 0.1)}).
#' @param tasa_descuento numeric. Discount rate forwarded to
#'   \code{calcular_metricas_financieras} (default: 0.12).
#'
#' @return \code{data.frame} with one row per multiplier and columns:
#'   \describe{
#'     \item{variable}{Name of the varied parameter.}
#'     \item{multiplicador}{Multiplier applied.}
#'     \item{valor}{Resulting parameter value.}
#'     \item{irr}{IRR (decimal) for this scenario.}
#'     \item{npv}{NPV for this scenario.}
#'   }
#'
#' @examples
#' params <- list(
#'   capex_fase1             = 250,
#'   captacion_anual         = 8429,
#'   tarifa_promedio         = 3500,
#'   tasa_ocupacion          = 0.85,
#'   costo_variable_paciente = 1500,
#'   costos_fijos_anuales    = 12
#' )
#' sens <- analisis_sensibilidad(params, variable = "tarifa_promedio")
#' print(sens)
#'
#' @export
analisis_sensibilidad <- function(params_base, variable,
                                  rango           = seq(0.7, 1.3, 0.1),
                                  tasa_descuento  = 0.12) {
  if (!is.list(params_base))          stop("params_base must be a list")
  if (!(variable %in% names(params_base))) {
    stop(paste0("'", variable, "' not found in params_base"))
  }
  if (!is.numeric(rango) || length(rango) == 0) {
    stop("rango must be a non-empty numeric vector")
  }

  resultados <- data.frame(
    variable     = variable,
    multiplicador = rango,
    valor        = numeric(length(rango)),
    irr          = numeric(length(rango)),
    npv          = numeric(length(rango)),
    stringsAsFactors = FALSE
  )

  for (i in seq_along(rango)) {
    params             <- params_base
    params[[variable]] <- params_base[[variable]] * rango[i]
    resultados$valor[i] <- params[[variable]]

    flujos_df <- tryCatch(
      do.call(proyectar_flujos_caja, params),
      error = function(e) NULL
    )

    if (is.null(flujos_df)) {
      resultados$irr[i] <- NA_real_
      resultados$npv[i] <- NA_real_
      next
    }

    metricas <- calcular_metricas_financieras(
      flujos_df,
      capex_inicial  = -params$capex_fase1,
      tasa_descuento = tasa_descuento
    )
    resultados$irr[i] <- metricas$irr
    resultados$npv[i] <- metricas$npv
  }

  resultados
}
