# ══════════════════════════════════════════════════════════════════════════════
# R/models/capacity_model.R
# Modelo de Capacidad Hospitalaria — Hospital Manzanillo
#
# Funciones puras (sin dependencias Shiny) para:
#   - calcular_capacidad_hospitalaria   : Camas y complementos (Hill-Burton)
#   - calcular_infraestructura_complementaria : Consultorios, QX, UCI, etc.
#   - comparar_con_estandares           : Benchmarking vs OMS / OCDE / MX
#   - calcular_desarrollo_por_fases     : Distribución en fases del proyecto
#   - validate_capacity_inputs          : Validación de entradas (auxiliar)
#
# Fuente original: app_canibalizacion_lovable_3.R (líneas 41-71, 1232-1281)
# Estándares base:
#   - Hill-Burton Act (USA 1946) adaptado a contexto mexicano
#   - OMS: 2.5 camas/1,000 hab  (meta_oms_camas)
#   - OCDE: 3.4 camas/1,000 hab
#   - NOM-016-SSA3-2012 (México): proporciones complementarias
# ══════════════════════════════════════════════════════════════════════════════


# ── Constantes de referencia (del diseño físico y calibración del app) ────────

# Diseño físico del hospital (imagen de diseño original)
HOSP_CAMAS_CENSABLES  <- 80L
HOSP_CONSULTORIOS     <- 20L
HOSP_QUIROFANOS       <- 5L    # + 1 sala hemodinámica fuera del conteo
HOSP_UCI              <- 4L    # UCI adultos
HOSP_UCIN             <- 3L    # Terapia neonatal
HOSP_CAMAS_URGENCIAS  <- 10L
HOSP_SILLONES_DIA     <- 6L

# Captación Huff proyectada (base para factores de calibración)
PROY_PACS_ANUALES     <- 8429L

# Factores de calibración derivados del diseño real
# (anclan el modelo teórico al diseño físico aprobado)
.EGRESOS_BASE_CAL  <- round(PROY_PACS_ANUALES * 100 / 1000)           # ~843
.CALC_CAMAS_CRUDO  <- ceiling(.EGRESOS_BASE_CAL * 3.5 / (365 * 0.85)) # teórico
.CALC_QX_CRUDO     <- max(ceiling(.EGRESOS_BASE_CAL * 0.40 / (4 * 365)), 1L)

FACTOR_CAMAS       <- HOSP_CAMAS_CENSABLES / .CALC_CAMAS_CRUDO  # ≈ 0.833
FACTOR_QX          <- HOSP_QUIROFANOS      / .CALC_QX_CRUDO     # ≈ 1.667
RATIO_CONS_CAMA    <- 4L   # 1 consultorio cada 4 camas (estándar industria MX)
FACTOR_CONS        <- (HOSP_CONSULTORIOS * 3 * 16 * 300) /
                      (PROY_PACS_ANUALES * 2.5)

# Estándares internacionales (camas por 1 000 habitantes)
META_OMS_CAMAS_POR_MIL  <- 2.5
META_OCDE_CAMAS_POR_MIL <- 3.4
META_MX_CAMAS_POR_MIL   <- 1.5


# ── 1. validate_capacity_inputs ───────────────────────────────────────────────

#' Validate inputs for capacity calculation
#'
#' @param poblacion numeric. Target population.
#' @param tasa_egresos numeric. Discharge rate (0–1).
#' @param dias_estancia_promedio numeric. Average length of stay (days).
#' @param tasa_ocupacion_objetivo numeric. Target occupancy rate (0–1).
#' @param k_calibracion numeric. Calibration factor (> 0).
#' @param mix_especialidades list. Specialty bed mix (must sum to 1).
#'
#' @return Invisibly returns NULL. Throws an error on invalid input.
#'
#' @keywords internal
validate_capacity_inputs <- function(poblacion,
                                     tasa_egresos,
                                     dias_estancia_promedio,
                                     tasa_ocupacion_objetivo,
                                     k_calibracion,
                                     mix_especialidades) {

  if (!is.numeric(poblacion) || length(poblacion) != 1L || poblacion <= 0) {
    stop("poblacion must be a positive number")
  }

  if (!is.numeric(tasa_egresos) || tasa_egresos <= 0 || tasa_egresos > 1) {
    stop("tasa_egresos must be between 0 and 1 (e.g., 0.08 for 8%)")
  }

  if (!is.numeric(dias_estancia_promedio) ||
      dias_estancia_promedio <= 0 || dias_estancia_promedio > 30) {
    stop("dias_estancia_promedio must be between 0 and 30 days")
  }

  if (!is.numeric(tasa_ocupacion_objetivo) ||
      tasa_ocupacion_objetivo <= 0 || tasa_ocupacion_objetivo > 1) {
    stop("tasa_ocupacion_objetivo must be between 0 and 1 (e.g., 0.85 for 85%)")
  }

  if (!is.numeric(k_calibracion) || k_calibracion <= 0) {
    stop("k_calibracion must be a positive number")
  }

  if (!is.list(mix_especialidades)) {
    stop("mix_especialidades must be a list")
  }

  suma_mix <- sum(unlist(mix_especialidades))
  if (abs(suma_mix - 1.0) > 0.01) {
    stop("mix_especialidades proportions must sum to 1.0 (currently: ",
         round(suma_mix, 3), ")")
  }

  invisible(NULL)
}


# ── 2. calcular_infraestructura_complementaria ────────────────────────────────

#' Calculate Complementary Hospital Infrastructure
#'
#' Calculates outpatient clinics, operating rooms, ICU beds, NICU beds,
#' emergency beds, and day-hospital chairs based on inpatient bed count,
#' using NOM-016-SSA3-2012 and industry ratios for the Mexican private sector.
#'
#' @param camas_censables numeric. Number of inpatient beds.
#' @param poblacion numeric. Target population (reserved for future use).
#'
#' @return list with:
#'   \describe{
#'     \item{consultorios}{Outpatient clinics (1 per 4 beds)}
#'     \item{quirofanos}{Operating rooms (1 per 18 beds)}
#'     \item{camas_uci}{Adult ICU beds (9\% of inpatient beds)}
#'     \item{camas_ucin}{NICU beds (4.5\% of inpatient beds)}
#'     \item{camas_urgencias}{Emergency beds (13\% of inpatient beds)}
#'     \item{sillones_hospital_dia}{Day-hospital chairs (7\% of inpatient beds)}
#'   }
#'
#' @keywords internal
calcular_infraestructura_complementaria <- function(camas_censables, poblacion) {

  # Consultorios externos: 1 por cada 4 camas censables (ratio industria MX)
  consultorios <- ceiling(camas_censables / RATIO_CONS_CAMA)

  # Quirófanos: 1 por cada 15-20 camas (media 18); NOM-016-SSA3
  quirofanos <- ceiling(camas_censables / 18)

  # UCI adultos: 8-10% de camas censables (usamos 9%)
  camas_uci <- ceiling(camas_censables * 0.09)

  # UCIN neonatal: 4-5% de camas censables (usamos 4.5%)
  camas_ucin <- ceiling(camas_censables * 0.045)

  # Camas de urgencias: 12-15% de camas censables (usamos 13%)
  camas_urgencias <- ceiling(camas_censables * 0.13)

  # Sillones hospital de día: 6-8% de camas censables (usamos 7%)
  sillones_hospital_dia <- ceiling(camas_censables * 0.07)

  list(
    consultorios          = consultorios,
    quirofanos            = quirofanos,
    camas_uci             = camas_uci,
    camas_ucin            = camas_ucin,
    camas_urgencias       = camas_urgencias,
    sillones_hospital_dia = sillones_hospital_dia
  )
}


# ── 3. calcular_capacidad_hospitalaria ────────────────────────────────────────

#' Calculate Hospital Capacity Requirements
#'
#' Calculates bed requirements and other infrastructure needs based on the
#' Hill-Burton methodology adapted for the Mexican healthcare context.
#' Calibration factors (\code{FACTOR_CAMAS}, \code{FACTOR_QX},
#' \code{FACTOR_CONS}) anchor the theoretical result to the approved physical
#' design of 80 census beds for Hospital Manzanillo.
#'
#' @param poblacion numeric. Target population to serve.
#' @param tasa_egresos numeric. Annual hospital discharge rate per capita
#'   (0–1). Default: \code{0.08} (80 per 1,000, approx. 8\% of population).
#' @param dias_estancia_promedio numeric. Average length of stay in days.
#'   Default: \code{4.2} days (IMSS/SSA private-sector benchmark).
#' @param tasa_ocupacion_objetivo numeric. Target occupancy rate (0–1).
#'   Default: \code{0.85} (85\%).
#' @param k_calibracion numeric. Calibration factor for local conditions.
#'   Default: \code{1.0} (no adjustment).
#'   Use \code{> 1} to increase capacity, \code{< 1} to decrease.
#' @param mix_especialidades list. Mix of bed types with named proportions.
#'   Must sum to 1.0. Defaults:
#'   \itemize{
#'     \item \code{medicina_interna = 0.30}
#'     \item \code{cirugia = 0.25}
#'     \item \code{gineco_obstetricia = 0.20}
#'     \item \code{pediatria = 0.15}
#'     \item \code{otras = 0.10}
#'   }
#' @param incluir_complementos logical. If \code{TRUE} (default), also
#'   calculates complementary infrastructure (clinics, ORs, ICU, etc.).
#'
#' @return A named list with:
#'   \describe{
#'     \item{camas_censables}{Total inpatient beds required (integer).}
#'     \item{camas_por_especialidad}{data.frame — beds by specialty.}
#'     \item{consultorios}{Number of outpatient clinics.}
#'     \item{quirofanos}{Number of operating rooms.}
#'     \item{camas_uci}{Adult ICU beds.}
#'     \item{camas_ucin}{NICU beds.}
#'     \item{camas_urgencias}{Emergency beds.}
#'     \item{sillones_hospital_dia}{Day-hospital chairs.}
#'     \item{metricas}{list — egresos, días-cama, tasa ocupación,
#'       índice rotación, camas/1,000 hab, estancia promedio.}
#'   }
#'
#' @details
#' **Hill-Burton formula:**
#' \deqn{Camas = \frac{Poblacion \times TasaEgresos \times DiasEstancia}
#'                    {365 \times TasaOcupacion} \times k_{calibracion}}
#'
#' The result is then scaled by \code{FACTOR_CAMAS} (≈ 0.833) to reconcile
#' the theoretical optimum with the physical design approved for the project.
#' Complementary ratios follow \strong{NOM-016-SSA3-2012} and private-sector
#' benchmarks compiled in the original feasibility model.
#'
#' @examples
#' # Basic calculation for Manzanillo catchment area
#' capacidad <- calcular_capacidad_hospitalaria(poblacion = 191031)
#' print(capacidad$camas_censables)
#'
#' # Custom parameters
#' capacidad <- calcular_capacidad_hospitalaria(
#'   poblacion        = 191031,
#'   tasa_egresos     = 0.10,
#'   k_calibracion    = 1.2
#' )
#'
#' @export
calcular_capacidad_hospitalaria <- function(
  poblacion,
  tasa_egresos             = 0.08,
  dias_estancia_promedio   = 4.2,
  tasa_ocupacion_objetivo  = 0.85,
  k_calibracion            = 1.0,
  mix_especialidades       = list(
    medicina_interna    = 0.30,
    cirugia             = 0.25,
    gineco_obstetricia  = 0.20,
    pediatria           = 0.15,
    otras               = 0.10
  ),
  incluir_complementos = TRUE
) {

  # ── Validación ────────────────────────────────────────────────────────────
  validate_capacity_inputs(
    poblacion, tasa_egresos, dias_estancia_promedio,
    tasa_ocupacion_objetivo, k_calibracion, mix_especialidades
  )

  # ── Cálculo de camas censables (Hill-Burton) ──────────────────────────────

  egresos_anuales   <- poblacion * tasa_egresos
  dias_cama_anuales <- egresos_anuales * dias_estancia_promedio
  camas_base        <- dias_cama_anuales / (365 * tasa_ocupacion_objetivo)
  camas_censables   <- ceiling(camas_base * k_calibracion * FACTOR_CAMAS)

  # ── Distribución por especialidad ─────────────────────────────────────────

  camas_por_esp <- data.frame(
    especialidad = names(mix_especialidades),
    proporcion   = as.numeric(mix_especialidades),
    camas        = NA_integer_,
    stringsAsFactors = FALSE
  )

  for (i in seq_len(nrow(camas_por_esp))) {
    camas_por_esp$camas[i] <- ceiling(camas_censables * camas_por_esp$proporcion[i])
  }

  # ── Infraestructura complementaria ────────────────────────────────────────

  complementos <- if (incluir_complementos) {
    calcular_infraestructura_complementaria(
      camas_censables = camas_censables,
      poblacion       = poblacion
    )
  } else {
    list(
      consultorios          = NA_integer_,
      quirofanos            = NA_integer_,
      camas_uci             = NA_integer_,
      camas_ucin            = NA_integer_,
      camas_urgencias       = NA_integer_,
      sillones_hospital_dia = NA_integer_
    )
  }

  # ── Métricas adicionales ──────────────────────────────────────────────────

  metricas <- list(
    egresos_proyectados      = egresos_anuales,
    dias_cama_totales        = dias_cama_anuales,
    tasa_ocupacion_objetivo  = tasa_ocupacion_objetivo,
    indice_rotacion          = egresos_anuales / camas_censables,
    camas_por_1000_hab       = (camas_censables / poblacion) * 1000,
    promedio_estancia        = dias_estancia_promedio
  )

  # ── Retorno ───────────────────────────────────────────────────────────────

  list(
    camas_censables       = camas_censables,
    camas_por_especialidad = camas_por_esp,
    consultorios          = complementos$consultorios,
    quirofanos            = complementos$quirofanos,
    camas_uci             = complementos$camas_uci,
    camas_ucin            = complementos$camas_ucin,
    camas_urgencias       = complementos$camas_urgencias,
    sillones_hospital_dia = complementos$sillones_hospital_dia,
    metricas              = metricas
  )
}


# ── 4. comparar_con_estandares ────────────────────────────────────────────────

#' Compare Hospital Capacity with International Standards
#'
#' Compares a calculated bed count against WHO, OECD, and national Mexican
#' standards expressed as beds per 1,000 inhabitants.
#'
#' @param camas_censables numeric. Calculated (or planned) bed count.
#' @param poblacion numeric. Target population.
#'
#' @return data.frame with one row per standard:
#'   \describe{
#'     \item{estandar}{Standard name (character).}
#'     \item{camas_por_1000}{Beds per 1,000 inhabitants (numeric).}
#'     \item{camas_totales}{Total beds implied for the given population (integer).}
#'     \item{deficit_vs_proyecto}{Beds required by that standard minus
#'       \code{camas_censables}; negative = project exceeds standard (integer).}
#'     \item{cumple_estandar}{logical — TRUE if project meets or exceeds standard.}
#'   }
#'
#' @examples
#' comparacion <- comparar_con_estandares(camas_censables = 80, poblacion = 191031)
#' print(comparacion)
#'
#' @export
comparar_con_estandares <- function(camas_censables, poblacion) {

  if (!is.numeric(camas_censables) || camas_censables <= 0) {
    stop("camas_censables must be a positive number")
  }
  if (!is.numeric(poblacion) || poblacion <= 0) {
    stop("poblacion must be a positive number")
  }

  camas_por_1000_proyecto <- (camas_censables / poblacion) * 1000

  estandares <- data.frame(
    estandar       = c("OCDE", "OMS", "Mexico Nacional", "Proyecto"),
    camas_por_1000 = c(
      META_OCDE_CAMAS_POR_MIL,
      META_OMS_CAMAS_POR_MIL,
      META_MX_CAMAS_POR_MIL,
      camas_por_1000_proyecto
    ),
    camas_totales  = c(
      ceiling(poblacion * META_OCDE_CAMAS_POR_MIL / 1000),
      ceiling(poblacion * META_OMS_CAMAS_POR_MIL  / 1000),
      ceiling(poblacion * META_MX_CAMAS_POR_MIL   / 1000),
      camas_censables
    ),
    stringsAsFactors = FALSE
  )

  estandares$deficit_vs_proyecto <- estandares$camas_totales - camas_censables
  estandares$cumple_estandar     <- estandares$deficit_vs_proyecto <= 0

  estandares
}


# ── 5. calcular_desarrollo_por_fases ─────────────────────────────────────────

#' Calculate Phased Hospital Development
#'
#' Distributes total capacity requirements across 2 or 3 development phases,
#' preserving the same infrastructure ratios across all phases.
#'
#' @param capacidad_total list. Output from
#'   \code{\link{calcular_capacidad_hospitalaria}}.
#' @param fases integer. Number of development phases (2 or 3).
#'   Default: \code{3}.
#' @param distribucion numeric vector. Proportion of total capacity per phase.
#'   Must have length equal to \code{fases} and sum to 1.0.
#'   Default: \code{c(0.4, 0.35, 0.25)}.
#'
#' @return Named list with one element per phase (\code{fase_1}, \code{fase_2},
#'   …), each containing:
#'   \describe{
#'     \item{camas_censables}{Inpatient beds for this phase.}
#'     \item{consultorios}{Outpatient clinics for this phase.}
#'     \item{quirofanos}{Operating rooms for this phase.}
#'     \item{camas_uci}{ICU beds for this phase.}
#'     \item{proporcion}{Fraction of total capacity (numeric).}
#'   }
#'
#' @examples
#' capacidad <- calcular_capacidad_hospitalaria(poblacion = 191031)
#' fases <- calcular_desarrollo_por_fases(
#'   capacidad,
#'   fases        = 3,
#'   distribucion = c(0.4, 0.35, 0.25)
#' )
#' str(fases$fase_1)
#'
#' @export
calcular_desarrollo_por_fases <- function(capacidad_total,
                                          fases        = 3L,
                                          distribucion = c(0.4, 0.35, 0.25)) {

  if (length(distribucion) != fases) {
    stop("distribucion must have length equal to fases")
  }

  if (abs(sum(distribucion) - 1.0) > 0.01) {
    stop("distribucion must sum to 1.0")
  }

  required_fields <- c("camas_censables", "consultorios", "quirofanos", "camas_uci")
  missing_fields  <- setdiff(required_fields, names(capacidad_total))
  if (length(missing_fields) > 0) {
    stop("capacidad_total is missing fields: ", paste(missing_fields, collapse = ", "))
  }

  resultado <- vector("list", fases)
  names(resultado) <- paste0("fase_", seq_len(fases))

  for (fase in seq_len(fases)) {
    p <- distribucion[fase]
    resultado[[paste0("fase_", fase)]] <- list(
      camas_censables = ceiling(capacidad_total$camas_censables * p),
      consultorios    = ceiling(capacidad_total$consultorios    * p),
      quirofanos      = ceiling(capacidad_total$quirofanos      * p),
      camas_uci       = ceiling(capacidad_total$camas_uci       * p),
      proporcion      = p
    )
  }

  resultado
}
