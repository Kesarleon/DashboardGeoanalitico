# ══════════════════════════════════════════════════════════════════════════════
# R/models/huff_model.R
# Huff Gravity Model for Healthcare Market Share Estimation
# ══════════════════════════════════════════════════════════════════════════════

#' Validate inputs for Huff model
#'
#' @param demanda demand data.frame
#' @param oferta supply data.frame
#'
#' @return NULL (throws error if validation fails)
#' @keywords internal
validate_huff_inputs <- function(demanda, oferta) {

  if (!is.data.frame(demanda)) stop("demanda must be a data.frame")
  if (!is.data.frame(oferta))  stop("oferta must be a data.frame")

  required_dem <- c("id", "lat", "lon", "poblacion")
  missing_dem  <- setdiff(required_dem, names(demanda))
  if (length(missing_dem) > 0) {
    stop("demanda missing required columns: ", paste(missing_dem, collapse = ", "))
  }

  required_of <- c("id", "nombre", "lat", "lon", "camas", "especialidades")
  missing_of  <- setdiff(required_of, names(oferta))
  if (length(missing_of) > 0) {
    stop("oferta missing required columns: ", paste(missing_of, collapse = ", "))
  }

  if (nrow(demanda) == 0) stop("demanda cannot be empty")
  if (nrow(oferta)  == 0) stop("oferta cannot be empty")

  if (any(demanda$poblacion <= 0, na.rm = TRUE)) {
    stop("demanda$poblacion must be positive")
  }

  if (any(oferta$camas <= 0, na.rm = TRUE)) {
    stop("oferta$camas must be positive")
  }

  invisible(NULL)
}


#' Calculate market concentration metrics
#'
#' @param resumen summary data.frame with total_pacientes column
#'
#' @return list with market metrics
#' @keywords internal
calculate_market_metrics <- function(resumen) {

  total <- sum(resumen$total_pacientes, na.rm = TRUE)

  if (total == 0) {
    return(list(
      total_market       = 0,
      hhi_index          = NA,
      top3_concentration = NA,
      gini_coefficient   = NA,
      n_competitors      = nrow(resumen)
    ))
  }

  shares <- resumen$total_pacientes / total

  # HHI (Herfindahl-Hirschman Index): 0 = competencia perfecta, 10000 = monopolio
  hhi <- sum(shares^2) * 10000

  # Top 3 concentration
  top3 <- sum(head(sort(shares, decreasing = TRUE), 3))

  # Gini coefficient (desigualdad en distribución de mercado)
  shares_sorted <- sort(shares)
  n    <- length(shares_sorted)
  gini <- (2 * sum((1:n) * shares_sorted)) / (n * sum(shares_sorted)) - (n + 1) / n

  list(
    total_market       = total,
    hhi_index          = round(hhi, 2),
    top3_concentration = round(top3, 3),
    gini_coefficient   = round(gini, 3),
    n_competitors      = nrow(resumen)
  )
}


#' Huff Model for Market Share Estimation
#'
#' Calculates probability of patient capture and market share for healthcare facilities
#' using the Huff gravity model. The model considers distance decay and facility
#' attractiveness based on beds and specialties.
#'
#' @param demanda data.frame with columns:
#'   - id: unique identifier for demand point
#'   - lat: latitude
#'   - lon: longitude
#'   - poblacion: population at demand point
#'
#' @param oferta data.frame with columns:
#'   - id: unique identifier for facility
#'   - nombre: facility name
#'   - tipo: facility type (publico/privado)
#'   - lat: latitude
#'   - lon: longitude
#'   - camas: number of beds
#'   - especialidades: number of specialties
#'   - k_factor: calibration factor (optional, defaults to 1)
#'
#' @param sensibilidad_dist numeric. Distance sensitivity parameter (lambda).
#'   Higher values = stronger distance decay effect. Default: 2
#'
#' @param peso_camas numeric. Weight for beds in attractiveness calculation (0-1).
#'   Default: 0.1
#'
#' @param peso_esp numeric. Weight for specialties in attractiveness calculation (0-1).
#'   Default: 0.9
#'
#' @return list with three elements:
#'   - detallado: full demand x supply matrix with probabilities for each pair
#'   - resumen: aggregated results by facility (total patients, market share, avg distance)
#'   - metricas: market concentration metrics (HHI, top3 share, Gini coefficient)
#'
#' @examples
#' \dontrun{
#' demanda <- data.frame(
#'   id = 1:3,
#'   lat = c(19.1, 19.2, 19.3),
#'   lon = c(-104.3, -104.4, -104.5),
#'   poblacion = c(10000, 15000, 12000)
#' )
#'
#' oferta <- data.frame(
#'   id = 1:2,
#'   nombre = c("Hospital A", "Hospital B"),
#'   tipo = c("publico", "privado"),
#'   lat = c(19.15, 19.25),
#'   lon = c(-104.35, -104.45),
#'   camas = c(100, 50),
#'   especialidades = c(15, 10),
#'   k_factor = c(1, 1)
#' )
#'
#' resultado <- calcular_huff(demanda, oferta, sensibilidad_dist = 2)
#' print(resultado$resumen)
#' print(resultado$metricas)
#' }
#'
#' @export
calcular_huff <- function(demanda, oferta, sensibilidad_dist = 2,
                          peso_camas = 0.1, peso_esp = 0.9) {

  # Validar inputs
  validate_huff_inputs(demanda, oferta)

  # Normalizar k_factor
  if (!"k_factor" %in% names(oferta)) oferta$k_factor <- 1
  oferta$k_factor[is.na(oferta$k_factor)] <- 1

  # Calcular atractividad
  oferta <- oferta %>%
    mutate(
      atractividad_base  = (camas * peso_camas) + (especialidades * peso_esp),
      atractividad_final = atractividad_base * k_factor
    )

  # Grid cruzado demanda x oferta
  grid  <- expand.grid(dem_id = demanda$id, of_id = oferta$id)
  datos <- grid %>%
    left_join(demanda, by = c("dem_id" = "id")) %>%
    left_join(oferta,  by = c("of_id"  = "id"), suffix = c("_dem", "_of"))

  # Distancias Haversine en km
  dist_km <- distHaversine(
    matrix(c(datos$lon_dem, datos$lat_dem), ncol = 2),
    matrix(c(datos$lon_of,  datos$lat_of),  ncol = 2)
  ) / 1000

  # Fricción, utilidad, probabilidad y mercado captado
  datos$distancia <- pmax(dist_km, 0.1)
  datos <- datos %>%
    mutate(
      friccion       = distancia ^ sensibilidad_dist,
      utilidad       = atractividad_final / friccion
    ) %>%
    group_by(dem_id) %>%
    mutate(probabilidad = utilidad / sum(utilidad)) %>%
    ungroup() %>%
    mutate(mercado_captado = probabilidad * poblacion)

  # Resumen por hospital con cuota de mercado y distancia promedio ponderada
  resumen <- datos %>%
    group_by(of_id, nombre, tipo) %>%
    summarise(
      total_pacientes    = sum(mercado_captado),
      distancia_prom_pond = weighted.mean(distancia, w = mercado_captado),
      .groups = "drop"
    ) %>%
    mutate(cuota_mercado = total_pacientes / sum(total_pacientes))

  # Calcular métricas adicionales de mercado
  metricas <- calculate_market_metrics(resumen)

  list(
    detallado = datos,
    resumen   = resumen,
    metricas  = metricas
  )
}
