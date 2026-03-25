library(testthat)
library(dplyr)

# Cargar modelo (necesario cuando se corre en aislamiento)
source(here::here("R/models/huff_model.R"))

# ══════════════════════════════════════════════════════════════════════════════
# Datos sintéticos compartidos
# ══════════════════════════════════════════════════════════════════════════════
demanda_test <- data.frame(
  id        = 1:3,
  lat       = c(19.1, 19.2, 19.3),
  lon       = c(-104.3, -104.4, -104.5),
  poblacion = c(10000, 15000, 12000)
)

oferta_test <- data.frame(
  id             = c(1, 2),
  nombre         = c("Hospital A", "Hospital B"),
  tipo           = c("publico", "privado"),
  lat            = c(19.15, 19.25),
  lon            = c(-104.35, -104.45),
  camas          = c(100, 50),
  especialidades = c(15, 10),
  k_factor       = c(1, 1)
)

# ══════════════════════════════════════════════════════════════════════════════
# 1. Tests de validación
# ══════════════════════════════════════════════════════════════════════════════

test_that("validate_huff_inputs catches missing columns in demanda", {
  bad_demanda <- demanda_test[, c("id", "lat")]  # falta lon y poblacion

  expect_error(
    calcular_huff(bad_demanda, oferta_test),
    "missing required columns"
  )
})

test_that("validate_huff_inputs catches missing columns in oferta", {
  bad_oferta <- oferta_test[, c("id", "nombre")]  # faltan otras

  expect_error(
    calcular_huff(demanda_test, bad_oferta),
    "missing required columns"
  )
})

test_that("validate_huff_inputs catches empty data", {
  empty_demanda <- demanda_test[0, ]

  expect_error(
    calcular_huff(empty_demanda, oferta_test),
    "cannot be empty"
  )
})

test_that("validate_huff_inputs catches invalid values", {
  bad_demanda <- demanda_test
  bad_demanda$poblacion[1] <- -100

  expect_error(
    calcular_huff(bad_demanda, oferta_test),
    "must be positive"
  )
})

# ══════════════════════════════════════════════════════════════════════════════
# 2. Tests de cálculo correcto
# ══════════════════════════════════════════════════════════════════════════════

test_that("calcular_huff returns correct structure", {
  result <- calcular_huff(demanda_test, oferta_test)

  expect_type(result, "list")
  expect_named(result, c("detallado", "resumen", "metricas"))

  expect_s3_class(result$detallado, "data.frame")
  expect_s3_class(result$resumen, "data.frame")
  expect_type(result$metricas, "list")
})

test_that("probabilities sum to 1 for each demand point", {
  result <- calcular_huff(demanda_test, oferta_test)

  # columna real: "probabilidad" (no prob_captura)
  prob_sums <- result$detallado %>%
    group_by(dem_id) %>%
    summarise(total_prob = sum(probabilidad)) %>%
    pull(total_prob)

  expect_true(all(abs(prob_sums - 1) < 0.001))
})

test_that("total patients equals total population", {
  result <- calcular_huff(demanda_test, oferta_test)

  total_patients   <- sum(result$resumen$total_pacientes)
  total_population <- sum(demanda_test$poblacion)

  expect_equal(total_patients, total_population, tolerance = 1)
})

test_that("sensibilidad_dist parameter affects results", {
  result_low  <- calcular_huff(demanda_test, oferta_test, sensibilidad_dist = 1)
  result_high <- calcular_huff(demanda_test, oferta_test, sensibilidad_dist = 3)

  expect_false(identical(result_low$resumen, result_high$resumen))
})

# ══════════════════════════════════════════════════════════════════════════════
# 3. Tests de métricas
# ══════════════════════════════════════════════════════════════════════════════

test_that("market metrics are calculated", {
  result <- calcular_huff(demanda_test, oferta_test)

  expect_named(result$metricas,
               c("total_market", "hhi_index", "top3_concentration",
                 "gini_coefficient", "n_competitors"))

  expect_equal(result$metricas$n_competitors, 2)
  expect_true(result$metricas$hhi_index >= 0)
  expect_true(result$metricas$hhi_index <= 10000)
  expect_true(result$metricas$top3_concentration >= 0)
  expect_true(result$metricas$top3_concentration <= 1)
})

test_that("HHI index makes sense with 2 competitors", {
  # Con 2 competidores, HHI debe estar entre 5000 (50-50) y 10000 (100-0)
  result <- calcular_huff(demanda_test, oferta_test)

  expect_true(result$metricas$hhi_index >= 5000)
  expect_true(result$metricas$hhi_index <= 10000)
})

# ══════════════════════════════════════════════════════════════════════════════
# 4. Tests de edge cases
# ══════════════════════════════════════════════════════════════════════════════

test_that("works with single competitor", {
  oferta_single <- oferta_test[1, ]

  result <- calcular_huff(demanda_test, oferta_single)

  # Un solo competidor captura el 100% del mercado
  expect_equal(result$resumen$total_pacientes[1], sum(demanda_test$poblacion),
               tolerance = 1)
  expect_equal(result$metricas$hhi_index, 10000)  # Monopolio
})

test_that("works with equal attractiveness and location", {
  oferta_equal <- data.frame(
    id             = 1:2,
    nombre         = c("Hospital A", "Hospital B"),
    tipo           = c("publico", "publico"),
    lat            = c(19.15, 19.15),   # Misma ubicación
    lon            = c(-104.35, -104.35),
    camas          = c(100, 100),       # Mismas camas
    especialidades = c(15, 15),         # Mismas especialidades
    k_factor       = c(1, 1)
  )

  result <- calcular_huff(demanda_test, oferta_equal)

  # Con atractividad y ubicación idénticas deben captar igual
  expect_equal(
    result$resumen$total_pacientes[1],
    result$resumen$total_pacientes[2],
    tolerance = 1
  )
})

test_that("k_factor=0 in oferta is handled (defaults to 1)", {
  oferta_kna <- oferta_test
  oferta_kna$k_factor[1] <- NA

  # No debe lanzar error, NA se reemplaza por 1
  expect_no_error(calcular_huff(demanda_test, oferta_kna))
})

test_that("resumen contains expected columns", {
  result <- calcular_huff(demanda_test, oferta_test)

  expect_true(all(c("of_id", "nombre", "tipo", "total_pacientes",
                    "distancia_prom_pond", "cuota_mercado") %in%
                    names(result$resumen)))
})
