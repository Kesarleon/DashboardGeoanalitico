# ══════════════════════════════════════════════════════════════════════════════
# tests/testthat/test-capacity_model.R
# Unit tests for R/models/capacity_model.R
#
# Coverage:
#   - calcular_capacidad_hospitalaria  : structure, Hill-Burton, calibration,
#                                        specialty mix, complements, metrics
#   - validate_capacity_inputs         : all invalid-input branches
#   - comparar_con_estandares          : structure, values, deficit logic
#   - calcular_desarrollo_por_fases    : structure, distribution, validation
#   - Edge cases                       : small population, extreme rates
# ══════════════════════════════════════════════════════════════════════════════

library(testthat)

# Load model (sourced directly for non-package usage)
source(here::here("R/models/capacity_model.R"))

# ── Parámetros base ───────────────────────────────────────────────────────────

poblacion_test       <- 191031
tasa_egresos_test    <- 0.08
dias_estancia_test   <- 4.2
ocupacion_test       <- 0.85


# ══════════════════════════════════════════════════════════════════════════════
# 1. calcular_capacidad_hospitalaria
# ══════════════════════════════════════════════════════════════════════════════

test_that("calcular_capacidad_hospitalaria returns correct structure", {
  result <- calcular_capacidad_hospitalaria(poblacion = poblacion_test)

  expect_type(result, "list")
  expect_named(result, c("camas_censables", "camas_por_especialidad",
                         "consultorios", "quirofanos", "camas_uci",
                         "camas_ucin", "camas_urgencias", "sillones_hospital_dia",
                         "metricas"))

  expect_true(is.numeric(result$camas_censables))
  expect_s3_class(result$camas_por_especialidad, "data.frame")
  expect_type(result$metricas, "list")
})

test_that("calcular_capacidad_hospitalaria calculates reasonable bed count", {
  result <- calcular_capacidad_hospitalaria(poblacion = poblacion_test)

  # Para población ~191k con tasa 8%, estancia 4.2, ocupación 85%
  # Debe dar aproximadamente 50-150 camas
  expect_true(result$camas_censables >= 50)
  expect_true(result$camas_censables <= 150)
})

test_that("calcular_capacidad_hospitalaria Hill-Burton formula is correct", {
  # Test con valores conocidos
  pob  <- 100000
  tasa <- 0.10   # 10%
  dias <- 5
  ocup <- 0.80   # 80%

  result <- calcular_capacidad_hospitalaria(
    poblacion               = pob,
    tasa_egresos            = tasa,
    dias_estancia_promedio  = dias,
    tasa_ocupacion_objetivo = ocup,
    k_calibracion           = 1.0
  )

  # La implementación aplica FACTOR_CAMAS (≈0.833) al resultado teórico
  # para anclar al diseño físico aprobado de 80 camas
  expected <- ceiling((pob * tasa * dias) / (365 * ocup) * FACTOR_CAMAS)
  expect_equal(result$camas_censables, expected)
})

test_that("calcular_capacidad_hospitalaria respects calibration factor", {
  result_base <- calcular_capacidad_hospitalaria(
    poblacion     = poblacion_test,
    k_calibracion = 1.0
  )

  result_high <- calcular_capacidad_hospitalaria(
    poblacion     = poblacion_test,
    k_calibracion = 1.5
  )

  # Con k=1.5 debe producir más camas que con k=1.0
  expect_true(result_high$camas_censables > result_base$camas_censables)
  # Debe ser al menos ~40% mayor (margen por ceiling())
  expect_true(result_high$camas_censables >= result_base$camas_censables * 1.4)
})

test_that("calcular_capacidad_hospitalaria specialty mix sums correctly", {
  result <- calcular_capacidad_hospitalaria(poblacion = poblacion_test)

  suma_especialidades <- sum(result$camas_por_especialidad$camas)

  # Permitir diferencia pequeña por redondeo (máx 1 cama por especialidad)
  expect_true(
    abs(suma_especialidades - result$camas_censables) <=
      nrow(result$camas_por_especialidad)
  )
})

test_that("calcular_capacidad_hospitalaria includes complementary infrastructure", {
  result <- calcular_capacidad_hospitalaria(
    poblacion            = poblacion_test,
    incluir_complementos = TRUE
  )

  expect_true(is.numeric(result$consultorios))
  expect_true(is.numeric(result$quirofanos))
  expect_true(result$consultorios > 0)
  expect_true(result$quirofanos   > 0)
  expect_true(result$camas_uci    > 0)
  expect_true(result$camas_ucin   > 0)
})

test_that("calcular_capacidad_hospitalaria without complementary infrastructure", {
  result <- calcular_capacidad_hospitalaria(
    poblacion            = poblacion_test,
    incluir_complementos = FALSE
  )

  expect_true(is.na(result$consultorios))
  expect_true(is.na(result$quirofanos))
  expect_true(is.na(result$camas_uci))
  expect_true(is.na(result$camas_ucin))
})

test_that("calcular_capacidad_hospitalaria calculates metrics", {
  result <- calcular_capacidad_hospitalaria(poblacion = poblacion_test)

  expect_named(result$metricas, c("egresos_proyectados", "dias_cama_totales",
                                   "tasa_ocupacion_objetivo", "indice_rotacion",
                                   "camas_por_1000_hab", "promedio_estancia"))

  # Camas por 1000 debe ser razonable para el contexto del proyecto
  expect_true(result$metricas$camas_por_1000_hab > 0.3)
  expect_true(result$metricas$camas_por_1000_hab < 5.0)

  # Índice de rotación debe ser positivo
  expect_true(result$metricas$indice_rotacion > 0)

  # Egresos proyectados: población × tasa_egresos (default 0.08)
  expect_equal(result$metricas$egresos_proyectados,
               poblacion_test * tasa_egresos_test)
})


# ══════════════════════════════════════════════════════════════════════════════
# 2. validate_capacity_inputs (via calcular_capacidad_hospitalaria)
# ══════════════════════════════════════════════════════════════════════════════

test_that("validate_capacity_inputs catches invalid population", {
  expect_error(
    calcular_capacidad_hospitalaria(poblacion = -1000),
    "positive number"
  )

  # Error message is "positive number" (not "numeric") for non-numeric input
  expect_error(
    calcular_capacidad_hospitalaria(poblacion = "not a number"),
    "positive number"
  )
})

test_that("validate_capacity_inputs catches invalid discharge rate", {
  expect_error(
    calcular_capacidad_hospitalaria(poblacion = 100000, tasa_egresos = -0.1),
    "between 0 and 1"
  )

  expect_error(
    calcular_capacidad_hospitalaria(poblacion = 100000, tasa_egresos = 1.5),
    "between 0 and 1"
  )
})

test_that("validate_capacity_inputs catches invalid length of stay", {
  expect_error(
    calcular_capacidad_hospitalaria(poblacion = 100000, dias_estancia_promedio = 0),
    "between 0 and 30"
  )

  expect_error(
    calcular_capacidad_hospitalaria(poblacion = 100000, dias_estancia_promedio = 50),
    "between 0 and 30"
  )
})

test_that("validate_capacity_inputs catches invalid occupancy", {
  expect_error(
    calcular_capacidad_hospitalaria(poblacion = 100000, tasa_ocupacion_objetivo = 0),
    "between 0 and 1"
  )

  expect_error(
    calcular_capacidad_hospitalaria(poblacion = 100000, tasa_ocupacion_objetivo = 1.2),
    "between 0 and 1"
  )
})

test_that("validate_capacity_inputs catches invalid calibration factor", {
  expect_error(
    calcular_capacidad_hospitalaria(poblacion = 100000, k_calibracion = -1),
    "positive number"
  )

  expect_error(
    calcular_capacidad_hospitalaria(poblacion = 100000, k_calibracion = 0),
    "positive number"
  )
})

test_that("validate_capacity_inputs catches invalid specialty mix", {
  bad_mix <- list(
    medicina_interna = 0.40,
    cirugia          = 0.40   # suma 0.80, no 1.0
  )

  expect_error(
    calcular_capacidad_hospitalaria(poblacion = 100000, mix_especialidades = bad_mix),
    "must sum to 1.0"
  )
})


# ══════════════════════════════════════════════════════════════════════════════
# 3. comparar_con_estandares
# ══════════════════════════════════════════════════════════════════════════════

test_that("comparar_con_estandares returns data.frame with expected rows", {
  result <- comparar_con_estandares(camas_censables = 80, poblacion = 191031)

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) >= 4)  # OCDE, OMS, Mexico Nacional, Proyecto
  expect_named(result, c("estandar", "camas_por_1000", "camas_totales",
                          "deficit_vs_proyecto", "cumple_estandar"))
})

test_that("comparar_con_estandares calculates project row correctly", {
  camas <- 80
  pob   <- 191031

  result <- comparar_con_estandares(camas_censables = camas, poblacion = pob)

  proyecto_row <- result[result$estandar == "Proyecto", ]
  expect_equal(proyecto_row$camas_totales, camas)

  expected_per_1000 <- (camas / pob) * 1000
  expect_equal(proyecto_row$camas_por_1000, expected_per_1000, tolerance = 0.01)

  # El proyecto no tiene déficit contra sí mismo
  expect_equal(proyecto_row$deficit_vs_proyecto, 0L)
  expect_true(proyecto_row$cumple_estandar)
})

test_that("comparar_con_estandares identifies deficits vs OCDE", {
  # 50 camas para 191k habitantes (≈0.26/1000) está muy por debajo del OCDE (3.4/1000)
  result <- comparar_con_estandares(camas_censables = 50, poblacion = 191031)

  ocde_row <- result[result$estandar == "OCDE", ]
  expect_true(ocde_row$deficit_vs_proyecto > 0)
  expect_false(ocde_row$cumple_estandar)
})

test_that("comparar_con_estandares validates inputs", {
  expect_error(comparar_con_estandares(camas_censables = -10, poblacion = 100000),
               "positive number")
  expect_error(comparar_con_estandares(camas_censables = 80, poblacion = 0),
               "positive number")
})


# ══════════════════════════════════════════════════════════════════════════════
# 4. calcular_desarrollo_por_fases
# ══════════════════════════════════════════════════════════════════════════════

test_that("calcular_desarrollo_por_fases returns correct structure", {
  capacidad <- calcular_capacidad_hospitalaria(poblacion = poblacion_test)

  fases <- calcular_desarrollo_por_fases(
    capacidad,
    fases        = 3,
    distribucion = c(0.4, 0.35, 0.25)
  )

  expect_type(fases, "list")
  expect_length(fases, 3)
  expect_named(fases, c("fase_1", "fase_2", "fase_3"))

  # Cada fase debe tener los campos esperados
  expect_named(fases$fase_1, c("camas_censables", "consultorios",
                                "quirofanos", "camas_uci", "proporcion"))
})

test_that("calcular_desarrollo_por_fases distributes capacity correctly", {
  capacidad <- calcular_capacidad_hospitalaria(poblacion = poblacion_test)

  fases <- calcular_desarrollo_por_fases(
    capacidad,
    fases        = 2,
    distribucion = c(0.6, 0.4)
  )

  # Fase 1 debe tener ~60% de las camas (tolerancia por ceiling())
  expect_true(fases$fase_1$camas_censables >= capacidad$camas_censables * 0.55)
  expect_true(fases$fase_1$camas_censables <= capacidad$camas_censables * 0.65)

  # Fase 2 debe tener menos camas que fase 1
  expect_true(fases$fase_2$camas_censables < fases$fase_1$camas_censables)
})

test_that("calcular_desarrollo_por_fases proporcion field is correct", {
  capacidad <- calcular_capacidad_hospitalaria(poblacion = poblacion_test)
  dist      <- c(0.4, 0.35, 0.25)

  fases <- calcular_desarrollo_por_fases(capacidad, fases = 3, distribucion = dist)

  expect_equal(fases$fase_1$proporcion, 0.40)
  expect_equal(fases$fase_2$proporcion, 0.35)
  expect_equal(fases$fase_3$proporcion, 0.25)
})

test_that("calcular_desarrollo_por_fases validates distribution length", {
  capacidad <- calcular_capacidad_hospitalaria(poblacion = poblacion_test)

  expect_error(
    calcular_desarrollo_por_fases(capacidad, fases = 3, distribucion = c(0.5, 0.5)),
    "must have length equal"
  )
})

test_that("calcular_desarrollo_por_fases validates distribution sum", {
  capacidad <- calcular_capacidad_hospitalaria(poblacion = poblacion_test)

  expect_error(
    calcular_desarrollo_por_fases(capacidad, fases = 2, distribucion = c(0.6, 0.5)),
    "must sum to 1.0"
  )
})

test_that("calcular_desarrollo_por_fases validates required fields", {
  capacidad_incompleta <- list(camas_censables = 80)  # le faltan consultorios, etc.

  expect_error(
    calcular_desarrollo_por_fases(capacidad_incompleta),
    "missing fields"
  )
})


# ══════════════════════════════════════════════════════════════════════════════
# 5. Edge cases
# ══════════════════════════════════════════════════════════════════════════════

test_that("capacity model works with small population", {
  result <- calcular_capacidad_hospitalaria(poblacion = 10000)

  expect_true(result$camas_censables > 0)
  expect_true(is.numeric(result$consultorios))
})

test_that("capacity model works with very low discharge rate", {
  result <- calcular_capacidad_hospitalaria(
    poblacion    = 100000,
    tasa_egresos = 0.02   # 2%
  )

  expect_true(result$camas_censables > 0)
})

test_that("capacity model works with high occupancy target", {
  result_high_ocup <- calcular_capacidad_hospitalaria(
    poblacion               = 100000,
    tasa_ocupacion_objetivo = 0.95
  )

  result_low_ocup <- calcular_capacidad_hospitalaria(
    poblacion               = 100000,
    tasa_ocupacion_objetivo = 0.75
  )

  # Con ocupación más alta necesita menos camas (denominador mayor)
  expect_true(result_high_ocup$camas_censables < result_low_ocup$camas_censables)
})

test_that("complementary infrastructure scales with bed count", {
  result_small <- calcular_infraestructura_complementaria(
    camas_censables = 40,
    poblacion       = 100000
  )
  result_large <- calcular_infraestructura_complementaria(
    camas_censables = 120,
    poblacion       = 100000
  )

  expect_true(result_large$consultorios    > result_small$consultorios)
  expect_true(result_large$quirofanos      > result_small$quirofanos)
  expect_true(result_large$camas_uci       > result_small$camas_uci)
  expect_true(result_large$camas_urgencias > result_small$camas_urgencias)
})
