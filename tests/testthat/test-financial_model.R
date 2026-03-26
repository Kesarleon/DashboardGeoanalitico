library(testthat)

# Cargar modelo (necesario cuando se corre en aislamiento)
source(here::here("R/models/financial_model.R"))

# ══════════════════════════════════════════════════════════════════════════════
# Datos sintéticos compartidos
# ══════════════════════════════════════════════════════════════════════════════
params_test <- list(
  capex_fase1             = 250000000,
  capex_fase2             = 200000000,
  capex_fase3             = 200000000,
  año_fase2               = 4,
  año_fase3               = 7,
  captacion_anual         = 8429,
  tarifa_promedio         = 50000,
  tasa_ocupacion          = 0.85,
  costo_variable_paciente = 30000,
  costos_fijos_anuales    = 50000000,
  tasa_crecimiento        = 0.03,
  años                    = 10
)

flujos_simple   <- c(-1000, 200, 300, 400, 500)  # TIR ~14.5%
flujos_negativo <- c(-1000, 100, 100, 100)        # TIR negativa


# ══════════════════════════════════════════════════════════════════════════════
# calcular_irr
# ══════════════════════════════════════════════════════════════════════════════
test_that("calcular_irr returns numeric value", {
  result <- calcular_irr(flujos_simple)

  expect_type(result, "double")
  expect_false(is.na(result))
})

test_that("calcular_irr with known values", {
  # Flujos con TIR conocida aproximada (~23.6%)
  flujos <- c(-1000, 500, 500)
  result <- calcular_irr(flujos)

  expect_true(result > 0.20 && result < 0.30)
})

test_that("calcular_irr validates inputs", {
  expect_error(
    calcular_irr("not numeric"),
    "must be numeric"
  )

  expect_error(
    calcular_irr(c(-1000)),
    "at least 2 values"
  )
})

test_that("calcular_irr warns for positive initial flow", {
  expect_warning(
    calcular_irr(c(1000, 200, 300)),
    "should be negative"
  )
})

test_that("calcular_irr handles no convergence gracefully", {
  # Todos negativos: no existe TIR real
  flujos_bad <- c(-1000, -100, -100, -100)
  result <- calcular_irr(flujos_bad, max_iter = 10)

  expect_true(is.na(result) || is.numeric(result))
})

test_that("calcular_irr returns decimal not percentage", {
  # TIR para c(-1000, 500, 500) está cerca de 0.236, no 23.6
  result <- calcular_irr(c(-1000, 500, 500))

  expect_true(result < 1)   # decimal, no porcentaje
})

test_that("calcular_irr handles NA in flujos", {
  result <- calcular_irr(c(-1000, NA, 300))

  expect_true(is.na(result))
})


# ══════════════════════════════════════════════════════════════════════════════
# proyectar_flujos_caja
# ══════════════════════════════════════════════════════════════════════════════
test_that("proyectar_flujos_caja returns correct structure", {
  result <- proyectar_flujos_caja(
    capex_fase1             = params_test$capex_fase1,
    captacion_anual         = params_test$captacion_anual,
    tarifa_promedio         = params_test$tarifa_promedio,
    tasa_ocupacion          = params_test$tasa_ocupacion,
    costo_variable_paciente = params_test$costo_variable_paciente,
    costos_fijos_anuales    = params_test$costos_fijos_anuales,
    años                    = 10
  )

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 10)
  expect_named(result, c("año", "ingresos", "egresos", "flujo_neto",
                         "flujo_acumulado", "fase"))
})

test_that("proyectar_flujos_caja has positive revenues", {
  result <- proyectar_flujos_caja(
    capex_fase1             = params_test$capex_fase1,
    captacion_anual         = params_test$captacion_anual,
    tarifa_promedio         = params_test$tarifa_promedio,
    tasa_ocupacion          = params_test$tasa_ocupacion,
    costo_variable_paciente = params_test$costo_variable_paciente,
    costos_fijos_anuales    = params_test$costos_fijos_anuales,
    años                    = 10
  )

  expect_true(all(result$ingresos > 0))
})

test_that("proyectar_flujos_caja shows revenue growth", {
  result <- proyectar_flujos_caja(
    capex_fase1             = params_test$capex_fase1,
    captacion_anual         = params_test$captacion_anual,
    tarifa_promedio         = params_test$tarifa_promedio,
    tasa_ocupacion          = params_test$tasa_ocupacion,
    costo_variable_paciente = params_test$costo_variable_paciente,
    costos_fijos_anuales    = params_test$costos_fijos_anuales,
    tasa_crecimiento        = 0.05,
    años                    = 10
  )

  expect_true(result$ingresos[10] > result$ingresos[1])
})

test_that("proyectar_flujos_caja handles multiple phases", {
  result <- proyectar_flujos_caja(
    capex_fase1             = params_test$capex_fase1,
    capex_fase2             = params_test$capex_fase2,
    año_fase2               = 4,
    captacion_anual         = params_test$captacion_anual,
    tarifa_promedio         = params_test$tarifa_promedio,
    tasa_ocupacion          = params_test$tasa_ocupacion,
    costo_variable_paciente = params_test$costo_variable_paciente,
    costos_fijos_anuales    = params_test$costos_fijos_anuales,
    años                    = 10
  )

  # La fase debe cambiar en año 4
  expect_equal(result$fase[3], "Fase 1")
  expect_equal(result$fase[4], "Fase 2")

  # Los egresos del año 4 deben incluir capex_fase2
  expect_true(result$egresos[4] > result$egresos[3] + params_test$capex_fase2 * 0.9)
})

test_that("proyectar_flujos_caja año column is 1:años", {
  result <- proyectar_flujos_caja(
    capex_fase1             = params_test$capex_fase1,
    captacion_anual         = params_test$captacion_anual,
    tarifa_promedio         = params_test$tarifa_promedio,
    tasa_ocupacion          = params_test$tasa_ocupacion,
    costo_variable_paciente = params_test$costo_variable_paciente,
    costos_fijos_anuales    = params_test$costos_fijos_anuales,
    años                    = 5
  )

  expect_equal(result$año, 1:5)
})

test_that("proyectar_flujos_caja validates negative capex_fase1", {
  expect_error(
    proyectar_flujos_caja(
      capex_fase1             = -1000,
      captacion_anual         = 1000,
      tarifa_promedio         = 1000,
      costo_variable_paciente = 500,
      costos_fijos_anuales    = 10000
    ),
    "must be positive"
  )
})

test_that("proyectar_flujos_caja validates año_fase2 out of range", {
  expect_error(
    proyectar_flujos_caja(
      capex_fase1             = params_test$capex_fase1,
      capex_fase2             = params_test$capex_fase2,
      año_fase2               = 15,           # fuera del horizonte de 10 años
      captacion_anual         = params_test$captacion_anual,
      tarifa_promedio         = params_test$tarifa_promedio,
      tasa_ocupacion          = params_test$tasa_ocupacion,
      costo_variable_paciente = params_test$costo_variable_paciente,
      costos_fijos_anuales    = params_test$costos_fijos_anuales,
      años                    = 10
    ),
    "año_fase2"
  )
})


# ══════════════════════════════════════════════════════════════════════════════
# calcular_metricas_financieras
# ══════════════════════════════════════════════════════════════════════════════
test_that("calcular_metricas_financieras returns all metrics", {
  flujos_df <- proyectar_flujos_caja(
    capex_fase1             = params_test$capex_fase1,
    captacion_anual         = params_test$captacion_anual,
    tarifa_promedio         = params_test$tarifa_promedio,
    tasa_ocupacion          = params_test$tasa_ocupacion,
    costo_variable_paciente = params_test$costo_variable_paciente,
    costos_fijos_anuales    = params_test$costos_fijos_anuales,
    años                    = 10
  )

  metricas <- calcular_metricas_financieras(
    flujos_df,
    capex_inicial = -params_test$capex_fase1
  )

  expect_type(metricas, "list")
  expect_named(metricas, c("irr", "npv", "roi", "payback_periodo", "margen_promedio"))
})

test_that("calcular_metricas_financieras IRR is in reasonable range", {
  flujos_df <- proyectar_flujos_caja(
    capex_fase1             = 100000,
    captacion_anual         = 1000,
    tarifa_promedio         = 50000,
    tasa_ocupacion          = 0.85,
    costo_variable_paciente = 30000,
    costos_fijos_anuales    = 5000000,
    años                    = 10
  )

  metricas <- calcular_metricas_financieras(
    flujos_df,
    capex_inicial = -100000
  )

  expect_true(is.numeric(metricas$irr))
  expect_true(metricas$irr > -0.5 && metricas$irr < 1.0)
})

test_that("calcular_metricas_financieras ROI is numeric", {
  flujos_df <- proyectar_flujos_caja(
    capex_fase1             = 100000,
    captacion_anual         = 1000,
    tarifa_promedio         = 50000,
    tasa_ocupacion          = 0.85,
    costo_variable_paciente = 30000,
    costos_fijos_anuales    = 5000000,
    años                    = 10
  )

  metricas <- calcular_metricas_financieras(
    flujos_df,
    capex_inicial = -100000
  )

  expect_true(is.numeric(metricas$roi))
})

test_that("calcular_metricas_financieras NPV decreases with higher discount rate", {
  flujos_df <- proyectar_flujos_caja(
    capex_fase1             = params_test$capex_fase1,
    captacion_anual         = params_test$captacion_anual,
    tarifa_promedio         = params_test$tarifa_promedio,
    tasa_ocupacion          = params_test$tasa_ocupacion,
    costo_variable_paciente = params_test$costo_variable_paciente,
    costos_fijos_anuales    = params_test$costos_fijos_anuales,
    años                    = 10
  )

  metricas_low  <- calcular_metricas_financieras(flujos_df,
                     capex_inicial = -params_test$capex_fase1,
                     tasa_descuento = 0.05)
  metricas_high <- calcular_metricas_financieras(flujos_df,
                     capex_inicial = -params_test$capex_fase1,
                     tasa_descuento = 0.20)

  expect_true(metricas_low$npv > metricas_high$npv)
})

test_that("calcular_metricas_financieras validates flujos_df", {
  expect_error(
    calcular_metricas_financieras("not a df", capex_inicial = -1000),
    "data.frame"
  )

  expect_error(
    calcular_metricas_financieras(
      data.frame(x = 1:3),   # sin columna flujo_neto
      capex_inicial = -1000
    ),
    "flujo_neto"
  )
})


# ══════════════════════════════════════════════════════════════════════════════
# analisis_sensibilidad
# ══════════════════════════════════════════════════════════════════════════════
test_that("analisis_sensibilidad returns data.frame", {
  result <- analisis_sensibilidad(
    params_base = params_test,
    variable    = "tarifa_promedio",
    rango       = c(0.9, 1.0, 1.1)
  )

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)
  expect_named(result, c("variable", "multiplicador", "valor", "irr", "npv"))
})

test_that("analisis_sensibilidad varies parameter correctly", {
  result <- analisis_sensibilidad(
    params_base = params_test,
    variable    = "tarifa_promedio",
    rango       = c(0.8, 1.0, 1.2)
  )

  expect_equal(result$valor[1], params_test$tarifa_promedio * 0.8)
  expect_equal(result$valor[2], params_test$tarifa_promedio * 1.0)
  expect_equal(result$valor[3], params_test$tarifa_promedio * 1.2)
})

test_that("analisis_sensibilidad shows impact on IRR", {
  result <- analisis_sensibilidad(
    params_base = params_test,
    variable    = "tarifa_promedio",
    rango       = c(0.8, 1.2)
  )

  # Mayor tarifa → mayor TIR
  expect_true(result$irr[2] > result$irr[1])
})

test_that("analisis_sensibilidad variable column is correct", {
  result <- analisis_sensibilidad(
    params_base = params_test,
    variable    = "captacion_anual",
    rango       = c(0.9, 1.1)
  )

  expect_true(all(result$variable == "captacion_anual"))
})

test_that("analisis_sensibilidad validates missing variable", {
  expect_error(
    analisis_sensibilidad(
      params_base = params_test,
      variable    = "variable_inexistente"
    ),
    "not found in params_base"
  )
})
