# ══════════════════════════════════════════════════════════════════════════════
# tests/testthat/test-mod_capacity_calculator.R
# Tests for R/modules/mod_capacity_calculator.R
#
# Coverage:
#   - mod_capacity_calculator_ui()   : structure, ns() isolation, output IDs
#   - mod_capacity_calculator_server(): reactive logic, outputs, return value
# ══════════════════════════════════════════════════════════════════════════════

library(testthat)
library(shiny)

source(here::here("R/models/capacity_model.R"))
source(here::here("R/utils/ui_components.R"))
source(here::here("R/modules/mod_capacity_calculator.R"))


# ══════════════════════════════════════════════════════════════════════════════
# 1. mod_capacity_calculator_ui
# ══════════════════════════════════════════════════════════════════════════════

test_that("mod_capacity_calculator_ui returns a shiny tag object", {
  ui <- mod_capacity_calculator_ui("test")
  expect_true(inherits(ui, "shiny.tag") || inherits(ui, "shiny.tag.list"))
})

test_that("mod_capacity_calculator_ui applies namespace to all input IDs", {
  ui_html <- as.character(mod_capacity_calculator_ui("myns"))

  expect_match(ui_html, "myns-poblacion")
  expect_match(ui_html, "myns-tasa_egresos")
  expect_match(ui_html, "myns-dias_estancia")
  expect_match(ui_html, "myns-ocupacion")
  expect_match(ui_html, "myns-k_calibracion")
})

test_that("mod_capacity_calculator_ui applies namespace to all output IDs", {
  ui_html <- as.character(mod_capacity_calculator_ui("myns"))

  expect_match(ui_html, "myns-kpi_camas")
  expect_match(ui_html, "myns-kpi_consultorios")
  expect_match(ui_html, "myns-kpi_quirofanos")
  expect_match(ui_html, "myns-kpi_uci")
  expect_match(ui_html, "myns-kpi_camas_por_1000")
  expect_match(ui_html, "myns-kpi_indice_rotacion")
  expect_match(ui_html, "myns-kpi_ucin")
  expect_match(ui_html, "myns-kpi_urgencias")
  expect_match(ui_html, "myns-tabla_especialidades")
  expect_match(ui_html, "myns-bench_cards")
  expect_match(ui_html, "myns-tabla_estandares")
  expect_match(ui_html, "myns-desarrollo_fases")
})

test_that("mod_capacity_calculator_ui namespaces are isolated across instances", {
  ui_a <- as.character(mod_capacity_calculator_ui("inst_a"))
  ui_b <- as.character(mod_capacity_calculator_ui("inst_b"))

  expect_match(ui_a, "inst_a-poblacion")
  expect_match(ui_b, "inst_b-poblacion")
  expect_false(grepl("inst_b", ui_a))
  expect_false(grepl("inst_a", ui_b))
})


# ══════════════════════════════════════════════════════════════════════════════
# 2. mod_capacity_calculator_server — reactive logic (via testServer)
# ══════════════════════════════════════════════════════════════════════════════

test_that("mod_capacity_calculator_server returns list with required reactives", {
  testServer(mod_capacity_calculator_server, {
    result <- session$returned

    expect_type(result, "list")
    expect_true("capacidad_results" %in% names(result))
    expect_true("camas_censables"   %in% names(result))
    expect_true(is.reactive(result$capacidad_results))
    expect_true(is.reactive(result$camas_censables))
  })
})

test_that("capacidad_results calls calcular_capacidad_hospitalaria correctly", {
  testServer(mod_capacity_calculator_server, {
    session$setInputs(
      poblacion      = 191031,
      tasa_egresos   = 80,
      dias_estancia  = 4.2,
      ocupacion      = 0.85,
      k_calibracion  = 1.0
    )

    cap <- session$returned$capacidad_results()

    expect_type(cap, "list")
    expect_true(!is.null(cap$camas_censables))
    expect_true(cap$camas_censables > 0)
    expect_true(!is.null(cap$consultorios))
    expect_true(!is.null(cap$quirofanos))
    expect_true(!is.null(cap$camas_uci))
    expect_true(!is.null(cap$camas_ucin))
    expect_true(!is.null(cap$camas_urgencias))
    expect_true(!is.null(cap$metricas))
  })
})

test_that("tasa_egresos is correctly converted from per-1000 to decimal", {
  testServer(mod_capacity_calculator_server, {
    session$setInputs(
      poblacion = 191031, tasa_egresos = 80,
      dias_estancia = 4.2, ocupacion = 0.85, k_calibracion = 1.0
    )
    cap_80 <- session$returned$capacidad_results()$camas_censables

    session$setInputs(tasa_egresos = 40)
    cap_40 <- session$returned$capacidad_results()$camas_censables

    expect_gt(cap_80, cap_40)
  })
})

test_that("camas_censables reactive matches capacidad_results$camas_censables", {
  testServer(mod_capacity_calculator_server, {
    session$setInputs(
      poblacion = 191031, tasa_egresos = 80,
      dias_estancia = 4.2, ocupacion = 0.85, k_calibracion = 1.0
    )

    expect_equal(
      session$returned$camas_censables(),
      session$returned$capacidad_results()$camas_censables
    )
  })
})

test_that("k_calibracion parameter affects bed count proportionally", {
  testServer(mod_capacity_calculator_server, {
    session$setInputs(
      poblacion = 191031, tasa_egresos = 80,
      dias_estancia = 4.2, ocupacion = 0.85, k_calibracion = 1.0
    )
    camas_base <- session$returned$camas_censables()

    session$setInputs(k_calibracion = 1.5)
    camas_high <- session$returned$camas_censables()

    expect_gt(camas_high, camas_base)
  })
})

test_that("capacidad_results includes metricas with camas_por_1000_hab", {
  testServer(mod_capacity_calculator_server, {
    session$setInputs(
      poblacion = 191031, tasa_egresos = 80,
      dias_estancia = 4.2, ocupacion = 0.85, k_calibracion = 1.0
    )

    metricas <- session$returned$capacidad_results()$metricas
    expect_true(!is.null(metricas$camas_por_1000_hab))
    expect_true(!is.null(metricas$indice_rotacion))
    expect_gt(metricas$camas_por_1000_hab, 0)
    expect_gt(metricas$indice_rotacion, 0)
  })
})
