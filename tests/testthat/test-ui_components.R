# tests/testthat/test-ui_components.R
# Tests para R/utils/ui_components.R
# ══════════════════════════════════════════════════════════════════════════════

library(testthat)
library(shiny)

# Cargar el módulo bajo prueba
source(
  system.file("../../R/utils/ui_components.R", package = "base"),
  local = FALSE
)
# Fallback: ruta relativa desde la raíz del proyecto
tryCatch(
  source(file.path(dirname(dirname(getwd())), "R", "utils", "ui_components.R")),
  error = function(e) {
    # En entornos de testthat la raíz es el directorio del proyecto
    source(file.path(rprojroot::find_root(rprojroot::is_r_package,
                                          path = "."), "R", "utils", "ui_components.R"))
  }
)

# ── Helpers ────────────────────────────────────────────────────────────────────
html_str <- function(x) as.character(x)

# ══════════════════════════════════════════════════════════════════════════════
# kpi_card
# ══════════════════════════════════════════════════════════════════════════════

test_that("kpi_card devuelve HTML válido con label y value", {
  result <- kpi_card("Población", "191,031")
  expect_s3_class(result, "html")
  html <- html_str(result)
  expect_match(html, 'class="kpi-card"')
  expect_match(html, 'class="kpi-label"')
  expect_match(html, 'class="kpi-value"')
  expect_match(html, "Población")
  expect_match(html, "191,031")
})

test_that("kpi_card formatea números con separador de miles", {
  result <- kpi_card("Habitantes", 191031)
  html <- html_str(result)
  expect_match(html, "191,031")
  # No debe aparecer el número crudo sin formatear
  expect_false(grepl(">191031<", html))
})

test_that("kpi_card formatea número cero", {
  result <- kpi_card("Sin datos", 0)
  html <- html_str(result)
  expect_match(html, "0")
})

test_that("kpi_card formatea números decimales", {
  result <- kpi_card("Tasa", 4.2)
  html <- html_str(result)
  expect_match(html, "4.2")
})

test_that("kpi_card incluye subtitle cuando se proporciona", {
  result <- kpi_card("Ocupación", "87%", sub = "+3 pp vs año anterior")
  html <- html_str(result)
  expect_match(html, 'class="kpi-sub"')
  expect_match(html, "\\+3 pp vs año anterior")
})

test_that("kpi_card aplica sub_cls al subtitle", {
  result <- kpi_card("Métrica", "100", sub = "creciendo", sub_cls = "kpi-up")
  html <- html_str(result)
  expect_match(html, 'class="kpi-sub kpi-up"')
})

test_that("kpi_card NO incluye subtitle cuando sub es NULL", {
  result <- kpi_card("Label", "Valor")
  html <- html_str(result)
  expect_false(grepl("kpi-sub", html))
})

test_that("kpi_card incluye icono cuando se proporciona", {
  result <- kpi_card("Camas", 120, icon = "hospital")
  html <- html_str(result)
  expect_match(html, 'class="kpi-icon"')
})

test_that("kpi_card NO incluye icono cuando icon es NULL", {
  result <- kpi_card("Label", "Valor")
  html <- html_str(result)
  expect_false(grepl("kpi-icon", html))
})

test_that("kpi_card acepta value character sin modificarlo", {
  result <- kpi_card("Estado", "Activo")
  html <- html_str(result)
  expect_match(html, "Activo")
})

# ══════════════════════════════════════════════════════════════════════════════
# bench_card
# ══════════════════════════════════════════════════════════════════════════════

test_that("bench_card devuelve HTML válido con value y label", {
  result <- bench_card("4.2", "Camas / 1,000 hab.")
  expect_s3_class(result, "html")
  html <- html_str(result)
  expect_match(html, 'class="bench-card"')
  expect_match(html, 'class="bval"')
  expect_match(html, 'class="blbl"')
  expect_match(html, "4\\.2")
  expect_match(html, "Camas / 1,000 hab\\.")
})

test_that("bench_card usa color por defecto #e2e8f0", {
  result <- bench_card("3.8", "Promedio")
  html <- html_str(result)
  expect_match(html, "color:#e2e8f0")
})

test_that("bench_card aplica color personalizado", {
  result <- bench_card("4.2", "Meta", color = "#38bdf8")
  html <- html_str(result)
  expect_match(html, "color:#38bdf8")
})

test_that("bench_card formatea números con separador de miles", {
  result <- bench_card(1500000, "Consultas anuales")
  html <- html_str(result)
  expect_match(html, "1,500,000")
  expect_false(grepl(">1500000<", html))
})

test_that("bench_card incluye subtitle cuando se proporciona", {
  result <- bench_card("4.2", "Camas", sub = "OCDE 2023")
  html <- html_str(result)
  expect_match(html, 'class="bsub"')
  expect_match(html, "OCDE 2023")
})

test_that("bench_card NO incluye subtitle cuando sub es NULL", {
  result <- bench_card("4.2", "Camas")
  html <- html_str(result)
  expect_false(grepl("bsub", html))
})

test_that("bench_card acepta value character sin modificarlo", {
  result <- bench_card("N/D", "Sin datos disponibles")
  html <- html_str(result)
  expect_match(html, "N/D")
})

# ══════════════════════════════════════════════════════════════════════════════
# page_header
# ══════════════════════════════════════════════════════════════════════════════

test_that("page_header devuelve un objeto shiny tag", {
  result <- page_header("ANÁLISIS", "Título de Sección", "Subtítulo descriptivo")
  expect_s3_class(result, "shiny.tag")
})

test_that("page_header tiene la estructura CSS correcta", {
  result <- page_header("BADGE", "Título", "Sub")
  html <- html_str(result)
  expect_match(html, 'class="page-section"')
  expect_match(html, 'class="section-badge"')
  expect_match(html, 'class="section-title"')
  expect_match(html, 'class="section-sub"')
})

test_that("page_header renderiza badge, title y sub correctamente", {
  result <- page_header("DEMANDA", "Análisis de Demanda", "Proyecciones 2025")
  html <- html_str(result)
  expect_match(html, "DEMANDA")
  expect_match(html, "Análisis de Demanda")
  expect_match(html, "Proyecciones 2025")
})

test_that("page_header acepta caracteres especiales en badge", {
  result <- page_header("► FASE 1", "Título", "Sub")
  html <- html_str(result)
  expect_match(html, "FASE 1", fixed = FALSE)
})

# ══════════════════════════════════════════════════════════════════════════════
# page_hdr (alias de compatibilidad)
# ══════════════════════════════════════════════════════════════════════════════

test_that("page_hdr es idéntico a page_header", {
  r1 <- page_hdr("BADGE", "Título", "Sub")
  r2 <- page_header("BADGE", "Título", "Sub")
  expect_identical(html_str(r1), html_str(r2))
})

test_that("page_hdr produce HTML válido", {
  result <- page_hdr("SECCIÓN", "Mi Sección", "Descripción")
  html <- html_str(result)
  expect_match(html, 'class="page-section"')
  expect_match(html, "Mi Sección")
})

# ══════════════════════════════════════════════════════════════════════════════
# Edge cases generales
# ══════════════════════════════════════════════════════════════════════════════

test_that("kpi_card maneja valores negativos", {
  result <- kpi_card("Variación", -500)
  html <- html_str(result)
  expect_match(html, "-500")
})

test_that("kpi_card maneja strings vacíos en label", {
  result <- kpi_card("", "100")
  expect_s3_class(result, "html")
})

test_that("bench_card maneja valores negativos", {
  result <- bench_card(-1000, "Déficit")
  html <- html_str(result)
  expect_match(html, "-1,000")
})
