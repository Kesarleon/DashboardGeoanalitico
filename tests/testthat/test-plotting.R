# tests/testthat/test-plotting.R
# Tests para R/utils/plotting.R y R/themes/theme_dark.R
# ══════════════════════════════════════════════════════════════════════════════

library(testthat)
library(plotly)

# Cargar módulos bajo prueba
tryCatch(
  {
    source(file.path(dirname(dirname(getwd())), "R", "utils", "plotting.R"))
    source(file.path(dirname(dirname(getwd())), "R", "themes", "theme_dark.R"))
  },
  error = function(e) {
    root <- rprojroot::find_root(rprojroot::is_r_package, path = ".")
    source(file.path(root, "R", "utils", "plotting.R"))
    source(file.path(root, "R", "themes", "theme_dark.R"))
  }
)

# ══════════════════════════════════════════════════════════════════════════════
# dark_plotly
# ══════════════════════════════════════════════════════════════════════════════

test_that("dark_plotly acepta un objeto plotly y retorna un plotly", {
  p      <- plot_ly(x = 1:10, y = 1:10, type = "scatter")
  result <- dark_plotly(p)
  expect_s3_class(result, "plotly")
})

test_that("dark_plotly aplica paper_bgcolor transparente", {
  p      <- plot_ly(x = 1:5, y = 1:5, type = "scatter")
  result <- dark_plotly(p)
  expect_equal(result$x$layout$paper_bgcolor, "rgba(0,0,0,0)")
})

test_that("dark_plotly aplica plot_bgcolor transparente", {
  p      <- plot_ly(x = 1:5, y = 1:5, type = "scatter")
  result <- dark_plotly(p)
  expect_equal(result$x$layout$plot_bgcolor, "rgba(0,0,0,0)")
})

test_that("dark_plotly aplica configuración de fuente", {
  p      <- plot_ly(x = 1:5, y = 1:5, type = "scatter")
  result <- dark_plotly(p)
  font   <- result$x$layout$font
  expect_equal(font$color,  "#64748b")
  expect_equal(font$size,   10)
  expect_equal(font$family, "Inter")
})

test_that("dark_plotly desactiva la barra de modo (displayModeBar)", {
  p      <- plot_ly(x = 1:5, y = 1:5, type = "scatter")
  result <- dark_plotly(p)
  expect_false(result$x$config$displayModeBar)
})

test_that("dark_plotly acepta parámetros adicionales con ...", {
  p      <- plot_ly(x = 1:5, y = 1:5, type = "scatter")
  result <- dark_plotly(p, title = "Test chart")
  expect_s3_class(result, "plotly")
  expect_equal(result$x$layout$title, "Test chart")
})

# ══════════════════════════════════════════════════════════════════════════════
# PALETTE_PRIMARY
# ══════════════════════════════════════════════════════════════════════════════

test_that("PALETTE_PRIMARY es un vector character de 6 elementos", {
  expect_type(PALETTE_PRIMARY, "character")
  expect_length(PALETTE_PRIMARY, 6)
})

test_that("PALETTE_PRIMARY contiene solo colores hex válidos", {
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", PALETTE_PRIMARY)))
})

# ══════════════════════════════════════════════════════════════════════════════
# PALETTE_EXTENDED
# ══════════════════════════════════════════════════════════════════════════════

test_that("PALETTE_EXTENDED es un vector character de 12 elementos", {
  expect_type(PALETTE_EXTENDED, "character")
  expect_length(PALETTE_EXTENDED, 12)
})

test_that("PALETTE_EXTENDED contiene solo colores hex válidos", {
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", PALETTE_EXTENDED)))
})

test_that("PALETTE_EXTENDED contiene todos los colores de PALETTE_PRIMARY", {
  expect_true(all(PALETTE_PRIMARY %in% PALETTE_EXTENDED))
})

# ══════════════════════════════════════════════════════════════════════════════
# COLOR_SCHEME
# ══════════════════════════════════════════════════════════════════════════════

test_that("COLOR_SCHEME es una lista", {
  expect_type(COLOR_SCHEME, "list")
})

test_that("COLOR_SCHEME contiene todos los keys requeridos", {
  expected_keys <- c("primary", "secondary", "success", "danger",
                     "warning", "info", "purple", "text", "muted")
  expect_true(all(expected_keys %in% names(COLOR_SCHEME)))
})

test_that("COLOR_SCHEME$primary es #f5a623", {
  expect_equal(COLOR_SCHEME$primary, "#f5a623")
})

test_that("COLOR_SCHEME$secondary es #38bdf8", {
  expect_equal(COLOR_SCHEME$secondary, "#38bdf8")
})

# ══════════════════════════════════════════════════════════════════════════════
# get_dark_css
# ══════════════════════════════════════════════════════════════════════════════

test_that("get_dark_css retorna un string (character)", {
  result <- get_dark_css()
  expect_type(result, "character")
  expect_length(result, 1)
})

test_that("get_dark_css contiene la directiva @import url", {
  result <- get_dark_css()
  expect_match(result, "@import url", fixed = TRUE)
})

test_that("get_dark_css contiene clase .kpi-card", {
  result <- get_dark_css()
  expect_match(result, "\\.kpi-card")
})

test_that("get_dark_css contiene clase .navbar", {
  result <- get_dark_css()
  expect_match(result, "\\.navbar")
})

test_that("get_dark_css no está vacío (longitud > 1000 caracteres)", {
  result <- get_dark_css()
  expect_gt(nchar(result), 1000)
})

# ══════════════════════════════════════════════════════════════════════════════
# get_dark_theme
# ══════════════════════════════════════════════════════════════════════════════

test_that("get_dark_theme retorna una lista", {
  result <- get_dark_theme()
  expect_type(result, "list")
})

test_that("get_dark_theme contiene los elementos colors, fonts y sizes", {
  result <- get_dark_theme()
  expect_true(all(c("colors", "fonts", "sizes") %in% names(result)))
})

test_that("get_dark_theme$colors contiene todos los colores de COL", {
  colors        <- get_dark_theme()$colors
  expected_keys <- c("bg", "panel", "panel2", "border", "gold",
                     "cyan", "green", "red", "orange", "purple", "text", "muted")
  expect_true(all(expected_keys %in% names(colors)))
})

test_that("get_dark_theme$colors$gold es #f5a623", {
  expect_equal(get_dark_theme()$colors$gold, "#f5a623")
})

test_that("get_dark_theme$fonts contiene base y heading", {
  fonts <- get_dark_theme()$fonts
  expect_true(all(c("base", "heading") %in% names(fonts)))
})

test_that("get_dark_theme$sizes contiene base_font, small_font y large_font", {
  sizes <- get_dark_theme()$sizes
  expect_true(all(c("base_font", "small_font", "large_font") %in% names(sizes)))
})
