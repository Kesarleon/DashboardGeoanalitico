# Tarea: Extraer componentes UI a módulo reutilizable

## Contexto
Tengo una aplicación Shiny monolítica en `app_canibalizacion_lovable_3.R` (8000+ líneas). Necesito extraer las funciones de UI a un módulo reutilizable siguiendo arquitectura limpia.

## Objetivos
1. Crear `R/utils/ui_components.R` con funciones:
   - `kpi_card()` - tarjetas de KPIs con iconos opcionales
   - `bench_card()` - tarjetas de benchmark con colores
   - `page_header()` - headers de página con badge
   - `info_box()` - cajas de información con tipos (info/warning/success/danger)

2. Crear `tests/testthat/test-ui_components.R` con tests para todas las funciones

3. Actualizar `app_canibalizacion_lovable_3.R` para:
   - Agregar `source("R/utils/ui_components.R")` al inicio
   - ELIMINAR las definiciones originales de estas funciones
   - Mantener todas las llamadas exactamente iguales (backward compatible)

## Especificaciones técnicas

### kpi_card()
```r
#' @param label Character. Label superior
#' @param value Character or numeric. Valor principal (auto-formatear números con comma)
#' @param sub Character. Subtítulo opcional
#' @param sub_cls Character. Clase CSS adicional para subtitle
#' @param icon Character. Nombre de icono bsicons (sin bs_icon() wrapper)
#' @return HTML object
```

### bench_card()
```r
#' @param value Character or numeric. Estadística principal
#' @param label Character. Label de la métrica
#' @param sub Character. Texto descriptivo opcional
#' @param color Character. Color hex del valor (default: "#e2e8f0")
#' @return HTML object
```

### page_header()
```r
#' @param badge Character. Texto del badge (ej: "① ANÁLISIS")
#' @param title Character. Título principal
#' @param subtitle Character. Subtítulo descriptivo
#' @param icon Character. Icono opcional
#' @return shiny div tag
```

### info_box()
```r
#' @param content Character. Contenido HTML
#' @param type Character. Uno de: "info", "warning", "success", "danger"
#' @return HTML div with appropriate styling
```

## Requisitos de calidad
- ✅ Documentación roxygen2 completa en cada función
- ✅ Validación de inputs (stopifnot)
- ✅ Tests unitarios con testthat (coverage > 80%)
- ✅ Formateo automático de números con scales::comma()
- ✅ Backward compatibility: app debe funcionar idéntico

## Archivos a modificar
1. **CREAR**: `R/utils/ui_components.R`
2. **CREAR**: `tests/testthat/test-ui_components.R`
3. **MODIFICAR**: `app_canibalizacion_lovable_3.R` (agregar source, eliminar definiciones viejas)

## Criterios de éxito
- [ ] Archivo `R/utils/ui_components.R` existe con 4 funciones documentadas
- [ ] Tests pasan: `testthat::test_file("tests/testthat/test-ui_components.R")`
- [ ] App corre sin errores: `shiny::runApp("app_canibalizacion_lovable_3.R")`
- [ ] No hay regresiones visuales (componentes se ven igual)

## Notas importantes
- Las definiciones actuales de estas funciones están cerca de las líneas 500-600 de `app_canibalizacion_lovable_3.R`
- Mantener EXACTAMENTE la misma estructura HTML que las funciones actuales
- NO cambiar nombres de clases CSS (kpi-card, bench-card, etc.)