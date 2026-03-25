library(shiny)
library(leaflet)
library(networkD3)
library(dplyr)
library(tidyr)
library(bslib)
library(bsicons)
library(scales)
library(geosphere)
library(sf)
library(tibble)
library(shinymanager)
library(plotly)
library(ggplot2)

# Módulos UI extraídos
source("R/utils/ui_components.R")
source("R/utils/plotting.R")
source("R/themes/theme_dark.R")
source("R/models/huff_model.R")


# ══════════════════════════════════════════════════════════════════════════════
# CREDENCIALES
# ══════════════════════════════════════════════════════════════════════════════
credentials <- data.frame(
  user     = c("admin", "cliente"),
  password = c("Lupiyo2026", "KesarleonAI"),
  stringsAsFactors = FALSE
)

# ══════════════════════════════════════════════════════════════════════════════
# DATOS FASE 3 — FUENTE DE VERDAD: Pestaña 1 + Diseño Hospital
# ══════════════════════════════════════════════════════════════════════════════
poblacion_base_estudio  <- 191031
teus_anual_2025         <- 3893357
camiones_diarios_mzo    <- 3400
cruceros_2024           <- 19
visitantes_cruceros     <- 47000
meta_oms_camas          <- 2.5

# ── Camas privadas reales (fuente: oferta_actual.rds) ───────────────────────
# Centro Médico San Pablo: 7 · Central Médica Quirúrgica: 7 · Hospital Echauri: 13
camas_privadas_actuales <- 27   # 7 + 7 + 13

# ── Volúmenes reales del Proyecto (Pestaña 1 / Desglose Operativo) ──────────
proy_pacs_anuales    <- 8429    # captación Huff anual
proy_urgencias       <- 2950
proy_consulta_ext    <- 21073
proy_hospitalizacion <- 843
proy_cirugias        <- 337

# ── Diseño físico del hospital (imagen de diseño) ───────────────────────────
hosp_camas_censables <- 80
hosp_consultorios    <- 20
hosp_quirofanos      <- 5    # + 1 sala hemodinámica
hosp_uci             <- 4    # UCI adultos
hosp_ucin            <- 3
hosp_camas_urgencias <- 10
hosp_sillones_dia    <- 6

# ── Factores de calibración — Calculadora de Capacidad ──────────────────────

EGRESOS_BASE     <- round(proy_pacs_anuales * 100 / 1000)
CALC_CAMAS_CRUDO <- ceiling(EGRESOS_BASE * 3.5 / (365 * 0.85))
CALC_QX_CRUDO    <- max(ceiling(EGRESOS_BASE * 0.40 / (4 * 365)), 1)
FACTOR_CAMAS     <- hosp_camas_censables / CALC_CAMAS_CRUDO
FACTOR_QX        <- hosp_quirofanos      / CALC_QX_CRUDO
RATIO_CONS_CAMA  <- 4
FACTOR_CONS      <- (hosp_consultorios * 3 * 16 * 300) / (proy_pacs_anuales * 2.5)
# ══════════════════════════════════════════════════════════════════════════════
# DATOS ANÁLISIS DE MERCADO
# ══════════════════════════════════════════════════════════════════════════════
hospitales_huff <- c("Clínica del Pacífico", "Hospital General", "IMSS Manzanillo",
                     "Nuevo Proyecto", "Star Médica (obra)")
prob_huff       <- c(11, 38, 30, 18, 10)
pacs_anuales    <- c(5000, 14022, 11500, 8429, 3500)

can_etiquetas <- c("Clínica del Pacífico", "Fuga a Colima/GDL", "Hospital General",
                   "IMSS Manzanillo", "Nuevo Proyecto", "Star Médica (obra)")
can_pct       <- c(12, 18, 32, 28, 18, 10)

# ── Tabla de competidores — datos reales oferta_actual (todos los 7 hospitales)
competidores_df <- data.frame(
  Hospital      = c("Hospital General de Manzanillo", "HGZ 10 IMSS Manzanillo",
                    "CH Manzanillo", "Hospital Naval Manzanillo",
                    "Centro Médico San Pablo", "Central Médica Quirúrgica",
                    "Hospital Echauri"),
  Tipo          = c("Público", "Público", "Público", "Público",
                    "Privado", "Privado", "Privado"),
  Camas         = c(56, 93, 20, 25, 7, 7, 13),
  Especialidades = c(7, 10, 4, 9, 9, 10, 9),
  Pacs_reales   = c(1742, 2892, 622, 778, 25, 109, 253),
  Ocup          = c("92%", "95%", "88%", "N/A", "45%", "72%", "68%"),
  Tasa_qx       = c("25%", "28%", "20%", "N/D", "42%", "45%", "38%"),
  Cirugias_dia  = c(
    round(1742 * 0.25 / 365 * 12, 1),   # HGM  ~14.4/día anualizado desde mensual
    round(2892 * 0.28 / 365 * 12, 1),   # IMSS ~26.6
    round(622  * 0.20 / 365 * 12, 1),   # CH   ~4.1
    NA,                                   # Naval — sin dato
    round(25   * 0.42 / 365 * 12, 1),   # San Pablo ~0.3
    round(109  * 0.45 / 365 * 12, 1),   # CMQ  ~1.6
    round(253  * 0.38 / 365 * 12, 1)    # Echauri ~3.2
  ),
  stringsAsFactors = FALSE
)

# ── Hospital Echauri — referencia de benchmarking (pestaña 1) ───────────────
echauri_ref <- list(
  camas         = 13,
  especialidades = 9,
  pacs_reales   = 253,
  ocupacion     = "68%",
  ingresos_est  = "~$4.5 MDP/año",   # estimado por tarifa privada × volumen
  consultorios  = 6,
  quirofanos    = 1,
  personal_med  = 18,                 # FTE estimados
  notas         = c(
    "Único hospital privado con quirófano activo en Manzanillo actualmente",
    "Referencia de tarifa privada local: $1,800–$2,400/día cama",
    "13 camas censables → 253 pacientes reales anuales (ocupación ~68%)",
    "9 especialidades: cirugía general, traumatología, ginecología, pediatría, urgencias y otras",
    "Sin UCI propia; traslada críticos a Colima o Guadalajara",
    "Carece de tomógrafo propio y laboratorio 24h — limitante competitiva"
  )
)

# ══════════════════════════════════════════════════════════════════════════════
# DATOS BRECHA DE SERVICIOS
# ══════════════════════════════════════════════════════════════════════════════
especialidades_v <- c("Medicina Interna","Cirugía General","Pediatría","Ginecología",
                      "Traumatología","Cardiología","Oncología","Nefrología","Urgencias","Oftalmología")
actual_c1000     <- c(2.1, 1.8, 2.3, 2.0, 0.8, 0.5, 0.2, 0.4, 3.2, 1.0)
estandar_c1000   <- c(3.0, 2.5, 2.5, 2.2, 2.0, 1.5, 1.0, 0.8, 3.0, 1.0)
brecha_v         <- actual_c1000 - estandar_c1000
fuga_pct         <- c(22, 18, 12, 13, 35, 42, 65, 28, 3, 5)
fuga_absoluta    <- c(310, 280, 195, 240, 450, 380, 320, 145, 95, 85)  # ← NUEVO
estado_esp       <- c("Crítico","Crítico","Moderado","Moderado","Crítico","Crítico","Crítico","Moderado","Cubierto","Cubierto")

# ══════════════════════════════════════════════════════════════════════════════
# MODELO HOSPITALARIO
# ══════════════════════════════════════════════════════════════════════════════
modelos_hosp_df <- data.frame(
  Modelo    = c("Hospital General","Materno-Infantil","Trauma-Portuario","Hospital Boutique","Complejo Ambulatorio","Escalonado Fase 1-2-3"),
  Camas     = c(120, 60, 45, 25, 0, 80),
  CAPEX     = c("$250M","$300M","$200M","$180M","$120M","$250M"),
  ROI_5a    = c(8.2, 12.1, 14.5, 18.3, 22.1, 15.8),
  ROI_10a   = c(18.5, 24.2, 28.7, 35.2, 42.5, 32.9),
  Brecha_cv = c(12, 23, 41, 18, 21, 55),
  Riesgo    = c("Medio","Bajo","Bajo","Bajo","Bajo","Bajo"),
  stringsAsFactors = FALSE
)

camas_servicio_df <- data.frame(
  Servicio  = c("Medicina Interna","Gineco-Obstetricia","Cirugía General",
                "Pediatría","Traumatología / Ortopedia","UCI Adultos",
                "Terapia Neonatal (UCIN)","Cuidados Intermedios"),
  Camas     = c(20, 17, 13, 10, 10, 4, 3, 3),
  stringsAsFactors = FALSE
)

anos_fin <- 1:10

# ══════════════════════════════════════════════════════════════════════════════
# RECLUTAMIENTO REGIONAL — datos por especialidad y origen
# ══════════════════════════════════════════════════════════════════════════════
reclu_df <- data.frame(
  Especialidad = c(
    "Anestesiología",
    "Medicina Crítica (UCI)",
    "Neonatología / UCIN",
    "Cardiología Intervencionista",
    "Cirujano Cardiovascular",
    "Traumatología Alta Complejidad",
    "Neurocirugía",
    "Oncología Médica",
    "Radiología Intervencionista",
    "Hematología"
  ),
  FTE       = c(4, 2, 3, 2, 1, 2, 1, 2, 1, 1),
  Origen    = c(
    "Colima / Guadalajara",
    "Hospital Civil Guadalajara",
    "Cd. Guzmán / Guadalajara",
    "CMNO / Hospital del Carmen GDL",
    "CMNO Guadalajara",
    "Colima / Centro Médico GDL",
    "Hospital Civil / IMSS GDL",
    "Centro Oncológico GDL / INCan",
    "Hospital Ángeles GDL",
    "Guadalajara / Colima"
  ),
  Modalidad = c(
    "Guardia activa 24/7 rotativo",
    "Planta 24/7 rotativo (2 turnos)",
    "Guardia activa 24h (3 Médicos)",
    "Planta + guardia semanal",
    "Por evento (≥2 procedimientos/sem.)",
    "Guardia fin de semana",
    "Por evento (≥2 cirugías/sem.)",
    "Planta Fase 3 / Visita Fase 1-2",
    "Por evento (3 sesiones/sem.)",
    "Visita quincenal"
  ),
  Urgencia  = c("Alta","Alta","Alta","Alta","Media","Alta","Media","Media","Media","Baja"),
  Fase      = c("Fase 1","Fase 1","Fase 2","Fase 2","Fase 2","Fase 1","Fase 2","Fase 3","Fase 2","Fase 3"),
  stringsAsFactors = FALSE
)

# ══════════════════════════════════════════════════════════════════════════════
# REFERENCIA DE MÉDICOS POR CIUDAD Y ESPECIALIDAD — Pestaña 2
# Fuente: estimaciones basadas en densidad médica CONAMED/SSA 2023
# Ciudades candidatas de reclutamiento: Colima, Cd. Guzmán, Guadalajara, Tepic
# ══════════════════════════════════════════════════════════════════════════════
medicos_ciudades_df <- data.frame(
  Especialidad = c(
    "Anestesiología", "Medicina Interna", "Cirugía General",
    "Traumatología/Ortopedia", "Ginecología/Obstetricia", "Pediatría",
    "Medicina Crítica (UCI)", "Neonatología", "Cardiología",
    "Cardiología Intervencionista", "Oncología Médica", "Nefrología",
    "Radiología/Imagen", "Neurocirugía", "Urgencias (médico)"
  ),
  Manzanillo_actual = c(3, 4, 5, 2, 6, 4, 0, 0, 1, 0, 0, 0, 2, 0, 6),
  Colima_cap        = c(18, 22, 20, 12, 24, 18, 4, 3, 8, 1, 3, 4, 10, 2, 20),
  Ciudad_Guzman     = c(8,  10,  9,  6, 12,  9, 1, 1, 3, 0, 1, 1,  4, 1,  9),
  Guadalajara       = c(210,280,260,180,310,240,85,60,140,35,90,70,200,45,280),
  Villa_Alvarez     = c(7,  9,  8,  4, 10,  7, 1, 1, 3, 0, 1, 1,  4, 0,  8),
  Tecoman           = c(3,  4,  3,  2,  5,  3, 0, 0, 1, 0, 0, 0,  2, 0,  3),
  Deficit_proyecto  = c(1, 0, 0, 1, 0, 0, 2, 3, 2, 2, 2, 1, 0, 1, 0),
  Prioridad         = c("Alta","Media","Media","Alta","Baja","Baja",
                        "Alta","Alta","Alta","Alta","Alta","Media","Baja","Media","Baja"),
  stringsAsFactors = FALSE
)

# ══════════════════════════════════════════════════════════════════════════════
# CURVAS DE RAMP-UP
# ══════════════════════════════════════════════════════════════════════════════
RAMP_CURVES <- list(
  "Rápido"      = c(0.32, 0.52, 0.65, 0.74, 0.80, 0.83, 0.85, 0.87, 0.88, 0.89),
  "Moderado"    = c(0.25, 0.42, 0.57, 0.68, 0.76, 0.81, 0.84, 0.86, 0.88, 0.89),
  "Conservador" = c(0.18, 0.30, 0.42, 0.54, 0.63, 0.70, 0.75, 0.80, 0.83, 0.86)
)

# ══════════════════════════════════════════════════════════════════════════════
# FUNCIÓN IRR (bisección)
# ══════════════════════════════════════════════════════════════════════════════
calcular_irr <- function(flujos, r_min = -0.5, r_max = 5, tol = 1e-6, max_iter = 200) {
  npv_fn <- function(r) sum(flujos / (1 + r)^(seq_along(flujos) - 1))
  if (is.na(npv_fn(r_min)) || is.na(npv_fn(r_max))) return(NA_real_)
  if (npv_fn(r_min) * npv_fn(r_max) > 0) return(NA_real_)
  for (i in seq_len(max_iter)) {
    r_mid <- (r_min + r_max) / 2
    if (abs(r_max - r_min) < tol) return(round(r_mid * 100, 2))
    if (npv_fn(r_min) * npv_fn(r_mid) < 0) r_max <- r_mid else r_min <- r_mid
  }
  round((r_min + r_max) / 2 * 100, 2)
}

# ══════════════════════════════════════════════════════════════════════════════
# DATOS DE ARCHIVOS
# ══════════════════════════════════════════════════════════════════════════════
if (file.exists("insumos_canibalizacion.rds")) {
  insumos           <- readRDS("insumos_canibalizacion.rds")
  oferta_actual     <- insumos$oferta_actual
  proyectos_futuros <- insumos$proyectos_futuros
  demanda           <- insumos$demanda
  poligonos_mapa    <- insumos$poligonos_mapa
  hex_data          <- insumos$hex_data
} else {
  oferta_actual     <- NULL
  proyectos_futuros <- NULL
  demanda           <- NULL
  poligonos_mapa    <- NULL
  hex_data          <- NULL
}

oferta_actual
proyectos_futuros

if (file.exists("denue_salud.rds")) {
  denue_salud <- readRDS("denue_salud.rds")
} else {
  denue_salud <- NULL
}

# ══════════════════════════════════════════════════════════════════════════════
# ══════════════════════════════════════════════════════════════════════════════
# UI HELPERS → movidos a R/utils/ui_components.R

# ══════════════════════════════════════════════════════════════════════════════
# UI
# ══════════════════════════════════════════════════════════════════════════════
ui_dash <- page_navbar(
  title    = "Factibilidad Hospitalaria · Manzanillo",
  theme    = bs_theme(bootswatch = "flatly"),
  position = "fixed-top",
  fillable = FALSE,
  
  header = tags$head(tags$style(HTML(get_dark_css()))),
  
  # ── 1. Simulación de Mercado ────────────────────────────────────────────────
  nav_panel(
    title = "Simulación de Mercado",
    layout_sidebar(
      fillable = FALSE,
      sidebar = sidebar(
        width = 320,
        title = tags$h4("Panel de Simulación", class = "sidebar-title", style = "margin-top:0;"),
        tags$p("Ajusta las palancas del mercado para evaluar diferentes escenarios.", style = "color:#64748b; font-size:11px;"),
        tags$hr(),
        tags$h5("1. Escenarios de Competencia"),
        tags$small("Proyectos que entrarán al mercado:", style = "color:#64748b;"),
        checkboxGroupInput("proyectos_activos", NULL,
                           choices  = c("Nuestro Proyecto " = "PROYECTO",
                                        "Competidor: La Joya      (48 camas)" = "COMP_CONST_1",
                                        "Competidor: Los Ángeles  (50 camas)" = "COMP_CONST_2"),
                           selected = c("PROYECTO", "COMP_CONST_1", "COMP_CONST_2")),
        tags$hr(),
        tags$h5("2. Comportamiento del Paciente"),
        sliderInput("sensibilidad", "Sensibilidad a la Distancia (λ):", min = 0.1, max = 3, value = 0.5, step = 0.1),
        tags$small("Mayor valor = priorizan cercanía.", style = "color:#64748b;"),
        tags$hr(),
        conditionalPanel(
          condition = "input.proyectos_activos.includes('PROYECTO')",
          tags$h5("3. Capacidad del Proyecto"),
          numericInput("camas_proy", "Camas Censables:", value = 80),
          numericInput("esp_proy",   "Especialidades Médicas:", value = 8)
        )
      ),
      card(fill = FALSE,
           card_header(tags$h4(bsicons::bs_icon("geo-alt-fill"), " Entorno Geográfico y Densidad de Demanda")),
           card_body(
             tags$p("Visualización de la oferta hospitalaria y probabilidad de captura de pacientes.", style = "color:#64748b; font-size:11px;"),
             leafletOutput("mapa", height = "520px")
           )
      ),
      card(fill = FALSE,
           card_header(tags$h4(bsicons::bs_icon("shuffle"), " Dinámica de Competencia y Canibalización")),
           card_body(
             tags$p("Flujo de canibalización entre hospitales al incorporar nuevos proyectos.", style = "color:#64748b; font-size:11px;"),
             sankeyNetworkOutput("sankey_canibal", height = "340px")
           )
      ),
      
      # ── Hospital Echauri: referencia de benchmarking ──
      card(fill = FALSE,
           card_header(tags$h4(bsicons::bs_icon("building"), " Hospital Echauri — Referencia Privada Local")),
           card_body(
             div(class = "echauri-box",
                 div(class = "etitle",
                     "🏥 Echauri es el único privado con quirófano activo en Manzanillo · Benchmark clave para tarifas y captación"),
                 div(class = "ebody",
                     HTML('Análisis comparativo con el principal competidor privado existente.
                     Echauri opera con <strong>13 camas · 9 especialidades · 253 pacientes/año</strong>.
                     Su ocupación del 68% con esa escala confirma la demanda privada reprimida
                     y proyecta el potencial del nuevo hospital (80 camas, ×6 el tamaño).'))
             ),
             layout_column_wrap(width = 1/2,
                                div(
                                  tags$h5("Echauri — Métricas Actuales",
                                          style = "color:#c084fc; font-size:11px; font-weight:700; margin-bottom:10px;"),
                                  HTML('
                  <div style="display:flex;flex-wrap:wrap;gap:6px;margin-bottom:12px;">
                    <div class="echauri-metric"><span>🛏</span> 13 camas censables</div>
                    <div class="echauri-metric"><span>🩺</span> 9 especialidades</div>
                    <div class="echauri-metric"><span>👥</span> 3,036 pacientes/año</div>
                    <div class="echauri-metric"><span>📊</span> 68% ocupación est.</div>
                    <div class="echauri-metric"><span>🔪</span> 1 quirófano</div>
                    <div class="echauri-metric"><span>💊</span> Sin UCI · Sin tomógrafo</div>
                    <div class="echauri-metric"><span>💰</span> ~$4.5 MDP ingresos/año</div>
                    <div class="echauri-metric"><span>🏷</span> Tarifa: $1,800–$2,400/día</div>
                  </div>
                  <div class="info-box"><p>
                    <strong>Brechas que Echauri no cubre</strong> y el Nuevo Proyecto sí cubrirá:
                    UCI adultos · UCIN · Sala hemodinámica · Tomógrafo · Oncología ambulatoria ·
                    Cardiología intervencionista · Laboratorio 24h.
                    Estas brechas representan la <strong>ventaja competitiva estructural</strong> del proyecto.
                  </p></div>')
                                ),
                                div(
                                  tags$h5("Proyección Comparativa: Echauri vs Nuevo Proyecto",
                                          style = "color:#e2e8f0; font-size:11px; font-weight:700; margin-bottom:10px;"),
                                  plotlyOutput("echauri_comp", height = "220px")
                                )
             )
           )
      ),
      
      card(fill = FALSE, style = "margin-bottom:30px;",
           card_header(tags$h4(bsicons::bs_icon("clipboard-data"), " Proyecciones Finales de Mercado")),
           card_body(
             layout_column_wrap(width = 1/2,
                                div(tags$h5("Estimaciones Anuales de Mercado", style = "color:#e2e8f0; font-size:12px;"),
                                    tableOutput("tabla_resumen")),
                                div(tags$h5("Desglose Operativo (Nuestro Proyecto)", style = "color:#e2e8f0; font-size:12px;"),
                                    tags$small("Basado en tasas de morbilidad local.", style = "color:#64748b;"), br(),
                                    plotlyOutput("plot_desglose", height = "200px"))
             )
           )
      )
    )
  ),
  
  # ── 2. Calculadora de Capacidad ─────────────────────────────────────────────
  nav_panel(
    title = "Calculadora de Capacidad",
    layout_sidebar(
      fillable = FALSE,
      sidebar = sidebar(
        width = 280,
        title = tags$h4("Parámetros de Diseño", class = "sidebar-title", style = "margin-top:0;"),
        tags$h5("1. Demanda"),
        numericInput("poblacion_objetivo", "Pacientes Anuales (Modelo Huff)", value = 8429, step = 500),
        tags$small("Valor base: Pacientes/año capturados por Huff en Simulación de Mercado.", style = "color:#64748b;"),
        sliderInput("tasa_hosp", "Tasa Hospitalización (Egresos x 1k pac.)", min = 20, max = 150, value = 100),
        numericInput("consultas_paciente", "Frecuencia Consulta Externa (anual)", value = 2.5, step = 0.1),
        tags$hr(),
        tags$h5("2. Eficiencia Hospitalaria"),
        sliderInput("ocupacion_obj", "Índice Ocupación Objetivo (%)", min = 60, max = 95, value = 85),
        sliderInput("estancia_prom", "Estancia Promedio (Días)", min = 1, max = 8, value = 3.5, step = 0.5),
        
        tags$hr(),
        tags$h5("3. Consultorios"),
        sliderInput("pacientes_hora", "Pacientes por Hora", min = 1, max = 6, value = 3),
        radioButtons("turnos_cons", "Turnos Activos",
                     choices = c("1 Turno (8h)" = 1, "2 Turnos (Mat/Vesp - 16h)" = 2), selected = 2),
        numericInput("dias_lab_cons", "Días Laborales Consultorio/Año", value = 300),
        tags$small(HTML("Ratio de industria: <b style='color:#f5a623;'>1 consultorio / 4 camas</b>"), style = "color:#64748b;"),
        tags$hr(),
        tags$h5("4. Salas Quirúrgicas"),
        sliderInput("tasa_qx", "Tasa Intervención Qx (% Egresos)", min = 10, max = 80, value = 40),
        numericInput("cirugias_sala_dia", "Cirugías por Sala/Día", value = 4),
        
        tags$hr(),
        tags$h5("5. Plantilla Médica"),
        sliderInput("ratio_medicos_cama", "Médicos por Cama",
                    min = 1.0, max = 4.0, value = 2.1, step = 0.1),
        tags$small(HTML("Estándar privado MX: <b style='color:#f5a623;'>2.1</b> · Alta complejidad: 2.5–3.0"),
                   style = "color:#64748b;")
      ),
      layout_column_wrap(width = 1/4,
                         value_box(title = "Camas Censables",   value = textOutput("res_camas"),
                                   showcase = bs_icon("hospital"),       theme = "primary",
                                   p("Factor ×0.833 aplicado · diseño: 80")),
                         value_box(title = "Consultorios",       value = textOutput("res_consultorios"),
                                   showcase = bs_icon("postcard-heart"), theme = "info",
                                   p("Ratio 1/4 camas · diseño: 20")),
                         value_box(title = "Salas Quirúrgicas",  value = textOutput("res_quirofanos"),
                                   showcase = bs_icon("activity"),       theme = "danger",
                                   p("Factor ×1.667 aplicado · diseño: 5")),
                         value_box(title = "Plantilla Médica",   value = textOutput("res_medicos"),
                                   showcase = bs_icon("people"),         theme = "success",
                                   p("Ratio ajustable · default: 2.1 médicos/cama"))
      ),
      card(fill = FALSE,
           card_header(tags$h4(bsicons::bs_icon("geo"), " Red de Soporte Médico")),
           card_body(
             layout_column_wrap(width = 1/2, min_width = 480,
                                div(tags$h5("Distribución Interna (Plantilla Base)", style = "color:#e2e8f0; font-size:12px;"),
                                    tableOutput("tabla_especialidades")),
                                div(tags$h5("Reclutamiento Regional — Especialidades Críticas", style = "color:#e2e8f0; font-size:12px;"),
                                    tags$small("Subespecialidades que no existen en Manzanillo · Origen, modalidad y urgencia.", style = "color:#64748b;"), br(),
                                    DT::DTOutput("tabla_talento_externo"))
             )
           )
      ),
      
      # ── NUEVO: Referencia de médicos por ciudad ──────────────────────────
      card(fill = FALSE, style = "margin-bottom:30px;",
           card_header(tags$h4(bsicons::bs_icon("pin-map-fill"),
                               " Mapa de Talento Regional — Médicos Disponibles por Ciudad y Especialidad")),
           card_body(
             div(class = "info-box",
                 HTML('<p>Estimado de médicos especialistas disponibles en ciudades cercanas
                 (fuente: densidad médica CONAMED/SSA 2023).
                 <strong>Déficit proyecto</strong> = Médicos faltantes para cubrir el diseño hospitalario.
                 Úsalo como guía para priorizar dónde y qué especialidad reclutar.</p>')),
             layout_column_wrap(width = 1/2, min_width = 480,
                                div(
                                  tags$h5("Disponibilidad por ciudad y especialidad", style = "color:#e2e8f0; font-size:12px;"),
                                  DT::DTOutput("tabla_medicos_ciudades")
                                ),
                                div(
                                  tags$h5("Déficit por especialidad vs ciudades de origen", style = "color:#e2e8f0; font-size:12px;"),
                                  plotlyOutput("plot_deficit_medicos", height = "420px")
                                )
             )
           )
      )
    )
  ),
  
  # ── 3. Análisis de Mercado ──────────────────────────────────────────────────
  nav_panel(
    title = "Análisis de Mercado",
    div(style = "padding: 20px 24px;",
        page_hdr("① ANÁLISIS", "Simulación de Mercado — Modelo Huff",
                 "Captación proyectada, canibalización y entorno competitivo · Manzanillo, Colima"),
        
        div(class = "kpi-row",
            kpi_card("POBLACIÓN OBJETIVO",       "191,031",  "Área metropolitana Manzanillo"),
            kpi_card("PACIENTES CAPTADOS",        "8,429",    "▲ Captación anual Huff",    "kpi-up"),
            kpi_card("CAPTACIÓN NUEVO PROYECTO",  "18%",      "Probabilidad Huff"),
            kpi_card("RIESGO CANIBALIZACIÓN",     "Medio",    "",       "kpi-warn")
        ),
        
        div(class = "chart-row",
            div(class = "chart-panel",
                div(class = "chart-title", "Probabilidad de Captación por Hospital (Modelo Huff)"),
                plotlyOutput("am_huff",  height = "210px")),
            div(class = "chart-panel",
                div(class = "chart-title", "Pacientes Anuales Proyectados"),
                plotlyOutput("am_pacs",  height = "210px"))
        ),
        
        div(class = "chart-row",
            div(class = "chart-panel",
                div(class = "chart-title",
                    HTML('<span style="color:#fb923c;">⇄</span> Flujo de Canibalización — Origen y Destino de Pacientes')),
                plotlyOutput("am_can",   height = "230px")),
            div(class = "chart-panel",
                div(class = "chart-title",
                    HTML('<span style="color:#f87171;">●</span> Competidores — Capacidad Real')),
                div(style = "margin-bottom:8px;",
                    HTML('<div class="info-box"><p>
                    Los hospitales públicos operan al <strong>92–95% de ocupación</strong>: señal de demanda insatisfecha.
                    La oferta privada real suma <strong>27 camas</strong> en 3 hospitales
                    (Echauri: 13 · San Pablo: 7 · CMQ: 7).
                    La Joya y Los Ángeles entrarán al mercado, pero la ventana de oportunidad permanece.
                    </p></div>')),
                DT::DTOutput("am_tbl_comp", height = "200px"))
        )
    )
  ),
  
  # ── 4. Brecha de Servicios ──────────────────────────────────────────────────
  nav_panel(
    title = "Brecha de Servicios",
    div(style = "padding: 20px 24px;",
        page_hdr("② DIAGNÓSTICO", "Brecha de Servicios de Salud en Manzanillo",
                 "Comparativo de capacidad instalada vs estándares · Clasificación semáforo por especialidad"),
        
        div(style = "margin-bottom:6px;",
            HTML('<div style="font-size:10px;font-weight:600;color:#64748b;text-transform:uppercase;letter-spacing:.07em;margin-bottom:10px;">
              Camas por 1,000 habitantes — Comparativo de referencia
            </div>')),
        div(class = "bench-row",
            bench_card("3.4",  "Estándar OCDE",          
                       paste0("Promedio países miembros · Faltan ", 
                              scales::comma(ceiling(poblacion_base_estudio * 3.4 / 1000) - sum(competidores_df$Camas)), 
                              " camas"),
                       "#f5a623"),
            bench_card("2.5",  "Meta OMS",                
                       paste0("Mínimo recomendado · Faltan ", 
                              scales::comma(ceiling(poblacion_base_estudio * 2.5 / 1000) - sum(competidores_df$Camas)), 
                              " camas"),
                       "#fb923c"),
            bench_card("1.5",  "Promedio Nacional MX",    
                       paste0("Sistema de salud mexicano · Faltan ", 
                              scales::comma(ceiling(poblacion_base_estudio * 1.5 / 1000) - sum(competidores_df$Camas)), 
                              " camas"),
                       "#38bdf8"),
            bench_card("1.12", "Manzanillo Actual",       
                       paste0("Déficit estructural confirmado · ",
                              sum(competidores_df$Camas), " camas totales"), 
                       "#f87171"),
            bench_card("27",   "Camas Privadas Hoy",      
                       "Echauri 13 + SanPablo 7 + CMQ 7",
                       "#f87171")
        ),
        
        div(class = "kpi-row",
            kpi_card("ESPECIALIDADES CRÍTICAS",   "5",  "Brecha severa — atención urgente", "kpi-down"),
            kpi_card("MODERADAS",                 "3",  "Brecha parcial"),
            kpi_card("CUBIERTAS",                 "2",  "Oferta adecuada",                  "kpi-up"),
            kpi_card("FUGA PROMEDIO",            "24%", "Pacientes que salen de Manzanillo","kpi-warn")
        ),
        
        div(class = "chart-row",
            div(class = "chart-panel",
                div(class = "chart-title", "Radar de Cobertura por Especialidad (Actual vs Estándar Nacional)"),
                plotlyOutput("bs_radar", height = "270px")),
            div(class = "chart-panel",
                div(class = "chart-title", "Fuga de Pacientes Fuera de Manzanillo (% por Especialidad)"),
                plotlyOutput("bs_fuga",  height = "270px"))
        ),
        
        div(class = "chart-panel full",
            div(class = "chart-title",
                HTML('<span style="color:#f5a623;font-size:12px;font-weight:700;">● CLASIFICACIÓN SEMÁFORO — Prioridad de Intervención por Especialidad</span>')),
            div(style = "margin-bottom:8px;",
                HTML('<div class="info-box"><p>
                  <strong>Crítico (rojo)</strong>: Brecha > 30% bajo estándar + fuga significativa.
                  <strong>Moderado (naranja)</strong>: Déficit parcialmente cubierto por sector público.
                  <strong>Cubierto (verde)</strong>: Capacidad suficiente.
                </p></div>')),
            DT::DTOutput("bs_tbl")
        )
    )
  ),
  
  # ── 5. Modelo Hospitalario ──────────────────────────────────────────────────
  nav_panel(
    title = "Modelo Hospitalario",
    div(style = "padding: 20px 24px;",
        page_hdr("③ DISEÑO", "Configurador de Modelo Hospitalario",
                 "Resultado de síntesis del estudio — selecciona el tipo de hospital para ver dimensionamiento y financiero"),
        
        div(style = "margin-bottom:14px;",
            HTML('<div class="info-box"><p>
              El modelo hospitalario óptimo surge de cruzar tres variables: <strong>brecha de servicios detectada</strong>,
              <strong>captación Huff proyectada</strong> y <strong>perfil de riesgo-retorno</strong> aceptable para el inversor.
              El <strong>Escalonado Fase 1-2-3</strong> maximiza la cobertura de brecha (55%) con el menor riesgo relativo.
            </p></div>')),
        
        uiOutput("mh_tabs_ui"),
        uiOutput("mh_content")
    )
  ),
  
  # ── 6. Modelo Financiero ────────────────────────────────────────────────────
  nav_panel(
    title = "Modelo Financiero",
    layout_sidebar(
      fillable = FALSE,
      sidebar = sidebar(
        width = 300,
        title = tags$h4("Parámetros del Escenario", class = "sidebar-title", style = "margin-top:0;"),
        tags$p("Ajusta los supuestos para generar proyecciones a 10 años.", style = "color:#64748b; font-size:11px;"),
        tags$hr(),
        tags$h5("① Capacidad Instalada"),
        numericInput("fin_camas",      "Camas censables",     value = 80,  min = 10, max = 300),
        numericInput("fin_qx",         "Quirófanos activos",   value = 5,   min = 1,  max = 20),
        sliderInput( "fin_ocup",       "Ocupación objetivo (%)", min = 40, max = 90, value = 70, step = 5),
        tags$hr(),
        tags$h5("② Tarifas por Servicio (MXN)"),
        numericInput("fin_tarifa_cama",  "Tarifa cama / día",      value = 2800,  step = 100),
        numericInput("fin_tarifa_qx",    "Tarifa cirugía (hosp.)", value = 18000, step = 500),
        numericInput("fin_tarifa_urg",   "Tarifa urgencias",       value = 3500,  step = 100),
        numericInput("fin_tarifa_cons",  "Tarifa consulta (inst.)",value = 350,   step = 25),
        numericInput("fin_tarifa_dia",   "Tarifa sillón / sesión", value = 3000,  step = 100),
        tags$hr(),
        tags$h5("③ Estructura de Costos"),
        sliderInput("fin_pct_personal", "Nómina y personal (%)",       min = 25, max = 55, value = 42, step = 1),
        sliderInput("fin_pct_insumos",  "Insumos y serv. médicos (%)", min = 10, max = 30, value = 18, step = 1),
        numericInput("fin_costo_fijo",  "Costos fijos anuales (MDP)",  value = 10, step = 1),
        sliderInput("fin_inf_gen",      "Inflación general (%)",        min = 2, max = 10, value = 4, step = 0.5),
        tags$hr(),
        tags$h5("④ Inversión y Descuento"),
        numericInput("fin_capex", "CAPEX total Fase 1 (MDP)", value = 250, step = 10),
        sliderInput("fin_wacc",   "WACC / Tasa descuento (%)", min = 8, max = 22, value = 12, step = 0.5),
        sliderInput("fin_g_vol",    "Crec. volumen anual (%)", min = 0, max = 12, value = 4, step = 0.5),
        sliderInput("fin_g_precio", "Inflación médica (%)",    min = 2, max = 12, value = 6, step = 0.5),
        radioButtons("fin_ramp", "Velocidad de ramp-up",
                     choices = c("Rápido (turismo + puerto)" = "Rápido",
                                 "Moderado (base)" = "Moderado",
                                 "Conservador (alta competencia)" = "Conservador"),
                     selected = "Moderado")
      ),
      
      div(style = "padding: 4px 8px;",
          page_hdr("⑤ FASE 3", "Modelo Financiero Integrado",
                   "Proyección a 10 años — Escenario base: Hospital Escalonado Fase 1-2-3"),
          
          layout_column_wrap(width = 1/4,
                             value_box(title = "CAPEX TOTAL",         value = textOutput("fin_kpi_capex"),
                                       showcase = bs_icon("cash-stack"),     theme = "primary",
                                       tags$small("Fase 1 inicial")),
                             value_box(title = "OPEX AÑO 1",          value = textOutput("fin_kpi_opex1"),
                                       showcase = bs_icon("gear-wide"),      theme = "danger",
                                       tags$small("Operación anual")),
                             value_box(title = "PUNTO DE EQUILIBRIO", value = textOutput("fin_kpi_be"),
                                       showcase = bs_icon("calendar-check"), theme = "warning",
                                       tags$small("Escenario base")),
                             value_box(title = "TIR PROYECTADA",      value = textOutput("fin_kpi_tir"),
                                       showcase = bs_icon("graph-up-arrow"), theme = "success",
                                       tags$small("▲ vs 12% benchmark · 10 años"))
          ),
          
          div(class = "chart-row", style = "margin-top:14px;",
              div(class = "chart-panel",
                  div(class = "chart-title", "Ingresos vs OPEX (MDP)"),
                  plotlyOutput("fin_ingresos", height = "220px")),
              div(class = "chart-panel",
                  div(class = "chart-title", "Flujo Acumulado Descontado (MDP)"),
                  plotlyOutput("fin_flujo", height = "220px"))
          ),
          
          div(class = "chart-panel full",
              div(class = "chart-title", "EBITDA Proyectado (MDP)"),
              plotlyOutput("fin_ebitda", height = "200px")
          ),
          br(),
          
          div(class = "chart-panel full",
              div(class = "chart-title",
                  HTML('<span style="color:#f5a623;">⊞</span> Análisis de Sensibilidad — 5 Escenarios')),
              div(style = "margin-bottom:8px;",
                  HTML('<div class="info-box"><p>Cada escenario ajusta la ocupación y las tarifas respecto al caso base.
                  <strong>Todos los resultados</strong> se recalculan en tiempo real.</p></div>')),
              DT::DTOutput("fin_sens")
          )
      )
    )
  ),
  
  # ── 7. Recomendación Ejecutiva ──────────────────────────────────────────────
  nav_panel(
    title = "Recomendación",
    div(style = "padding: 20px 24px;",
        div(style = "display:flex; justify-content:space-between; align-items:flex-start; flex-wrap:wrap; gap:12px;",
            page_hdr("④ CONCLUSIÓN", "Recomendación Ejecutiva",
                     "Resumen para comité de inversión — Todo lo que el proyecto conlleva · Manzanillo, Colima"),
            tags$button("⬇ Exportar PDF",
                        style = paste0("background:#f5a623; color:#000; border:none; border-radius:6px;",
                                       "padding:9px 18px; font-size:11px; font-weight:700; cursor:pointer;",
                                       "font-family:Inter,sans-serif; margin-top:30px;"))
        ),
        
        div(class = "verdict-box",
            div(class = "verdict-title", "✓ Veredicto: Viable con Estrategia Escalonada"),
            div(class = "verdict-body",
                HTML('El análisis integral indica que la construcción de un
          <strong>Hospital Escalonado en 3 fases</strong> en Manzanillo es viable
          con un ROI proyectado del <strong>32.9% a 10 años</strong> y TIR del <strong>15.8%</strong>.
          La estrategia escalonada minimiza el riesgo de sobreoferta mientras capitaliza
          la demanda insatisfecha en traumatología, cardiología y cirugía general.
          Los hospitales públicos operan al 92–95% de ocupación;
          la oferta privada existente son apenas <strong>27 camas</strong>
          (Echauri: 13 · San Pablo: 7 · CMQ: 7) — completamente insuficiente.
          El Modelo Huff proyecta <strong>8,429 pacientes anuales</strong> para el nuevo hospital
          (2,950 urgencias · 21,073 consultas externas · 843 egresos · 337 cirugías).'))
        ),
        
        div(class = "rec-grid",
            div(class = "rec-card",
                div(class = "rec-card-label", "🏥 HOSPITAL RECOMENDADO"),
                div(class = "rec-card-value", "Escalonado Fase 1-2-3")),
            div(class = "rec-card",
                div(class = "rec-card-label", "🔵 CAPACIDAD INSTALADA (DISEÑO CONFIRMADO)"),
                div(class = "rec-card-value",
                    "80 camas censables · 20 consultorios · 5 quirófanos + 1 hemodinámica · 10 camas urgencias · 6 sillones hospital de día"))
        ),
        div(class = "rec-grid",
            div(class = "rec-card",
                div(class = "rec-card-label", "$ NIVEL DE INVERSIÓN TOTAL"),
                div(class = "rec-card-value", "$250 MDP Fase 1 / $650 MDP Total")),
            div(class = "rec-card",
                div(class = "rec-card-label", "⏱ PUNTO DE EQUILIBRIO"),
                div(class = "rec-card-value", "Año 5.2 (escenario base) — Año 4.1 (optimista)"))
        ),
        
        div(class = "rec-grid",
            div(class = "rec-card",
                div(class = "rec-card-label", "★ Especialidades Prioritarias (orden de implementación)"),
                HTML('<ol class="priority-list">
            <li><div class="priority-num">1</div> Traumatología / Ortopedia · fuga 35% — demanda portuaria inmediata (10 camas)</li>
            <li><div class="priority-num">2</div> Gineco-Obstetricia · 17 camas + sala cesáreas — oferta privada inexistente</li>
            <li><div class="priority-num">3</div> Cirugía General · brecha crítica, alto volumen (13 camas)</li>
            <li><div class="priority-num">4</div> Cardiología · fuga 42% — sala hemodinámica como diferenciador regional</li>
            <li><div class="priority-num">5</div> Oncología ambulatoria · 6 sillones hospital de día (quimioterapia / hemodiálisis)</li>
          </ol>')),
            div(class = "rec-card",
                div(class = "rec-card-label", "↗ Ruta de Crecimiento por Fase"),
                HTML('<ul class="strategy-list">
            <li><div class="dot-ring"></div> <strong style="color:#f5a623;">Fase 1 (Año 1-2)</strong>: Trauma/Ortopedia + Cirugía General + Gineco-Obstetricia + Urgencias (40 camas).</li>
            <li><div class="dot-ring"></div> <strong style="color:#f5a623;">Fase 2 (Año 3-4)</strong>: Cardiología + UCI 4 camas + UCIN 3 camas + sala hemodinámica.</li>
            <li><div class="dot-ring"></div> <strong style="color:#f5a623;">Fase 3 (Año 5+)</strong>: Hospital de día completo + 20 consultorios plenos + Certificación JCI para turismo médico.</li>
          </ul>'))
        ),
        
        div(style = "margin-bottom:14px;",
            HTML('<div style="font-size:11px;font-weight:700;color:#e2e8f0;text-transform:uppercase;letter-spacing:.06em;margin-bottom:10px;">
              ⚙ Lo que conlleva — Condiciones de Éxito No Negociables
            </div>')),
        div(class = "cond-grid",
            div(class = "cond-card",
                div(class = "cond-card-label", "📋 Operativo"),
                HTML('<ul class="cond-list">
              <li><span class="cond-icon">→</span> Ocupación mínima sostenida del 65% desde Año 2 para alcanzar breakeven operativo</li>
              <li><span class="cond-icon">→</span> Contratación de al menos 3 especialistas de alta complejidad antes de apertura</li>
              <li><span class="cond-icon">→</span> Plan de reclutamiento regional activo desde Guadalajara y Cd. Guzmán</li>
              <li><span class="cond-icon">→</span> Modelo de guardia activa 24/7 en urgencias desde Día 1</li>
            </ul>')),
            div(class = "cond-card",
                div(class = "cond-card-label", "💰 Financiero"),
                HTML('<ul class="cond-list">
              <li><span class="cond-icon">→</span> CAPEX Fase 1 de $250 MDP comprometido antes del inicio de obra</li>
              <li><span class="cond-icon">→</span> Capital de trabajo para cubrir flujo negativo en Año 1 ($4 MDP déficit proyectado)</li>
              <li><span class="cond-icon">→</span> Estructura de deuda-capital máxima 60/40 para mantener TIR proyectada</li>
              <li><span class="cond-icon">→</span> Revisión de tarifas cada 18 meses vinculada a índice de inflación médica</li>
            </ul>')),
            div(class = "cond-card",
                div(class = "cond-card-label", "⚖ Regulatorio y Estratégico"),
                HTML('<ul class="cond-list">
              <li><span class="cond-icon">→</span> Licencia sanitaria COFEPRIS y habilitación SSA antes de apertura (8-14 meses)</li>
              <li><span class="cond-icon">→</span> Convenio con IMSS/ISSSTE para referencia-contrarreferencia desde Fase 1</li>
              <li><span class="cond-icon">→</span> Certificación JCI en Fase 3 para acceso a turismo médico y cruceros (47k visitantes/año)</li>
              
            </ul>'))
        ),
        
        div(class = "caution-box",
            div(class = "caution-title", "⚠ Riesgos Clave que el Comité Debe Conocer"),
            div(class = "caution-body",
                HTML('<ul class="risk-list">

          <li><span class="risk-dot">●</span> <strong>Talento médico:</strong> Manzanillo carece de especialistas en cardiología intervencionista y oncología.</li>
          <li><span class="risk-dot">●</span> <strong>Escenario pesimista:</strong> Ocupación al 45% + tarifas -15% → breakeven Año 12.3 · TIR 3.7%.</li>
          <li><span class="risk-dot">●</span> <strong>Dependencia del puerto:</strong> Caída portuaria reduce demanda de traumatología (motor Fase 1).</li>
          <li><span class="risk-dot">●</span> <strong>Turismo médico:</strong> JCI necesaria pero no suficiente. Requiere marketing + acuerdos con aseguradoras.</li>
        </ul>'))
        ),
        
        div(style = "background:#1c2333; border:1px solid #252f45; border-radius:10px; padding:16px 20px; margin-top:4px;",
            HTML('<div style="font-size:11px;color:#64748b;line-height:1.8;">
              <strong style="color:#e2e8f0;">Nota para el comité:</strong>
              Este análisis integra el Modelo Huff calibrado con datos reales de captación (7 hospitales existentes),
              la brecha de servicios medida contra estándares nacionales e internacionales,
              y un modelo financiero con cinco escenarios de sensibilidad.
              Los datos de oferta actual provienen de oferta_actual.rds (camas/especialidades/pacientes reales por hospital).
              Las cifras de CAPEX y OPEX son estimaciones de prefactibilidad.
            </div>'))
    )
  )
)

ui <- secure_app(ui_dash,
                 background = "linear-gradient(135deg, #0d1117 0%, #1c2333 100%)",
                 tags_top = tags$div(
                   style = "text-align:center;",
                   tags$h3("Acceso Restringido", style = "color:#f5a623; font-family:'Rajdhani',sans-serif;"),
                   tags$img(src = "https://cdn-icons-png.flaticon.com/512/3063/3063176.png", width = 70)
                 ),
                 language = "es"
)

# ══════════════════════════════════════════════════════════════════════════════
# SERVER
# ══════════════════════════════════════════════════════════════════════════════
server <- function(input, output, session) {
  
  res_auth <- secure_server(check_credentials = check_credentials(credentials))
  observe({ req(res_auth$user) })
  
  mapa_listo <- reactiveVal(FALSE)
  # ─── Reactive: datos escenario ─────────────────────────────────────────────
  datos_escenario <- reactive({
    req(res_auth$user)
    req(!is.null(oferta_actual), !is.null(demanda))
    
    oferta_temp <- oferta_actual %>% mutate(k_factor = 1)
    res_teorico <- calcular_huff(demanda, oferta_temp, input$sensibilidad)
    
    total_reales  <- sum(oferta_actual$pacientes_reales, na.rm = TRUE)
    total_modelo  <- sum(res_teorico$resumen_hospitales$total_pacientes)
    tasa_uso      <- ifelse(total_modelo > 0, total_reales / total_modelo, 0)
    
    calibracion <- res_teorico$resumen_hospitales %>%
      left_join(oferta_actual %>% select(id, pacientes_reales), by = c("of_id" = "id")) %>%
      mutate(share_real   = pacientes_reales / total_reales,
             share_modelo = total_pacientes  / total_modelo,
             k_factor_calc = ifelse(share_modelo > 0, share_real / share_modelo, 1)) %>%
      select(of_id, k_factor_calc)
    
    oferta_cal <- oferta_actual %>%
      left_join(calibracion, by = c("id" = "of_id")) %>%
      mutate(k_factor = replace_na(k_factor_calc, 1))
    
    proy_din <- proyectos_futuros %>%
      filter(id %in% input$proyectos_activos) %>%
      mutate(k_factor = 1.0)
    
    if (nrow(proy_din) > 0 && "PROYECTO" %in% input$proyectos_activos) {
      proy_din$camas[proy_din$id == "PROYECTO"]          <- input$camas_proy
      proy_din$especialidades[proy_din$id == "PROYECTO"] <- input$esp_proy
      proy_din$k_factor <- mean(oferta_cal$k_factor, na.rm = TRUE)
    }
    
    res_base   <- calcular_huff(demanda, oferta_cal, input$sensibilidad)
    res_base$resumen_hospitales$total_pacientes <-
      res_base$resumen_hospitales$total_pacientes * tasa_uso
    
    oferta_total <- if (nrow(proy_din) > 0) bind_rows(oferta_cal, proy_din) else oferta_cal
    res_futuro   <- calcular_huff(demanda, oferta_total, input$sensibilidad)
    res_futuro$resumen_hospitales$total_pacientes <-
      res_futuro$resumen_hospitales$total_pacientes * tasa_uso
    
    layer_prob <- res_futuro$detallado %>%
      filter(of_id == "PROYECTO") %>%
      mutate(min_p = min(probabilidad, na.rm = TRUE),
             max_p = max(probabilidad, na.rm = TRUE),
             rango = max_p - min_p,
             prob_norm = ifelse(rango > 0, (probabilidad - min_p) / rango, 0)) %>%
      select(dem_id, lat_dem, lon_dem, probabilidad, prob_norm, mercado_captado)
    
    list(base = res_base, futuro = res_futuro, oferta_usada = oferta_total,
         layer_prob = layer_prob, tasa_uso_global = tasa_uso)
  })
  
  observe({
    req(res_auth$user)
    if (is.null(oferta_actual)) return()
    pacs_mes <- tryCatch(
      datos_escenario()$futuro$resumen_hospitales %>%
        filter(of_id == "PROYECTO" | tipo == "Nuestro Proyecto") %>%
        pull(total_pacientes) %>% sum(na.rm = TRUE),
      error = function(e) 0
    )
    if (pacs_mes > 0)
      updateNumericInput(session, "poblacion_objetivo", value = round(pacs_mes * 12))
  })
  
  # ── Reactive: vectores de mercado derivados de datos_escenario() ─────────
  mercado_reactivo <- reactive({
    req(res_auth$user)
    
    # Valores estáticos de fallback (cuando .rds no está disponible)
    fallback <- list(
      hospitales = c("Clínica del Pacífico", "Hospital General",
                     "IMSS Manzanillo", "Nuevo Proyecto"),
      prob       = c(11, 38, 30, 18),
      pacs       = c(5000, 14022, 11500, 8429),
      can_labels = c("Clínica del Pacífico", "Fuga a Colima/GDL",
                     "Hospital General", "IMSS Manzanillo", "Nuevo Proyecto"),
      can_pct    = c(12, 18, 32, 28, 18)
    )
    
    if (is.null(oferta_actual)) return(fallback)
    
    tryCatch({
      fut <- datos_escenario()$futuro$resumen_hospitales
      bas <- datos_escenario()$base$resumen_hospitales
      
      # Anualizar (la fuente .rds está en unidades mensuales)
      fut_a <- fut %>% mutate(pacs_anual = round(total_pacientes * 12))
      bas_a <- bas %>% mutate(pacs_anual = round(total_pacientes * 12))
      
      total <- sum(fut_a$pacs_anual)
      
      hosp_names <- stringr::str_to_title(tolower(fut_a$nombre))
      prob_v     <- round(fut_a$pacs_anual / total * 100, 0)
      pacs_v     <- fut_a$pacs_anual
      
      # Canibalización: hospitales que pierden pacientes al entrar el proyecto
      comp <- fut_a %>%
        select(of_id, nombre, pacs_futuro = pacs_anual) %>%
        left_join(bas_a %>% select(of_id, pacs_base = pacs_anual), by = "of_id") %>%
        mutate(pacs_base  = replace_na(pacs_base, 0),
               diferencia = pacs_futuro - pacs_base)
      
      nuevo_ganancia <- comp %>%
        filter(of_id == "PROYECTO" | grepl("proyecto", tolower(nombre))) %>%
        pull(diferencia) %>% sum(na.rm = TRUE)
      
      perdedores <- comp %>%
        filter(diferencia < 0) %>%
        mutate(nombre = stringr::str_to_title(tolower(nombre)))
      
      perdida_local <- sum(abs(perdedores$diferencia))
      fuga_abs      <- max(0, nuevo_ganancia - perdida_local)
      
      can_labels_v <- c(perdedores$nombre, "Fuga a Colima/GDL", "Nuevo Proyecto")
      
      pool_total <- sum(abs(perdedores$diferencia)) + fuga_abs
      
      can_pct_v <- c(
        round(abs(perdedores$diferencia) / pool_total * 100),
        round(fuga_abs                   / pool_total * 100),
        round(nuevo_ganancia             / total      * 100)   # este sigue referenciando total
      )
      
      list(hospitales = hosp_names, prob = prob_v, pacs = pacs_v,
           can_labels = can_labels_v, can_pct = can_pct_v)
      
    }, error = function(e) fallback)
  })
  
  # ─── Pestaña 1: Mapa ───────────────────────────────────────────────────────
  # ─── Pestaña 1: Mapa — construcción inicial (solo se ejecuta una vez) ──────
  output$mapa <- renderLeaflet({
    req(res_auth$user)
    m <- leaflet() %>%
      addTiles() %>%
      setView(-104.33, 19.1, zoom = 13)
    
    if (!is.null(denue_salud)) {
      icon_farm <- awesomeIcons(icon = "plus-square", library = "fa", markerColor = "green",  iconColor = "white")
      icon_cons <- awesomeIcons(icon = "user-md",     library = "fa", markerColor = "blue",   iconColor = "white")
      icon_lab  <- awesomeIcons(icon = "flask",       library = "fa", markerColor = "orange", iconColor = "white")
      m <- m %>%
        addAwesomeMarkers(data = denue_salud %>% filter(tipo_negocio == "Farmacias"),
                          icon = icon_farm, group = "Farmacias",
                          popup = ~paste0("<b>", nom_estab, "</b><br>", tipo_negocio),
                          clusterOptions = markerClusterOptions()) %>%
        addAwesomeMarkers(data = denue_salud %>% filter(tipo_negocio == "Consultorios"),
                          icon = icon_cons, group = "Consultorios",
                          popup = ~paste0("<b>", nom_estab, "</b><br>", tipo_negocio),
                          clusterOptions = markerClusterOptions()) %>%
        addAwesomeMarkers(data = denue_salud %>% filter(tipo_negocio == "Laboratorios"),
                          icon = icon_lab,  group = "Laboratorios",
                          popup = ~paste0("<b>", nom_estab, "</b><br>", tipo_negocio),
                          clusterOptions = markerClusterOptions())
    }
    
    m %>%
      addLayersControl(
        baseGroups    = c("Mapa"),
        overlayGroups = c("Ubicaciones", "Hospitales", "Probabilidad", "Farmacias", "Consultorios", "Laboratorios"),
        options = layersControlOptions(collapsed = FALSE)) %>%
      hideGroup("Consultorios") %>% 
      { mapa_listo(TRUE); . }
  })
  
  # ─── Pestaña 1: Mapa — actualización reactiva de capas de datos ────────────
  observe({
    req(res_auth$user)
    req(mapa_listo()) 
    if (is.null(oferta_actual)) return()
    
    res        <- datos_escenario()$futuro$resumen_hospitales
    oferta_mapa <- datos_escenario()$oferta_usada
    data_prob  <- datos_escenario()$layer_prob
    tasa_uso   <- datos_escenario()$tasa_uso_global
    
    map_data <- oferta_mapa %>%
      left_join(res %>% select(of_id, total_pacientes), by = c("id" = "of_id"))
    
    pal_prob <- colorNumeric("Reds", domain = c(0, 1))
    cols_seg <- c("Existente" = "#64748b", "Nuestro Proyecto" = "#38bdf8", "Competencia en Obra" = "#c084fc")
    pal_hosp <- colorFactor(palette = cols_seg, domain = map_data$tipo)
    col_marc <- case_when(
      as.character(map_data$tipo) == "Existente"           ~ "gray",
      as.character(map_data$tipo) == "Nuestro Proyecto"    ~ "blue",
      as.character(map_data$tipo) == "Competencia en Obra" ~ "purple",
      TRUE ~ "red"
    )
    icons_hosp <- awesomeIcons(icon = "h-square", library = "fa",
                               markerColor = col_marc, iconColor = "white")
    
    leafletProxy("mapa") %>%
      clearGroup("Probabilidad") %>%
      clearGroup("Hospitales") %>%
      clearGroup("Ubicaciones") %>%
      addCircleMarkers(data = data_prob, lng = ~lon_dem, lat = ~lat_dem,
                       group = "Probabilidad",
                       radius = 8, fillColor = ~pal_prob(prob_norm), color = "transparent",
                       fillOpacity = 0.75,
                       popup = ~paste0("<b>Prob. de Captura:</b> ", round(prob_norm * 100, 1), "%<br>",
                                       "Pacientes Est.: ", round(mercado_captado * tasa_uso, 1))) %>%
      addCircleMarkers(data = map_data, lng = ~lon, lat = ~lat,
                       group = "Hospitales",
                       radius = ~sqrt(total_pacientes) / 4, color = ~pal_hosp(tipo),
                       stroke = TRUE, weight = 2, opacity = 1, fillOpacity = 0.35,
                       popup = ~paste0("<b>", nombre, "</b><br>Pacientes Est: ",
                                       prettyNum(round(total_pacientes, 0), big.mark = ","))) %>%
      addAwesomeMarkers(data = map_data, lng = ~lon, lat = ~lat,
                        icon = icons_hosp, group = "Ubicaciones",
                        popup = ~paste0("<b>", nombre, "</b><br>Tipo: ", tipo)) %>%
      addLegend("bottomright", pal = pal_prob, values = c(0, 1),
                layerId = "leyenda_prob",
                title = "Prob. de Captura",
                labFormat = labelFormat(suffix = "%", transform = function(x) x * 100))
  })
  
  # ─── Pestaña 1: Sankey ─────────────────────────────────────────────────────
  output$sankey_canibal <- renderSankeyNetwork({
    req(res_auth$user)
    if (is.null(oferta_actual)) return(NULL)
    
    base   <- datos_escenario()$base$resumen_hospitales   %>% mutate(total_pacientes = total_pacientes * 12) %>%  rename(pacientes_base   = total_pacientes)
    futuro <- datos_escenario()$futuro$resumen_hospitales %>% mutate(total_pacientes = total_pacientes * 12) %>% rename(pacientes_futuro = total_pacientes)
    
    flujo <- bind_rows(base, futuro) %>%
      mutate(nombre = stringr::str_to_title(tolower(trimws(nombre)))) %>%
      group_by(of_id, nombre, tipo) %>%
      summarise(p_base = sum(pacientes_base, na.rm = TRUE),
                p_futuro = sum(pacientes_futuro, na.rm = TRUE), .groups = "drop") %>%
      mutate(diferencia = p_futuro - p_base,
             status = ifelse(diferencia > 0, "Ganador", "Perdedor"))
    
    perdedores <- flujo %>% filter(status == "Perdedor")
    ganadores  <- flujo %>% filter(status == "Ganador")
    if (nrow(ganadores) == 0) return(NULL)
    
    total_mov  <- sum(ganadores$diferencia)
    perdedores <- perdedores %>%
      mutate(pct = abs(diferencia) / total_mov,
             nombre_label = paste0(nombre, "|", round(pct * 100, 1), "% "))
    ganadores  <- ganadores %>%
      mutate(pct = diferencia / total_mov,
             nombre_label = paste0(nombre, "|", round(pct * 100, 1), "% "))
    
    links <- data.frame()
    for (i in 1:nrow(perdedores)) {
      for (j in 1:nrow(ganadores)) {
        links <- rbind(links, data.frame(
          source = perdedores$nombre_label[i],
          target = ganadores$nombre_label[j],
          value  = abs(perdedores$diferencia[i]) * (ganadores$diferencia[j] / total_mov),
          group  = ganadores$tipo[j]))
      }
    }
    
    nodes <- data.frame(name = unique(c(as.character(links$source), as.character(links$target))))
    nodes$nombre_puro <- gsub("\\|.*", "", nodes$name)
    nodes <- nodes %>%
      left_join(flujo %>% select(nombre, tipo) %>% distinct(), by = c("nombre_puro" = "nombre")) %>%
      mutate(color_group = case_when(
        tipo == "Nuestro Proyecto"    ~ "PROYECTO",
        tipo == "Competencia en Obra" ~ "COMPETENCIA",
        TRUE ~ "EXISTENTE"))
    nodes$group <- nodes$color_group
    links <- links %>% mutate(group = case_when(
      group == "Nuestro Proyecto"    ~ "PROYECTO",
      group == "Competencia en Obra" ~ "COMPETENCIA",
      TRUE ~ "EXISTENTE"))
    links$IDsource <- match(links$source, nodes$name) - 1
    links$IDtarget <- match(links$target, nodes$name) - 1
    
    js_col <- sprintf('d3.scaleOrdinal().domain(%s).range(%s)',
                      jsonlite::toJSON(c("PROYECTO","COMPETENCIA","EXISTENTE"), auto_unbox = TRUE),
                      jsonlite::toJSON(c("#38bdf8","#c084fc","#64748b"),        auto_unbox = TRUE))
    
    sn <- sankeyNetwork(Links = links, Nodes = nodes,
                        Source = "IDsource", Target = "IDtarget",
                        Value = "value", NodeID = "name", units = "Pacientes",
                        fontSize = 12, nodeWidth = 36, nodePadding = 18, sinksRight = FALSE,
                        colourScale = js_col, LinkGroup = "group", NodeGroup = "group",
                        fontFamily = "Inter",
                        margin = list(left = 200, right = 200, top = 20, bottom = 20))
    
    htmlwidgets::onRender(sn, '
      function(el, x) {
        d3.select(el).selectAll(".node text")
          .attr("text-anchor", function(d) { return d.x < 100 ? "end" : "start"; })
          .html(function(d) {
            var parts = d.name.split("|");
            if (parts.length === 1) return parts[0];
            var isLeft = d.x < 100;
            var xPos = isLeft ? -12 : 48;
            return "<tspan x=\\"" + xPos + "\\" dy=\\"-0.2em\\">" + parts[0] + "</tspan>" +
                   "<tspan x=\\"" + xPos + "\\" dy=\\"1.3em\\" style=\\"font-size:10px;fill:#64748b;font-weight:400;\\">" + parts[1] + "</tspan>";
          });
      }')
  })
  
  # ─── Pestaña 1: Gráfica comparativa Echauri ────────────────────────────────
  output$echauri_comp <- renderPlotly({
    req(res_auth$user)
    
    variables_cap <- c("Camas", "Consultorios", "Quirófanos")
    vals_proy_cap <- c(80, 20, 5)
    vals_ech_cap  <- c(13, 6, 1)
    unidades_cap  <- c("camas", "consultorios", "quirófanos")
    
    plot_ly() %>%
      # ── Eje principal: capacidad física ──────────────────────────────────
      add_bars(x = variables_cap, y = vals_proy_cap,
               name = "Nuevo Proyecto",
               marker = list(color = "#38bdf8", cornerradius = 3),
               text = paste0(vals_proy_cap, " ", unidades_cap),
               hovertemplate = "<b>Nuevo Proyecto</b><br>%{text}<extra></extra>") %>%
      add_bars(x = variables_cap, y = vals_ech_cap,
               name = "Hospital Echauri",
               marker = list(color = "#c084fc", cornerradius = 3),
               text = paste0(vals_ech_cap, " ", unidades_cap),
               hovertemplate = "<b>Hospital Echauri</b><br>%{text}<extra></extra>") %>%
      # ── Eje secundario: pacientes/año ─────────────────────────────────────
      add_bars(x = c("Pacientes/año"), y = c(8429),
               name = "Proy. — Pacientes",
               yaxis = "y2",
               marker = list(color = "#38bdf8", opacity = 0.6, cornerradius = 3),
               text = "8,429 pacientes/año",
               hovertemplate = "<b>Nuevo Proyecto</b><br>%{text}<extra></extra>") %>%
      add_bars(x = c("Pacientes/año"), y = c(253),
               name = "Echauri — Pacientes",
               yaxis = "y2",
               marker = list(color = "#c084fc", opacity = 0.6, cornerradius = 3),
               text = "253 pacientes/año",
               hovertemplate = "<b>Hospital Echauri</b><br>%{text}<extra></extra>") %>%
      dark_plotly() %>%
      layout(
        barmode = "group",
        yaxis  = list(title = "Capacidad física",
                      gridcolor = "#252f45", color = "#64748b"),
        yaxis2 = list(title = "Pacientes / año",
                      overlaying = "y", side = "right",
                      gridcolor = "rgba(0,0,0,0)", color = "#64748b",
                      showgrid = FALSE),
        legend = list(x = 0, y = 1.2, orientation = "h",
                      font = list(color = "#94a3b8", size = 9))
      )
  })
  
  # ─── Pestaña 1: Tablas ─────────────────────────────────────────────────────
  output$tabla_resumen <- renderTable({
    req(res_auth$user)
    if (is.null(oferta_actual)) return(data.frame(Nota = "Datos no disponibles"))
    datos_escenario()$futuro$resumen_hospitales %>%
      mutate(nombre = stringr::str_to_title(tolower(nombre)),
             Anuales = scales::comma(total_pacientes * 12, accuracy = 1)) %>%
      arrange(desc(total_pacientes * 12)) %>%
      mutate(es = (of_id == "PROYECTO" | tipo == "Nuestro Proyecto"),
             Nombre = ifelse(es, paste0("<b style='color:#38bdf8;'>", nombre, "</b>"), nombre),
             Tipo   = ifelse(es, paste0("<b>", tipo, "</b>"), tipo),
             Anuales = ifelse(es, paste0("<b>", Anuales, "</b>"), Anuales)) %>%
      select(Nombre, Tipo, `Pacientes Anuales` = Anuales)
  }, sanitize.text.function = function(x) x)
  
  output$plot_desglose <- renderPlotly({
    req(res_auth$user)
    if (is.null(oferta_actual)) return(NULL)
    
    pacs_mes   <- datos_escenario()$futuro$resumen_hospitales %>%
      filter(tipo == "Nuestro Proyecto" | of_id == "PROYECTO") %>%
      pull(total_pacientes) %>% sum(na.rm = TRUE)
    pacs_anual <- pacs_mes * 12
    
    servicios <- c("Urgencias", "Consulta Externa", "Hospitalización", "Cirugías")
    volumenes  <- round(c(pacs_anual * .35,
                          pacs_anual * 2.5,
                          pacs_anual * .10,
                          pacs_anual * .10 * .40))
    colores    <- c("#f87171", "#38bdf8", "#f5a623", "#4ade80")
    
    plot_ly(
      x = volumenes, y = servicios,
      type = "bar", orientation = "h",
      marker = list(color = colores, cornerradius = 3),
      text  = scales::comma(volumenes),
      textposition = "outside",
      hovertemplate = "<b>%{y}</b><br>%{text} visitas/año<extra></extra>"
    ) |>
      dark_plotly() |>
      layout(
        xaxis = list(title = "Volumen anual estimado",
                     tickformat = ",d"),
        yaxis = list(autorange = "reversed"),
        margin = list(l = 130, r = 60)
      )
  })
  
  # ─── Pestaña 2: Calculadora de Capacidad ───────────────────────────────────
  egresos_anuales <- reactive({
    req(res_auth$user)
    round(input$poblacion_objetivo * input$tasa_hosp / 1000)
  })
  
  n_camas <- reactive({
    req(res_auth$user, input$estancia_prom, input$ocupacion_obj)
    raw <- ceiling((egresos_anuales() * input$estancia_prom) /
                     (365 * (input$ocupacion_obj / 100)))
    round(raw * FACTOR_CAMAS)
  })
  
  n_consultorios <- reactive({
    req(res_auth$user, input$consultas_paciente, input$pacientes_hora,
        input$turnos_cons, input$dias_lab_cons)
    consultas_año  <- input$poblacion_objetivo * input$consultas_paciente
    horas_diarias  <- as.numeric(input$turnos_cons) * 8
    ceiling(consultas_año * FACTOR_CONS /
              (input$pacientes_hora * horas_diarias * input$dias_lab_cons))
  })
  
  output$res_camas <- renderText({
    req(res_auth$user); paste(n_camas(), "camas")
  })
  output$res_consultorios <- renderText({
    req(res_auth$user); paste(n_consultorios(), "consultorios")
  })
  output$res_quirofanos <- renderText({
    req(res_auth$user)
    raw <- ceiling((egresos_anuales() * input$tasa_qx / 100) /
                     (input$cirugias_sala_dia * 365))
    paste(ceiling(raw * FACTOR_QX), "salas")
  })
  output$res_medicos <- renderText({
    req(res_auth$user); paste(round(n_camas() * input$ratio_medicos_cama), "médicos")
  })
  
  output$tabla_especialidades <- renderTable({
    req(res_auth$user, n_camas())
    total <- round(n_camas() * input$ratio_medicos_cama)
    personal <- round(total * c(.15,.18,.14,.12,.13,.10,.11,.07))
    tibble(
      Especialidad    = c("Medicina Interna","Gineco-Obstetricia","Cirugía General",
                          "Pediatría","Traumatología/Ortopedia","Anestesiología",
                          "Urgencias y Hosp. Día","Apoyo Diagnóstico","— TOTAL —"),
      `% Plantilla`   = c("15%","18%","14%","12%","13%","10%","11%","7%", "100%"),
      `Personal Est.` = c(personal, sum(personal))
    )
  })
  
  output$tabla_talento_externo <- DT::renderDT({
    req(res_auth$user)
    df <- reclu_df
    df$Urgencia <- paste0('<span style="color:',
                          ifelse(df$Urgencia == "Alta",  "#f87171",
                                 ifelse(df$Urgencia == "Media", "#fb923c", "#4ade80")),
                          '; font-weight:700;">● ', df$Urgencia, '</span>')
    df$Fase <- paste0('<span style="color:',
                      ifelse(df$Fase == "Fase 1", "#38bdf8",
                             ifelse(df$Fase == "Fase 2", "#f5a623", "#c084fc")),
                      '; font-weight:600;">', df$Fase, '</span>')
    df$FTE <- paste0('<span style="background:#1c2333;border:1px solid #252f45;border-radius:4px;',
                     'padding:2px 8px;font-weight:700;color:#e2e8f0;">', df$FTE, '</span>')
    DT::datatable(df, escape = FALSE, rownames = FALSE,
                  colnames = c("Especialidad","Médicos","Origen Regional","Modalidad","Urgencia","Fase"),
                  options  = list(dom = "t", ordering = FALSE, pageLength = 15,
                                  scrollX = TRUE,
                                  columnDefs = list(
                                    list(className = "dt-center", targets = c(1, 4, 5)),
                                    list(width = "30%", targets = 3)
                                  )),
                  class = "display")
  })
  
  # ─── Pestaña 2: Tabla médicos por ciudad ───────────────────────────────────
  output$tabla_medicos_ciudades <- DT::renderDT({
    req(res_auth$user)
    df <- medicos_ciudades_df
    
    df$Prioridad <- paste0('<span style="color:',
                           ifelse(df$Prioridad == "Alta",  "#f87171",
                                  ifelse(df$Prioridad == "Media", "#fb923c", "#4ade80")),
                           '; font-weight:700;">● ', df$Prioridad, '</span>')
    
    df$Deficit_proyecto <- paste0(
      '<span style="background:',
      ifelse(df$Deficit_proyecto > 0, "rgba(248,113,113,.15)", "rgba(74,222,128,.1)"),
      '; border:1px solid ',
      ifelse(df$Deficit_proyecto > 0, "rgba(248,113,113,.4)", "rgba(74,222,128,.3)"),
      '; border-radius:4px; padding:1px 8px; font-weight:700; color:',
      ifelse(df$Deficit_proyecto > 0, "#f87171", "#4ade80"), ';">',
      ifelse(df$Deficit_proyecto > 0, paste0("-", df$Deficit_proyecto), "✓"), '</span>')
    
    DT::datatable(df, escape = FALSE, rownames = FALSE,
                  colnames = c("Especialidad", "Manzanillo (actual)",
                               "Colima", "Cd. Guzmán", "Guadalajara", "Villa de Álvarez", "Tecomán",
                               "Déficit Proyecto", "Prioridad"),
                  options = list(
                    dom = "ft", ordering = TRUE, pageLength = 15,
                    scrollX = TRUE,
                    columnDefs = list(
                      list(className = "dt-center", targets = 1:7),
                      list(width = "18%", targets = 0)
                    )
                  ),
                  class = "display")
  })
  
  # ─── Pestaña 2: Gráfica déficit médicos ────────────────────────────────────
  output$plot_deficit_medicos <- renderPlotly({
    req(res_auth$user)
    df_def <- medicos_ciudades_df %>%
      filter(Deficit_proyecto > 0) %>%
      arrange(desc(Deficit_proyecto))
    
    col_urg <- ifelse(df_def$Prioridad == "Alta", "#f87171",
                      ifelse(df_def$Prioridad == "Media", "#fb923c", "#4ade80"))
    
    plot_ly() %>%
      add_bars(x = df_def$Colima_cap, y = df_def$Especialidad,
               name = "Colima", orientation = "h",
               marker = list(color = "#38bdf8", cornerradius = 3),
               hovertemplate = "<b>%{y}</b><br>Colima: %{x} médicos<extra></extra>") %>%
      add_bars(x = df_def$Ciudad_Guzman, y = df_def$Especialidad,
               name = "Cd. Guzmán", orientation = "h",
               marker = list(color = "#f5a623", cornerradius = 3),
               hovertemplate = "<b>%{y}</b><br>Cd. Guzmán: %{x} médicos<extra></extra>") %>%
      add_bars(x = df_def$Villa_Alvarez, y = df_def$Especialidad,
               name = "Villa de Álvarez", orientation = "h",
               marker = list(color = "#4ade80", cornerradius = 3),
               hovertemplate = "<b>%{y}</b><br>Villa de Álvarez: %{x} médicos<extra></extra>") %>%
      add_markers(x = df_def$Deficit_proyecto, y = df_def$Especialidad,
                  name = "Médicos faltantes",
                  marker = list(color = col_urg, size = 10, symbol = "diamond",
                                line = list(color = "#0d1117", width = 1)),
                  hovertemplate = "<b>%{y}</b><br>Déficit: %{x} Médicos<extra></extra>") %>%
      dark_plotly() %>%
      layout(
        barmode = "group",
        xaxis   = list(title = "Número de médicos disponibles"),
        yaxis   = list(autorange = "reversed"),
        legend  = list(x = 0.45, y = 0.05, font = list(color = "#94a3b8", size = 9))
      )
  })
  
  # ─── Pestaña 3: Análisis de Mercado ────────────────────────────────────────
  output$am_huff <- renderPlotly({
    m    <- mercado_reactivo()
    paleta <- c("#f5a623","#38bdf8","#4ade80","#f87171","#c084fc","#fb923c",
                "#e2e8f0","#67e8f9","#fde68a","#a78bfa","#34d399","#f472b6")
    cols <- paleta[((seq_along(m$hospitales) - 1) %% length(paleta)) + 1]
    plot_ly(x = m$prob, y = m$hospitales, type = "bar", orientation = "h",
            marker = list(color = cols, cornerradius = 3),
            hovertemplate = "%{x}%<extra></extra>") |>
      dark_plotly() |>
      layout(xaxis = list(ticksuffix = "%", range = c(0, 45)),
             yaxis = list(autorange = "reversed"))
  })
  
  output$am_pacs <- renderPlotly({
    m    <- mercado_reactivo()
    paleta <- c("#f5a623","#38bdf8","#4ade80","#f87171","#c084fc","#fb923c",
                "#e2e8f0","#67e8f9","#fde68a","#a78bfa","#34d399","#f472b6")
    cols <- paleta[((seq_along(m$hospitales) - 1) %% length(paleta)) + 1]
    plot_ly(x = m$hospitales, y = m$pacs, type = "bar",
            marker = list(color = cols, cornerradius = 3),
            hovertemplate = "%{y:,}<extra></extra>") |>
      dark_plotly() |>
      layout(yaxis = list(tickformat = ",d"))
  })
  
  output$am_can <- renderPlotly({
    m    <- mercado_reactivo()
    paleta <- c("#4ade80","#f87171","#f5a623","#38bdf8","#c084fc","#fb923c",
                "#e2e8f0","#67e8f9","#fde68a","#a78bfa","#34d399","#f472b6")
    cols <- paleta[((seq_along(m$can_labels) - 1) %% length(paleta)) + 1]
    plot_ly(x = m$can_pct, y = m$can_labels, type = "bar", orientation = "h",
            marker = list(color = cols, cornerradius = 3),
            hovertemplate = "%{x}%<extra></extra>") |>
      dark_plotly() |>
      layout(xaxis = list(ticksuffix = "%"), 
             yaxis = list(autorange = "reversed"))
  })
  
  output$am_tbl_comp <- DT::renderDT({
    df <- competidores_df
    
    # Formato columna Tipo
    df$Tipo <- paste0('<span style="color:',
                      ifelse(df$Tipo == "Público", "#38bdf8",
                             ifelse(df$Tipo == "Privado", "#f5a623",
                                    ifelse(df$Tipo == "Seg. Social", "#94a3b8", "#94a3b8"))),
                      '; font-weight:600;">', df$Tipo, '</span>')
    
    # Formato ocupación con colores
    ocup_color <- sapply(df$Ocup, function(x) {
      if (x %in% c("92%", "95%")) return("#f87171")
      if (x %in% c("N/A", "N/D")) return("#64748b")
      n <- suppressWarnings(as.numeric(gsub("%", "", x)))
      if (is.na(n)) return("#64748b")
      if (n >= 70) "#fb923c" else "#4ade80"
    })
    df$Ocup <- paste0('<span style="color:', ocup_color,
                      '; font-weight:700; font-size:13px;">', df$Ocup, '</span>')
    
    # Resaltar Echauri como benchmark
    df$Hospital <- ifelse(df$Hospital == "Hospital Echauri",
                          paste0('<span style="color:#c084fc;font-weight:700;">★ ', df$Hospital, '</span>'),
                          df$Hospital)
    
    # C09: Pacs_reales × 12 para mostrar anual (la fuente .rds está en unidades mensuales)
    df$Pacs_reales <- scales::comma(df$Pacs_reales * 12, accuracy = 1)
    
    # Formato Tasa_qx
    df$Tasa_qx <- paste0('<span style="color:#94a3b8;">', df$Tasa_qx, '</span>')
    
    # Formato Cirugias_dia
    df$Cirugias_dia <- ifelse(
      is.na(df$Cirugias_dia),
      '<span style="color:#64748b;">N/D</span>',
      paste0('<span style="color:#e2e8f0;font-weight:600;">', df$Cirugias_dia, '</span>')
    )
    
    DT::datatable(df, escape = FALSE, rownames = FALSE,
                  colnames = c("Hospital", "Tipo", "Camas", "Especialidades",
                               "Pac. Reales/año", "Ocupación", "Tasa Qx", "Cirugías/día"),
                  options = list(dom = "t", ordering = FALSE, pageLength = 10,
                                 scrollX = TRUE,
                                 columnDefs = list(
                                   list(className = "dt-center", targets = 2:7)
                                 )),
                  class = "display")
  })
  
  # ─── Pestaña 4: Brecha de Servicios ────────────────────────────────────────
  output$bs_radar <- renderPlotly({
    theta <- c(especialidades_v, especialidades_v[1])
    plot_ly(type = "scatterpolar", fill = "toself") |>
      add_trace(r = c(estandar_c1000, estandar_c1000[1]), theta = theta,
                name = "Estándar Nacional",
                line = list(color = "#f5a623", width = 2),
                fillcolor = "rgba(245,166,35,.12)") |>
      add_trace(r = c(actual_c1000, actual_c1000[1]), theta = theta,
                name = "Capacidad Actual",
                line = list(color = "#38bdf8", width = 2),
                fillcolor = "rgba(56,189,248,.12)") |>
      layout(paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
             polar = list(bgcolor = "rgba(0,0,0,0)",
                          radialaxis  = list(color = "#64748b", gridcolor = "#252f45",
                                             tickfont = list(size = 8)),
                          angularaxis = list(color = "#64748b", gridcolor = "#252f45",
                                             tickfont = list(size = 9))),
             showlegend = TRUE,
             legend = list(x = 0, y = -0.12, orientation = "h",
                           font = list(color = "#64748b", size = 9)),
             margin = list(t = 10, b = 40)) |>
      config(displayModeBar = FALSE)
  })
  
  output$bs_fuga <- renderPlotly({
    ord      <- order(fuga_pct)
    esp_ord  <- especialidades_v[ord]
    fuga_ord <- fuga_pct[ord]
    cols     <- ifelse(fuga_ord >= 40, "#f87171", ifelse(fuga_ord >= 20, "#fb923c", "#f5a623"))
    
    # Pacientes anuales estimados que salen de Manzanillo por especialidad
    # Demanda potencial = estandar_c1000 × poblacion / 1000; fuga = esa demanda × fuga_pct/100
    #pac_abs_v <- round(fuga_pct / 100 * estandar_c1000 * poblacion_base_estudio / 1000)
    #pac_ord   <- pac_abs_v[ord]
    pac_ord  <- fuga_absoluta[ord] 
    
    plot_ly(x = fuga_ord, y = esp_ord, type = "bar", orientation = "h",
            text      = paste0("~", scales::comma(pac_ord), " pac./año"),
            marker    = list(color = cols, cornerradius = 3),
            hovertemplate = "<b>%{y}</b><br>Fuga: <b>%{x}%</b><br>Estimado: %{text}<extra></extra>") |>
      dark_plotly() |>
      layout(xaxis = list(ticksuffix = "%"))
  })
  
  output$bs_tbl <- DT::renderDT({
    df <- data.frame(
      Especialidad    = especialidades_v,
      `Actual c/1000` = actual_c1000,
      Estándar        = estandar_c1000,
      Brecha          = round(brecha_v, 1),
      `Fuga %`        = paste0(fuga_pct, "%"),
      Estado          = estado_esp,
      check.names = FALSE, stringsAsFactors = FALSE
    )
    df$Brecha  <- paste0('<span style="color:', ifelse(df$Brecha < 0, "#f87171", "#4ade80"),
                         '; font-weight:600;">', df$Brecha, '</span>')
    df$Estado  <- paste0('<span style="color:',
                         ifelse(df$Estado == "Crítico", "#f87171",
                                ifelse(df$Estado == "Moderado", "#fb923c", "#4ade80")),
                         '; font-weight:700; font-size:13px;">● ', df$Estado, '</span>')
    DT::datatable(df, escape = FALSE, rownames = FALSE,
                  options = list(dom = "t", ordering = TRUE, pageLength = 15,
                                 columnDefs = list(
                                   list(className = "dt-center", targets = 1:5)  # Centra columnas 1-5
                                 )
                                 ),
                  class = "display")
  })
  
  # ─── Pestaña 5: Modelo Hospitalario ────────────────────────────────────────
  modelo_sel <- reactiveVal("Escalonado Fase 1-2-3")
  observeEvent(input$modelo_tab_sel, { modelo_sel(input$modelo_tab_sel) })
  
  output$mh_tabs_ui <- renderUI({
    mods <- modelos_hosp_df$Modelo
    sel  <- modelo_sel()
    tabs <- lapply(mods, function(m) {
      cls <- if (m == sel) "model-tab active" else "model-tab"
      tags$div(class = cls,
               onclick = sprintf("Shiny.setInputValue('modelo_tab_sel','%s',{priority:'event'})", m), m)
    })
    div(class = "model-tabs", tabs)
  })
  
  output$mh_content <- renderUI({
    m   <- modelo_sel()
    idx <- match(m, modelos_hosp_df$Modelo)
    row <- modelos_hosp_df[idx, ]
    es_recomendado <- (m == "Escalonado Fase 1-2-3")
    
    tagList(
      div(style = "margin-bottom:12px;",
          tags$span(style = "background:rgba(74,222,128,.1); border:1px solid rgba(74,222,128,.3); border-radius:4px; padding:3px 10px; font-size:10px; font-weight:600; color:#4ade80;",
                    paste("Riesgo:", row$Riesgo)),
          if (es_recomendado)
            tags$span(style = "margin-left:8px; background:rgba(245,166,35,.12); border:1px solid rgba(245,166,35,.3); border-radius:4px; padding:3px 10px; font-size:10px; font-weight:600; color:#f5a623;",
                      "⬤ Modelo Recomendado")
      ),
      div(class = "metric-row",
          div(class = "metric-box",
              div(class = "val", if (es_recomendado) hosp_camas_censables else row$Camas),
              div(class = "lbl", "Camas Censables")),
          div(class = "metric-box",
              div(class = "val", style = "color:#f5a623;", if (es_recomendado) hosp_consultorios else "14"),
              div(class = "lbl", "Consultorios")),
          div(class = "metric-box",
              div(class = "val", if (es_recomendado) hosp_quirofanos else "2"),
              div(class = "lbl", if (es_recomendado) "Qx + 1 Hemodinámica" else "Qx")),
          div(class = "metric-box",
              div(class = "val", if (es_recomendado) hosp_uci else "2"),
              div(class = "lbl", "UCI Adultos")),
          div(class = "metric-box",
              div(class = "val", if (es_recomendado) hosp_camas_urgencias else "6"),
              div(class = "lbl", "Camas Urgencias"))
      ),
      div(class = "metric-row",
          div(class = "metric-box",
              div(class = "val", row$CAPEX),
              div(class = "lbl", "CAPEX")),
          div(class = "metric-box",
              div(class = "val", style = "color:#fb923c;", paste0(row$ROI_5a, "%")),
              div(class = "lbl", "TIR 5 años")),
          div(class = "metric-box",
              div(class = "val", style = "color:#4ade80;", paste0(row$ROI_10a, "%")),
              div(class = "lbl", "ROI 10 años")),
          div(class = "metric-box",
              div(class = "val", style = "color:#4ade80;", paste0(row$Brecha_cv, "%")),
              div(class = "lbl", "Brecha cubierta"))
      ),
      
      if (es_recomendado) div(class = "chart-row",
                              div(class = "chart-panel",
                                  div(class = "chart-title", "Distribución de Camas por Servicio (Diseño Confirmado)"),
                                  plotlyOutput("mh_camas_dist", height = "200px")),
                              div(class = "chart-panel",
                                  div(class = "chart-title",
                                      HTML('<span style="color:#f5a623;">📋</span> Servicios de Apoyo Diagnóstico')),
                                  HTML('<div style="margin-top:6px;">
              <div style="display:flex;flex-direction:column;gap:8px;">
                <div style="display:flex;justify-content:space-between;border-bottom:1px solid #252f45;padding:6px 0;font-size:11px;color:#94a3b8;">
                  <span>🔬 Tomógrafo</span><span style="color:#38bdf8;font-weight:600;">1 unidad</span></div>
                <div style="display:flex;justify-content:space-between;border-bottom:1px solid #252f45;padding:6px 0;font-size:11px;color:#94a3b8;">
                  <span>🎗 Mamógrafo</span><span style="color:#38bdf8;font-weight:600;">1 unidad</span></div>
                <div style="display:flex;justify-content:space-between;border-bottom:1px solid #252f45;padding:6px 0;font-size:11px;color:#94a3b8;">
                  <span>📡 Ultrasonido</span><span style="color:#38bdf8;font-weight:600;">2 unidades</span></div>
                <div style="display:flex;justify-content:space-between;border-bottom:1px solid #252f45;padding:6px 0;font-size:11px;color:#94a3b8;">
                  <span>☢ Sala Rayos X</span><span style="color:#38bdf8;font-weight:600;">1 sala</span></div>
                <div style="display:flex;justify-content:space-between;border-bottom:1px solid #252f45;padding:6px 0;font-size:11px;color:#94a3b8;">
                  <span>🧪 Laboratorio Clínico</span><span style="color:#38bdf8;font-weight:600;">Propio</span></div>
                <div style="display:flex;justify-content:space-between;border-bottom:1px solid #252f45;padding:6px 0;font-size:11px;color:#94a3b8;">
                  <span>💊 Hospital de Día</span><span style="color:#38bdf8;font-weight:600;">6 sillones</span></div>
                <div style="display:flex;justify-content:space-between;padding:6px 0;font-size:11px;color:#94a3b8;">
                  <span>🚨 Sala de Hemodinamia</span><span style="color:#f5a623;font-weight:600;">1 sala</span></div>
              </div></div>')
                              )
      ),
      
      if (es_recomendado) div(class = "chart-row",
                              div(class = "chart-panel",
                                  div(class = "chart-title", "Bloque Quirúrgico — Distribución de Salas"),
                                  HTML('<div style="margin-top:6px;display:flex;gap:10px;flex-wrap:wrap;">
              <div style="flex:1;min-width:110px;background:#1c2333;border:1px solid #252f45;border-radius:8px;padding:10px;text-align:center;">
                <div style="font-family:Rajdhani,sans-serif;font-size:22px;font-weight:700;color:#e2e8f0;">1</div>
                <div style="font-size:9px;color:#64748b;text-transform:uppercase;">Séptico</div></div>
              <div style="flex:1;min-width:110px;background:#1c2333;border:1px solid #252f45;border-radius:8px;padding:10px;text-align:center;">
                <div style="font-family:Rajdhani,sans-serif;font-size:22px;font-weight:700;color:#e2e8f0;">3</div>
                <div style="font-size:9px;color:#64748b;text-transform:uppercase;line-height:1.4;">Trabajo Parto / Parto / Recuperación</div></div>
              <div style="flex:1;min-width:110px;background:#1c2333;border:1px solid #252f45;border-radius:8px;padding:10px;text-align:center;">
                <div style="font-family:Rajdhani,sans-serif;font-size:22px;font-weight:700;color:#e2e8f0;">1</div>
                <div style="font-size:9px;color:#64748b;text-transform:uppercase;">Sala de Cesáreas</div></div>
              <div style="flex:1;min-width:110px;background:rgba(245,166,35,.08);border:1px solid rgba(245,166,35,.3);border-radius:8px;padding:10px;text-align:center;">
                <div style="font-family:Rajdhani,sans-serif;font-size:22px;font-weight:700;color:#f5a623;">+1</div>
                <div style="font-size:9px;color:#64748b;text-transform:uppercase;">Sala Hemodinamia</div></div>
            </div>')
                              ),
                              div(class = "chart-panel",
                                  div(class = "chart-title", "Volúmenes Operativos Proyectados (Año 1 — Fuente: Huff)"),
                                  HTML(sprintf('<div style="margin-top:6px;display:flex;gap:10px;flex-wrap:wrap;">
              <div style="flex:1;min-width:110px;background:#1c2333;border:1px solid #252f45;border-radius:8px;padding:10px;text-align:center;">
                <div style="font-family:Rajdhani,sans-serif;font-size:22px;font-weight:700;color:#e2e8f0;">%s</div>
                <div style="font-size:9px;color:#64748b;text-transform:uppercase;">Urgencias / año</div></div>
              <div style="flex:1;min-width:110px;background:#1c2333;border:1px solid #252f45;border-radius:8px;padding:10px;text-align:center;">
                <div style="font-family:Rajdhani,sans-serif;font-size:22px;font-weight:700;color:#f5a623;">%s</div>
                <div style="font-size:9px;color:#64748b;text-transform:uppercase;">Consulta Externa</div></div>
              <div style="flex:1;min-width:110px;background:#1c2333;border:1px solid #252f45;border-radius:8px;padding:10px;text-align:center;">
                <div style="font-family:Rajdhani,sans-serif;font-size:22px;font-weight:700;color:#38bdf8;">%s</div>
                <div style="font-size:9px;color:#64748b;text-transform:uppercase;">Egresos</div></div>
              <div style="flex:1;min-width:110px;background:#1c2333;border:1px solid #252f45;border-radius:8px;padding:10px;text-align:center;">
                <div style="font-family:Rajdhani,sans-serif;font-size:22px;font-weight:700;color:#4ade80;">%s</div>
                <div style="font-size:9px;color:#64748b;text-transform:uppercase;">Cirugías</div></div>
            </div>',
                                               scales::comma(proy_urgencias),
                                               scales::comma(proy_consulta_ext),
                                               scales::comma(proy_hospitalizacion),
                                               scales::comma(proy_cirugias)))
                              )
      ),
      
      div(class = "chart-row",
          div(class = "chart-panel",
              div(class = "chart-title", "ROI 10 años por Modelo (%)"),
              plotlyOutput("mh_roi",    height = "190px")),
          div(class = "chart-panel",
              div(class = "chart-title", "Brecha Cubierta vs CAPEX"),
              plotlyOutput("mh_brecha", height = "190px"))
      ),
      div(class = "chart-panel full",
          div(class = "chart-title", "Scorecard Ejecutivo Comparativo"),
          DT::DTOutput("mh_scorecard")
      )
    )
  })
  
  output$mh_roi <- renderPlotly({
    sel  <- modelo_sel()
    cols <- ifelse(modelos_hosp_df$Modelo == sel, "#f5a623", "#2a3448")
    plot_ly(x = modelos_hosp_df$Modelo, y = modelos_hosp_df$ROI_10a, type = "bar",
            marker = list(color = cols, cornerradius = 3),
            hovertemplate = "%{y:.1f}%<extra></extra>") |>
      dark_plotly() |>
      layout(yaxis = list(ticksuffix = "%"), xaxis = list(tickfont = list(size = 9)))
  })
  
  output$mh_brecha <- renderPlotly({
    sel  <- modelo_sel()
    cols <- ifelse(modelos_hosp_df$Modelo == sel, "#4ade80", "#2a3448")
    plot_ly(x = modelos_hosp_df$Modelo, y = modelos_hosp_df$Brecha_cv, type = "bar",
            marker = list(color = cols, cornerradius = 3),
            hovertemplate = "%{y}%<extra></extra>") |>
      dark_plotly() |>
      layout(yaxis = list(ticksuffix = "%"), xaxis = list(tickfont = list(size = 9)))
  })
  
  output$mh_camas_dist <- renderPlotly({
    cols_camas <- c("#38bdf8","#c084fc","#f5a623","#4ade80","#fb923c","#f87171","#94a3b8","#64748b")
    plot_ly(
      x = camas_servicio_df$Camas,
      y = camas_servicio_df$Servicio,
      type = "bar", orientation = "h",
      marker = list(color = cols_camas, cornerradius = 3),
      text  = ~paste0(camas_servicio_df$Camas, " camas"),
      textposition = "outside",
      hovertemplate = "<b>%{y}</b>: %{x} camas<extra></extra>"
    ) |>
      dark_plotly() |>
      layout(xaxis = list(range = c(0, 25), tickvals = c(0, 5, 10, 15, 20)),
             yaxis = list(autorange = "reversed"),
             margin = list(l = 170, r = 30))
  })
  
  output$mh_scorecard <- DT::renderDT({
    df        <- modelos_hosp_df
    sel       <- modelo_sel()
    df$ROI_5a  <- paste0('<span style="color:#fb923c;font-weight:600;">', df$ROI_5a,  '%</span>')
    df$ROI_10a <- paste0('<span style="color:#4ade80;font-weight:600;">', df$ROI_10a, '%</span>')
    df$Brecha_cv <- paste0(df$Brecha_cv, "%")
    df$Riesgo  <- paste0('<span style="color:',
                         ifelse(df$Riesgo == "Medio", "#fb923c", "#4ade80"),
                         ';font-weight:600;">', df$Riesgo, '</span>')
    df$Modelo  <- ifelse(df$Modelo == sel,
                         paste0('<span style="color:#f5a623;font-weight:700;">⬤ ', df$Modelo, '</span>'),
                         df$Modelo)
    DT::datatable(df, escape = FALSE, rownames = FALSE,
                  colnames = c("Modelo","Camas","CAPEX","ROI 5a","ROI 10a","Brecha cv.","Riesgo"),
                  options  = list(dom = "t", ordering = FALSE, pageLength = 10),
                  class = "display")
  })
  
  # ─── Pestaña 6: Modelo Financiero — Motor reactivo ─────────────────────────
  fin_data <- reactive({
    req(res_auth$user, input$fin_camas, input$fin_qx, input$fin_ocup,
        input$fin_tarifa_cama, input$fin_tarifa_qx, input$fin_tarifa_urg,
        input$fin_tarifa_cons, input$fin_tarifa_dia,
        input$fin_pct_personal, input$fin_pct_insumos, input$fin_costo_fijo,
        input$fin_capex, input$fin_wacc, input$fin_g_vol, input$fin_g_precio,
        input$fin_inf_gen, input$fin_ramp)
    
    ramp <- RAMP_CURVES[[input$fin_ramp]]
    
    rev_hosp <- input$fin_camas * 365 * (input$fin_ocup / 100) * input$fin_tarifa_cama / 1e6
    rev_qx   <- proy_cirugias * (input$fin_qx / hosp_quirofanos) * input$fin_tarifa_qx / 1e6
    rev_urg  <- proy_urgencias    * input$fin_tarifa_urg  / 1e6
    rev_cons <- proy_consulta_ext * input$fin_tarifa_cons / 1e6
    rev_dia  <- hosp_sillones_dia * 1.5 * 300 * input$fin_tarifa_dia / 1e6
    
    rev_base <- rev_hosp + rev_qx + rev_urg + rev_cons + rev_dia
    
    ingresos_v <- opex_v <- numeric(10)
    for (t in 1:10) {
      crec_vol  <- (1 + input$fin_g_vol   / 100)^(t - 1)
      crec_prec <- (1 + input$fin_g_precio / 100)^(t - 1)
      crec_fijo <- (1 + input$fin_inf_gen  / 100)^(t - 1)
      ingr_t    <- (rev_hosp * ramp[t] +
                      (rev_qx + rev_urg + rev_cons + rev_dia) * ramp[t] * crec_vol) * crec_prec
      ingresos_v[t] <- ingr_t
      opex_v[t]     <- (input$fin_pct_personal + input$fin_pct_insumos) / 100 * ingr_t +
        input$fin_costo_fijo * crec_fijo
    }
    
    ebitda_v   <- ingresos_v - opex_v
    flujo_ac_v <- cumsum(ebitda_v) - input$fin_capex
    
    vpn     <- -input$fin_capex + sum(ebitda_v / (1 + input$fin_wacc / 100)^(1:10))
    tir_val <- tryCatch(calcular_irr(c(-input$fin_capex, ebitda_v)), error = function(e) NA_real_)
    
    cumsum_ebitda <- cumsum(ebitda_v)
    be_idx <- which(cumsum_ebitda >= input$fin_capex)[1]
    be_str <- if (is.na(be_idx)) {
      ">10 años"
    } else if (be_idx == 1) {
      "Año 1.0"
    } else {
      prev_v <- cumsum_ebitda[be_idx - 1]
      curr_v <- cumsum_ebitda[be_idx]
      frac   <- (input$fin_capex - prev_v) / (curr_v - prev_v)
      sprintf("Año %.1f", be_idx - 1 + frac)
    }
    
    calc_escenario <- function(ocup_adj_pp, tarifa_adj_pct) {
      ocup_s <- min(90, max(30, input$fin_ocup + ocup_adj_pp))
      k_tar  <- 1 + tarifa_adj_pct / 100
      rh_s   <- input$fin_camas * 365 * (ocup_s / 100) * input$fin_tarifa_cama * k_tar / 1e6
      rq_s   <- proy_cirugias * (input$fin_qx / hosp_quirofanos) * input$fin_tarifa_qx * k_tar / 1e6
      ru_s   <- proy_urgencias    * input$fin_tarifa_urg  * k_tar / 1e6
      rc_s   <- proy_consulta_ext * input$fin_tarifa_cons * k_tar / 1e6
      rd_s   <- hosp_sillones_dia * 1.5 * 300 * input$fin_tarifa_dia * k_tar / 1e6
      ingr_s <- opex_s <- numeric(10)
      for (t in 1:10) {
        cv <- (1 + input$fin_g_vol   / 100)^(t - 1)
        cp <- (1 + input$fin_g_precio / 100)^(t - 1)
        cf <- (1 + input$fin_inf_gen  / 100)^(t - 1)
        it <- (rh_s * ramp[t] + (rq_s + ru_s + rc_s + rd_s) * ramp[t] * cv) * cp
        ingr_s[t] <- it
        opex_s[t] <- (input$fin_pct_personal + input$fin_pct_insumos) / 100 * it +
          input$fin_costo_fijo * cf
      }
      eb_s  <- ingr_s - opex_s
      fl_s  <- c(-input$fin_capex, eb_s)
      vpn_s <- -input$fin_capex + sum(eb_s / (1 + input$fin_wacc / 100)^(1:10))
      tir_s <- tryCatch(calcular_irr(fl_s), error = function(e) NA_real_)
      ce_s  <- cumsum(eb_s)
      bi_s  <- which(ce_s >= input$fin_capex)[1]
      be_s  <- if (is.na(bi_s)) ">10a" else {
        if (bi_s == 1) "1.0 años" else {
          frc <- (input$fin_capex - ce_s[bi_s - 1]) / (ce_s[bi_s] - ce_s[bi_s - 1])
          sprintf("%.1f años", bi_s - 1 + frc)
        }
      }
      list(vpn = round(vpn_s), tir = round(tir_s, 1), be = be_s, ocup = paste0(ocup_s, "%"))
    }
    
    escenarios <- list(
      list(nombre = "Base",        ocup_adj =  0,  tar_adj =   0, tar_lbl = "Base"),
      list(nombre = "Optimista",   ocup_adj = +10, tar_adj = +10, tar_lbl = "+10%"),
      list(nombre = "Conservador", ocup_adj = -10, tar_adj =  -5, tar_lbl = "-5%"),
      list(nombre = "Competencia", ocup_adj = -15, tar_adj = -10, tar_lbl = "-10%"),
      list(nombre = "Pesimista",   ocup_adj = -25, tar_adj = -15, tar_lbl = "-15%")
    )
    sens_rows <- lapply(escenarios, function(e) {
      r <- calc_escenario(e$ocup_adj, e$tar_adj)
      data.frame(Escenario = e$nombre, Ocupacion = r$ocup, Tarifa = e$tar_lbl,
                 VPN = r$vpn, TIR = r$tir, Breakeven = r$be, stringsAsFactors = FALSE)
    })
    sens_tbl <- do.call(rbind, sens_rows)
    
    list(anos = anos_fin, ingresos = ingresos_v, opex = opex_v,
         ebitda = ebitda_v, flujo_ac = flujo_ac_v,
         vpn = round(vpn), tir = tir_val, breakeven = be_str,
         opex_yr1 = round(opex_v[1], 1), sens = sens_tbl)
  })
  
  output$fin_kpi_capex <- renderText({ req(res_auth$user); paste0("$", input$fin_capex, " MDP") })
  output$fin_kpi_opex1 <- renderText({ req(res_auth$user); paste0("$", fin_data()$opex_yr1, " MDP") })
  output$fin_kpi_be    <- renderText({ req(res_auth$user); fin_data()$breakeven })
  output$fin_kpi_tir   <- renderText({
    req(res_auth$user)
    tir <- fin_data()$tir
    if (is.na(tir)) "N/A" else paste0(tir, "%")
  })
  
  # ── Ingresos vs OPEX — eje X numérico con etiquetas ────────────────────────
  output$fin_ingresos <- renderPlotly({
    req(res_auth$user)
    d <- fin_data()
    plot_ly() |>
      add_bars(x = d$anos, y = round(d$ingresos, 1), name = "Ingresos",
               marker = list(color = "#f5a623", cornerradius = 3),
               hovertemplate = "$%{y:.1f} MDP<extra></extra>") |>
      add_bars(x = d$anos, y = round(d$opex, 1), name = "OPEX",
               marker = list(color = "#38bdf8", cornerradius = 3),
               hovertemplate = "$%{y:.1f} MDP<extra></extra>") |>
      dark_plotly() |>
      layout(barmode = "group",
             xaxis = list(tickvals = 1:10, ticktext = paste0("Año ", 1:10),
                          gridcolor = "#252f45", color = "#64748b"),
             yaxis = list(ticksuffix = "M"),
             showlegend = TRUE,
             legend = list(x = 0, y = 1.12, orientation = "h",
                           font = list(color = "#64748b", size = 9)))
  })
  
  # ── Flujo Acumulado — eje X numérico ───────────────────────────────────────
  output$fin_flujo <- renderPlotly({
    req(res_auth$user)
    d  <- fin_data()
    fc <- round(d$flujo_ac, 1)
    plot_ly(x = d$anos, y = fc, type = "scatter", mode = "lines+markers",
            fill = "tozeroy",
            line      = list(color = "#f5a623", width = 2),
            marker    = list(color = "#f5a623", size = 6),
            fillcolor = "rgba(245,166,35,.12)",
            hovertemplate = "Año %{x}: $%{y:.1f} MDP<extra></extra>") |>
      dark_plotly() |>
      layout(xaxis = list(tickvals = 1:10, ticktext = paste0("Año ", 1:10),
                          gridcolor = "#252f45", color = "#64748b"),
             yaxis = list(ticksuffix = "M"),
             shapes = list(list(type = "line", y0 = 0, y1 = 0, x0 = 1, x1 = 10,
                                line = list(color = "#f87171", dash = "dot", width = 1))))
  })
  
  # ── EBITDA — eje X numérico ─────────────────────────────────────────────────
  output$fin_ebitda <- renderPlotly({
    req(res_auth$user)
    d  <- fin_data()
    eb <- round(d$ebitda, 1)
    cols <- ifelse(eb >= 0, "#4ade80", "#f87171")
    plot_ly(x = d$anos, y = eb, type = "scatter", mode = "lines+markers",
            line   = list(color = "#4ade80", width = 2),
            marker = list(color = cols, size = 7,
                          line = list(color = "#0d1117", width = 1)),
            hovertemplate = "Año %{x}: $%{y:.1f} MDP<extra></extra>") |>
      dark_plotly() |>
      layout(xaxis = list(tickvals = 1:10, ticktext = paste0("Año ", 1:10),
                          gridcolor = "#252f45", color = "#64748b"),
             yaxis = list(ticksuffix = "M"),
             shapes = list(list(type = "line", y0 = 0, y1 = 0, x0 = 1, x1 = 10,
                                line = list(color = "#f87171", dash = "dot", width = 1))))
  })
  
  output$fin_sens <- DT::renderDT({
    req(res_auth$user)
    df <- fin_data()$sens
    df$VPN <- paste0('<span style="color:',
                     ifelse(df$VPN > 200, "#4ade80", ifelse(df$VPN > 50, "#fb923c", "#f87171")),
                     '; font-weight:600;">$', df$VPN, '</span>')
    df$TIR <- paste0('<span style="color:',
                     ifelse(df$TIR > 15, "#4ade80", ifelse(df$TIR > 8, "#fb923c", "#f87171")),
                     '; font-weight:600;">', df$TIR, '%</span>')
    df$Escenario <- paste0('<strong style="color:#e2e8f0;">', df$Escenario, '</strong>')
    DT::datatable(df, escape = FALSE, rownames = FALSE,
                  colnames = c("Escenario","Ocupación","Tarifa","VPN (MDP)","TIR","Breakeven"),
                  options  = list(dom = "t", ordering = FALSE, pageLength = 10),
                  class = "display")
  })
}

shinyApp(ui, server)