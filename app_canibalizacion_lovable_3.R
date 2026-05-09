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
source("R/models/financial_model.R")
source("R/models/capacity_model.R")
source("R/modules/mod_market_simulation.R")
source("R/modules/mod_capacity_calculator.R")
source("R/modules/mod_market_analysis.R")
source("R/modules/mod_service_gap.R")
source("R/modules/mod_financial_analysis.R")


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

# Factores de calibración de capacidad definidos en R/models/capacity_model.R
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

# calcular_irr: sourced from R/models/financial_model.R (returns decimal; ×100 at call sites)

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
    icon  = bsicons::bs_icon("graph-up-arrow"),
    mod_market_simulation_ui("market_sim")
  ),
  
  # ── 2. Calculadora de Capacidad ─────────────────────────────────────────────
  nav_panel(
    title = "Calculadora",
    icon  = bsicons::bs_icon("calculator-fill"),
    mod_capacity_calculator_ui("capacity_calc")
  ),
  
  # ── 3. Análisis de Mercado ──────────────────────────────────────────────────
  nav_panel(
    title = "Análisis de Mercado",
    icon  = bsicons::bs_icon("pie-chart-fill"),
    mod_market_analysis_ui("market_analysis")
  ),
  
  # ── 4. Brecha de Servicios ──────────────────────────────────────────────────
  nav_panel(
    title = "Brecha de Servicios",
    icon  = bsicons::bs_icon("clipboard-pulse"),
    mod_service_gap_ui("service_gap")
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
  
  # ── 6. Modelo de Negocio ─────────────────────────────────────────────────────
  nav_panel(
    title = "Modelo de Negocio",
    icon  = bsicons::bs_icon("currency-dollar"),
    mod_financial_analysis_ui("financial")
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
  
  # ── Módulo: Simulación de Mercado ────────────────────────────────────────────
  shared_data <- reactiveValues(
    oferta_actual     = oferta_actual,
    demanda           = demanda,
    proyectos_futuros = proyectos_futuros,
    denue_salud       = denue_salud
  )

  market_sim_results <- mod_market_simulation_server("market_sim", shared_data)

  # ── Módulo: Análisis de Mercado ───────────────────────────────────────────
  market_analysis_results <- mod_market_analysis_server(
    "market_analysis",
    market_data = market_sim_results
  )

  # ─── Pestaña 2: Calculadora de Capacidad ───────────────────────────────────
  capacity_calc_results <- mod_capacity_calculator_server("capacity_calc")

  # Opcional: Acceder a resultados desde el server principal
  # capacity_calc_results$capacidad_results()
  # capacity_calc_results$camas_censables()
  
  # ─── Pestaña 4: Brecha de Servicios ─────────────────────────────────────────
  service_gap_results <- mod_service_gap_server("service_gap",
                           market_data = market_sim_results)

  # ── Módulo: Análisis Financiero ───────────────────────────────────────────
  financial_results <- mod_financial_analysis_server(
    "financial",
    project_data = market_sim_results
  )

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
  
}

shinyApp(ui, server)