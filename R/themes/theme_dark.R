# R/themes/theme_dark.R
# Dark theme configuration and CSS

# ══════════════════════════════════════════════════════════════════════════════

#' Get dark theme CSS
#'
#' Returns the complete CSS string for the dark theme
#'
#' @return character string containing CSS
#'
#' @export
get_dark_css <- function() {
  "
@import url('https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700&family=Rajdhani:wght@500;600;700&display=swap');

*, *::before, *::after { box-sizing: border-box; }

html, body {
  background: #0d1117 !important;
  color: #e2e8f0 !important;
  font-family: 'Inter', sans-serif !important;
  font-size: 13px;
}

.navbar, .navbar-default, nav.navbar {
  background: #161b27 !important;
  border-bottom: 1px solid #252f45 !important;
  box-shadow: 0 1px 8px rgba(0,0,0,.5) !important;
  padding-top: 0 !important;
  padding-bottom: 0 !important;
}
.navbar-brand, .navbar-brand:hover {
  color: #f5a623 !important;
  font-family: 'Rajdhani', sans-serif !important;
  font-size: 18px !important;
  font-weight: 700 !important;
  letter-spacing: .03em;
}
.navbar-nav > li > a,
.navbar-nav > li > a:hover,
.navbar-nav > li > a:focus {
  color: #94a3b8 !important;
  font-size: 12px !important;
  font-weight: 500;
  padding: 14px 14px !important;
  transition: color .15s;
}
.navbar-nav > li.active > a,
.navbar-nav > li > a:hover {
  color: #f5a623 !important;
  border-bottom: 2px solid #f5a623;
  background: transparent !important;
}

.bslib-sidebar-layout > .sidebar {
  background: #161b27 !important;
  border-right: 1px solid #252f45 !important;
}
.sidebar-title { color: #38bdf8 !important; font-weight: 700 !important; font-size: 14px !important; }
.sidebar .form-label, .sidebar label { color: #94a3b8 !important; font-size: 11px !important; font-weight: 500; }
.sidebar .help-block, .sidebar .form-text { color: #64748b !important; font-size: 10px !important; }
.sidebar hr { border-color: #252f45 !important; }
.sidebar h5 { color: #e2e8f0 !important; font-size: 12px !important; font-weight: 600; margin-bottom: 6px; }

.form-control, .selectize-input, .form-select {
  background: #1c2333 !important;
  border: 1px solid #252f45 !important;
  color: #e2e8f0 !important;
  border-radius: 6px !important;
  font-size: 12px !important;
}
.form-control:focus { border-color: #38bdf8 !important; box-shadow: 0 0 0 2px rgba(56,189,248,.15) !important; }
.selectize-dropdown { background: #1c2333 !important; border: 1px solid #252f45 !important; }
.selectize-dropdown .option { color: #e2e8f0 !important; }
.selectize-dropdown .option:hover, .selectize-dropdown .active { background: #252f45 !important; }

.irs--shiny .irs-bar { background: #38bdf8 !important; border-top-color: #38bdf8 !important; border-bottom-color: #38bdf8 !important; }
.irs--shiny .irs-handle { background: #f5a623 !important; border-color: #f5a623 !important; }
.irs--shiny .irs-from, .irs--shiny .irs-to, .irs--shiny .irs-single { background: #f5a623 !important; }
.irs--shiny .irs-line { background: #252f45 !important; }
.irs--shiny .irs-grid-text { color: #64748b !important; }
.irs--shiny .irs-min, .irs--shiny .irs-max { color: #64748b !important; }

.checkbox label { color: #94a3b8 !important; font-size: 12px !important; }
input[type='checkbox'] { accent-color: #38bdf8; }
.radio label { color: #94a3b8 !important; font-size: 12px !important; }

.card, .bslib-card {
  background: #161b27 !important;
  border: 1px solid #252f45 !important;
  border-radius: 10px !important;
  box-shadow: 0 4px 16px rgba(0,0,0,.3) !important;
  margin-bottom: 16px;
}
.card-header, .bslib-card > .card-header {
  background: #1c2333 !important;
  border-bottom: 1px solid #252f45 !important;
  padding: 10px 16px !important;
}
.card-header h4 {
  color: #38bdf8 !important;
  font-size: 13px !important;
  font-weight: 600 !important;
  margin: 0 !important;
}
.card-body { padding: 14px 16px !important; }

.bslib-value-box {
  background: #161b27 !important;
  border: 1px solid #252f45 !important;
  border-radius: 10px !important;
}
.value-box-title { color: #64748b !important; font-size: 10px !important; text-transform: uppercase; letter-spacing: .08em; }
.value-box-value { color: #e2e8f0 !important; font-family: 'Rajdhani', sans-serif !important; font-size: 28px !important; font-weight: 700 !important; }
.value-box-showcase .bi { color: #f5a623 !important; }

.dataTables_wrapper { background: transparent !important; }
table.dataTable thead th {
  background: #0d1117 !important; color: #64748b !important;
  border-bottom: 1px solid #252f45 !important;
  font-size: 10px !important; font-weight: 600; letter-spacing: .06em; text-transform: uppercase;
}
table.dataTable tbody tr:nth-child(odd)  td { background: #161b27 !important; color: #e2e8f0 !important; }
table.dataTable tbody tr:nth-child(even) td { background: #1c2333 !important; color: #e2e8f0 !important; }
table.dataTable tbody tr:hover td        { background: #252f45 !important; color: #ffffff !important; }
table.dataTable tbody td {
  border-color: #252f45 !important;
  padding: 8px 10px !important; font-size: 12px !important;
}
.dataTables_info, .dataTables_length label, .dataTables_filter label { color: #64748b !important; font-size: 11px !important; }
.dataTables_paginate .paginate_button { color: #64748b !important; }
.dataTables_paginate .paginate_button.current,
.dataTables_paginate .paginate_button.current:hover {
  background: #f5a623 !important; color: #000 !important; border-radius: 4px; border: none !important;
}

.shiny-html-output table, .table {
  color: #e2e8f0 !important;
  border-color: #252f45 !important;
  font-size: 12px !important;
}
.table thead th { background: #0d1117 !important; color: #64748b !important; border-bottom: 1px solid #252f45 !important; }
.table tbody tr:nth-child(odd)  td { background: #161b27 !important; color: #e2e8f0 !important; }
.table tbody tr:nth-child(even) td { background: #1c2333 !important; color: #e2e8f0 !important; }
.table tbody tr:hover td { background: #252f45 !important; color: #ffffff !important; }
.table-striped > tbody > tr:nth-of-type(odd)  > * { background: #161b27 !important; color: #e2e8f0 !important; }
.table-striped > tbody > tr:nth-of-type(even) > * { background: #1c2333 !important; color: #e2e8f0 !important; }
.table-hover > tbody > tr:hover > * { background: #252f45 !important; color: #ffffff !important; }

.leaflet-container { background: #0d1117 !important; }
.leaflet-popup-content-wrapper { background: #161b27 !important; color: #e2e8f0 !important; border: 1px solid #252f45; }
.leaflet-popup-tip { background: #161b27 !important; }
.leaflet-control-layers { background: #161b27 !important; color: #e2e8f0 !important; border: 1px solid #252f45 !important; }
.leaflet-control-layers label { color: #94a3b8 !important; }
.leaflet-control-zoom a { background: #161b27 !important; color: #e2e8f0 !important; border-color: #252f45 !important; }
.leaflet-control-attribution { background: rgba(13,17,23,.8) !important; color: #64748b !important; }

.node text { fill: #e2e8f0 !important; font-family: 'Inter', sans-serif !important; }
.link:hover { stroke-opacity: .85 !important; }

.kpi-row { display: flex; gap: 12px; margin-bottom: 16px; flex-wrap: wrap; }
.kpi-card {
  flex: 1; min-width: 140px;
  background: #161b27;
  border: 1px solid #252f45;
  border-radius: 10px;
  padding: 14px 16px;
}
.kpi-label { font-size: 9px; font-weight: 600; letter-spacing: .08em; color: #64748b; text-transform: uppercase; margin-bottom: 6px; }
.kpi-value { font-family: 'Rajdhani', sans-serif; font-size: 26px; font-weight: 700; color: #e2e8f0; line-height: 1; }
.kpi-sub   { font-size: 10px; color: #64748b; margin-top: 4px; }
.kpi-up    { color: #4ade80 !important; }
.kpi-warn  { color: #fb923c !important; }
.kpi-down  { color: #f87171 !important; }

.bench-row { display: flex; gap: 10px; margin-bottom: 16px; flex-wrap: wrap; }
.bench-card {
  flex: 1; min-width: 130px;
  background: #1c2333; border: 1px solid #252f45;
  border-radius: 10px; padding: 14px 16px; text-align: center;
}
.bench-card .bval { font-family: 'Rajdhani', sans-serif; font-size: 30px; font-weight: 700; line-height: 1; }
.bench-card .blbl { font-size: 9px; color: #64748b; text-transform: uppercase; letter-spacing: .07em; margin-top: 4px; }
.bench-card .bsub { font-size: 10px; color: #94a3b8; margin-top: 3px; }

.model-tabs { display: flex; gap: 8px; margin-bottom: 16px; flex-wrap: wrap; }
.model-tab {
  padding: 8px 14px;
  border: 1px solid #252f45;
  border-radius: 6px; cursor: pointer;
  font-size: 11px; font-weight: 500; color: #94a3b8;
  background: #1c2333; transition: all .15s;
}
.model-tab:hover { border-color: #38bdf8; color: #38bdf8; }
.model-tab.active { background: #f5a623; border-color: #f5a623; color: #000; font-weight: 700; }

.metric-row { display: flex; gap: 10px; margin-bottom: 10px; flex-wrap: wrap; }
.metric-box {
  flex: 1; min-width: 90px;
  background: #1c2333; border: 1px solid #252f45;
  border-radius: 8px; padding: 10px 12px; text-align: center;
}
.metric-box .val { font-family: 'Rajdhani', sans-serif; font-size: 20px; font-weight: 700; color: #e2e8f0; }
.metric-box .lbl { font-size: 9px; color: #64748b; text-transform: uppercase; letter-spacing: .05em; }

.verdict-box {
  background: rgba(74,222,128,.07);
  border: 1px solid rgba(74,222,128,.25);
  border-radius: 10px; padding: 16px 20px; margin-bottom: 16px;
}
.verdict-title { font-size: 14px; font-weight: 700; color: #4ade80; margin-bottom: 6px; }
.verdict-body  { font-size: 12px; color: #94a3b8; line-height: 1.7; }
.verdict-body strong { color: #f5a623; }

.caution-box {
  background: rgba(248,113,113,.06);
  border: 1px solid rgba(248,113,113,.25);
  border-radius: 10px; padding: 16px 20px; margin-bottom: 16px;
}
.caution-title { font-size: 13px; font-weight: 700; color: #f87171; margin-bottom: 8px; }
.caution-body  { font-size: 12px; color: #94a3b8; line-height: 1.75; }
.caution-body strong { color: #fb923c; }

.info-box {
  background: rgba(56,189,248,.06);
  border: 1px solid rgba(56,189,248,.2);
  border-radius: 10px; padding: 14px 18px; margin-bottom: 14px;
}
.info-box p { font-size: 12px; color: #94a3b8; line-height: 1.7; margin: 0; }
.info-box strong { color: #38bdf8; }

/* Echauri reference box */
.echauri-box {
  background: rgba(192,132,252,.06);
  border: 1px solid rgba(192,132,252,.25);
  border-radius: 10px; padding: 14px 18px; margin-bottom: 14px;
}
.echauri-box .etitle { font-size: 12px; font-weight: 700; color: #c084fc; margin-bottom: 8px; }
.echauri-box .ebody  { font-size: 11px; color: #94a3b8; line-height: 1.75; }
.echauri-box .ebody strong { color: #f5a623; }
.echauri-metric { display:inline-block; background:#1c2333; border:1px solid #252f45; border-radius:6px; padding:5px 10px; margin:3px 4px; font-size:11px; color:#e2e8f0; }
.echauri-metric span { color:#c084fc; font-weight:700; margin-right:4px; }

.rec-grid { display: flex; gap: 12px; margin-bottom: 12px; flex-wrap: wrap; }
.rec-card { flex: 1; min-width: 220px; background: #161b27; border: 1px solid #252f45; border-radius: 10px; padding: 14px 16px; }
.rec-card-label { font-size: 9px; color: #64748b; text-transform: uppercase; letter-spacing: .08em; margin-bottom: 6px; }
.rec-card-value { font-size: 13px; font-weight: 600; color: #e2e8f0; }
.priority-list { list-style: none; padding: 0; }
.priority-list li { display: flex; align-items: center; gap: 10px; padding: 7px 0; border-bottom: 1px solid #252f45; font-size: 12px; color: #94a3b8; }
.priority-num { width: 22px; height: 22px; background: #f5a623; border-radius: 50%; display: flex; align-items: center; justify-content: center; font-size: 10px; font-weight: 700; color: #000; flex-shrink: 0; }
.strategy-list { list-style: none; padding: 0; }
.strategy-list li { display: flex; align-items: flex-start; gap: 8px; padding: 7px 0; border-bottom: 1px solid #252f45; font-size: 11px; color: #94a3b8; line-height: 1.6; }
.dot-ring { width: 12px; height: 12px; border: 2px solid #f5a623; border-radius: 50%; margin-top: 3px; flex-shrink: 0; }
.risk-list { list-style: none; padding: 0; }
.risk-list li { display: flex; align-items: flex-start; gap: 8px; font-size: 11px; color: #94a3b8; padding: 4px 0; line-height: 1.6; }
.risk-dot { color: #f87171; font-size: 14px; }

.cond-grid { display: flex; gap: 12px; flex-wrap: wrap; margin-bottom: 14px; }
.cond-card { flex: 1; min-width: 200px; background: #1c2333; border: 1px solid #252f45; border-radius: 10px; padding: 14px 16px; }
.cond-card-label { font-size: 9px; color: #64748b; text-transform: uppercase; letter-spacing: .08em; margin-bottom: 8px; }
.cond-list { list-style: none; padding: 0; margin: 0; }
.cond-list li { font-size: 11px; color: #94a3b8; padding: 5px 0; border-bottom: 1px solid #1c2333; line-height: 1.5; display: flex; gap: 8px; align-items: flex-start; }
.cond-icon { flex-shrink: 0; margin-top: 1px; }

.page-section { padding: 6px 0 16px; }
.section-badge {
  display: inline-flex; align-items: center; gap: 6px;
  background: rgba(245,166,35,.12); border: 1px solid rgba(245,166,35,.3);
  border-radius: 4px; padding: 2px 10px;
  font-size: 10px; font-weight: 600; color: #f5a623; margin-bottom: 8px;
}
.section-title { font-family: 'Rajdhani', sans-serif; font-size: 24px; font-weight: 700; color: #e2e8f0; margin-bottom: 2px; }
.section-sub { font-size: 11px; color: #64748b; margin-bottom: 16px; }

.chart-row { display: flex; gap: 14px; margin-bottom: 14px; }
.chart-panel { flex: 1; background: #161b27; border: 1px solid #252f45; border-radius: 10px; padding: 14px; }
.chart-panel.full { flex: none; width: 100%; }
.chart-title { font-size: 11px; font-weight: 600; color: #e2e8f0; margin-bottom: 10px; }

::-webkit-scrollbar { width: 5px; height: 5px; }
::-webkit-scrollbar-track { background: transparent; }
::-webkit-scrollbar-thumb { background: #252f45; border-radius: 3px; }

body { padding-top: 58px !important; }

.bslib-sidebar-layout > .sidebar,
.bslib-sidebar-layout > .main {
  max-height: calc(100vh - 58px) !important;
  overflow-y: auto !important;
}

/* Prevent cards and panels from overflowing horizontally */
.bslib-card .card-body,
.chart-panel {
  overflow-x: auto !important;
  min-width: 0 !important;
}

/* Responsive column wrapping — columns stack below 900px */
@media (max-width: 900px) {
  .chart-row { flex-direction: column !important; }
  .kpi-row   { flex-wrap: wrap !important; }
  .bench-row { flex-wrap: wrap !important; }
  .metric-row { flex-wrap: wrap !important; }
}
"
}

# ══════════════════════════════════════════════════════════════════════════════

#' Get dark theme configuration
#'
#' Returns theme configuration including colors, fonts, and sizes
#'
#' @return list with theme configuration
#'
#' @export
get_dark_theme <- function() {
  list(
    colors = list(
      bg     = "#0d1117",
      panel  = "#161b27",
      panel2 = "#1c2333",
      border = "#252f45",
      gold   = "#f5a623",
      cyan   = "#38bdf8",
      green  = "#4ade80",
      red    = "#f87171",
      orange = "#fb923c",
      purple = "#c084fc",
      text   = "#e2e8f0",
      muted  = "#64748b"
    ),
    fonts = list(
      base    = "'Inter', sans-serif",
      heading = "'Rajdhani', sans-serif"
    ),
    sizes = list(
      base_font  = "13px",
      small_font = "11px",
      large_font = "16px"
    )
  )
}
