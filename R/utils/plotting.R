# R/utils/plotting.R
# Plotting utilities and theme helpers

# ══════════════════════════════════════════════════════════════════════════════

#' Primary color palette
#' @export
PALETTE_PRIMARY <- c("#f5a623", "#38bdf8", "#4ade80", "#f87171", "#c084fc", "#fb923c")

#' Extended color palette for charts
#' @export
PALETTE_EXTENDED <- c(
  "#f5a623", "#38bdf8", "#4ade80", "#f87171", "#c084fc", "#fb923c",
  "#e2e8f0", "#67e8f9", "#fde68a", "#a78bfa", "#34d399", "#f472b6"
)

#' Color scheme configuration
#' @export
COLOR_SCHEME <- list(
  primary   = "#f5a623",
  secondary = "#38bdf8",
  success   = "#4ade80",
  danger    = "#f87171",
  warning   = "#fb923c",
  info      = "#38bdf8",
  purple    = "#c084fc",
  text      = "#e2e8f0",
  muted     = "#64748b"
)

# ══════════════════════════════════════════════════════════════════════════════

#' Apply dark theme to plotly chart
#'
#' Applies consistent dark theme styling to plotly visualizations
#'
#' @param p plotly object to style
#' @param ... additional layout parameters passed to plotly::layout()
#'
#' @return modified plotly object with dark theme
#'
#' @examples
#' library(plotly)
#' p <- plot_ly(x = 1:10, y = 1:10)
#' dark_plotly(p)
#'
#' @export
dark_plotly <- function(p, ...) {
  p |>
    layout(
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor  = "rgba(0,0,0,0)",
      font          = list(color = "#64748b", size = 10, family = "Inter"),
      xaxis         = list(gridcolor = "#252f45", zerolinecolor = "#252f45", color = "#64748b"),
      yaxis         = list(gridcolor = "#252f45", zerolinecolor = "#252f45", color = "#64748b"),
      margin        = list(t = 8, b = 30, l = 40, r = 10),
      ...
    ) |>
    config(displayModeBar = FALSE)
}
