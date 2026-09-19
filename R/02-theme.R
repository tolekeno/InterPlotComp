# ---------------------------------------------------------------------------
# Visual design system
#
# One palette drives both the Bootstrap 5 theme and every figure, so that
# on-screen output and exported publication figures are visually identical.
# Fonts are a system stack rather than a web font: the application must render
# correctly on an offline analysis machine or an institutional server with no
# outbound internet access.
# ---------------------------------------------------------------------------

# Shown in the navigation bar. Kept in step with the Version field of
# DESCRIPTION; read from there when the package is installed so the two
# cannot drift, with a literal fallback for a source checkout.
APP_VERSION <- tryCatch(
  as.character(utils::packageVersion("InterPlotComp")),
  error = function(e) "3.5.0"
)

# Palette: field greens for structure, warm earth for competition, a
# colour-blind-safe diverging ramp for correlations and spatial residuals.
PAL <- list(
  ink        = "#14261F",
  body       = "#32433B",
  muted      = "#6B7F74",
  line       = "#D8E2DB",
  surface    = "#FFFFFF",
  canvas     = "#F4F7F5",
  primary    = "#1B6B4A",
  primary_dk = "#11543A",
  primary_lt = "#E6F1EB",
  accent     = "#C77A2E",
  accent_lt  = "#FBF0E3",
  info       = "#2A6F97",
  success    = "#1B6B4A",
  warning    = "#B8860B",
  danger     = "#B3412C",
  # Series colours for the three effect types used throughout the app.
  direct     = "#1B6B4A",
  competition= "#C77A2E",
  pure       = "#2A6F97"
)

# Diverging ramp for genetic correlations and residual maps (ColorBrewer RdBu,
# reversed so that positive is warm). Perceptually ordered and safe for the
# common forms of colour-vision deficiency.
DIVERGING <- c(
  "#053061", "#2166AC", "#4393C3", "#92C5DE", "#D1E5F0",
  "#F7F7F7",
  "#FDDBC7", "#F4A582", "#D6604D", "#B2182B", "#67001F"
)

# Sequential ramp for yield maps.
SEQUENTIAL <- c("#F7FCF5", "#C7E9C0", "#74C476", "#31A354", "#1B6B4A", "#0B3C27")

FONT_STACK <- paste(
  '"Segoe UI"', "Roboto", '"Helvetica Neue"', "Arial",
  '"Noto Sans"', "sans-serif", sep = ", "
)

MONO_STACK <- paste('"Cascadia Mono"', '"SFMono-Regular"', "Consolas",
                    '"Liberation Mono"', "monospace", sep = ", ")

#' Bootstrap 5 theme for the whole application.
#' @noRd
app_theme <- function() {
  bslib::bs_theme(
    version           = 5,
    bg                = PAL$surface,
    fg                = PAL$ink,
    primary           = PAL$primary,
    secondary         = PAL$muted,
    success           = PAL$success,
    info              = PAL$info,
    warning           = PAL$warning,
    danger            = PAL$danger,
    base_font         = FONT_STACK,
    heading_font      = FONT_STACK,
    code_font         = MONO_STACK,
    "body-bg"         = PAL$canvas,
    "navbar-bg"       = PAL$primary_dk,
    "border-color"    = PAL$line,
    "card-border-color" = PAL$line,
    "card-cap-bg"     = PAL$primary_lt,
    "border-radius"   = "0.6rem",
    "font-size-base"  = "0.94rem",
    "table-striped-bg" = "#F7FAF8"
  )
}

#' Additional CSS that Bootstrap variables alone cannot express.
#' @noRd
app_css <- function() {
  htmltools::HTML(sprintf("
    .navbar-brand { font-weight: 650; letter-spacing: .2px; }
    .nav-tabs .nav-link { font-weight: 550; }

    /* Compact, legible section headings inside cards. */
    .card-header { font-weight: 600; font-size: .95rem; }
    .section-note { color: %s; font-size: .82rem; line-height: 1.45; }
    .section-note code { font-size: .8rem; }

    /* Status banners. */
    .status { padding: .75rem .9rem; border-radius: .5rem; border-left: 4px solid;
              font-size: .88rem; line-height: 1.5; }
    .status-ok   { background: %s; border-color: %s; }
    .status-warn { background: %s; border-color: %s; }
    .status-bad  { background: #FDEEEA; border-color: %s; }
    .status b, .status strong { font-weight: 650; }

    /* Metric strip used at the top of the results panels. */
    .metric-row { display: flex; flex-wrap: wrap; gap: .6rem; margin-bottom: .9rem; }
    .metric { flex: 1 1 150px; background: %s; border: 1px solid %s;
              border-radius: .55rem; padding: .6rem .75rem; }
    .metric .metric-label { font-size: .72rem; text-transform: uppercase;
              letter-spacing: .5px; color: %s; font-weight: 600; }
    .metric .metric-value { font-size: 1.32rem; font-weight: 650; color: %s;
              line-height: 1.25; }
    .metric .metric-sub { font-size: .75rem; color: %s; }

    /* Figure toolbars. */
    .fig-toolbar { display: flex; justify-content: flex-end; gap: .4rem;
                   margin-bottom: .4rem; }

    /* Keep wide tables inside their card rather than the page. */
    .table-scroll { overflow-x: auto; }
    table.dataTable { font-size: .86rem; }

    /* Step markers on the workflow tabs. */
    .step-badge { display: inline-block; min-width: 1.35rem; text-align: center;
                  background: %s; color: #fff; border-radius: .35rem;
                  font-size: .75rem; font-weight: 700; margin-right: .4rem;
                  padding: .05rem .25rem; }

    .licence-box { font-size: .84rem; }

    @media (max-width: 575px) {
      .metric { flex: 1 1 100%%; }
    }
  ",
  PAL$muted,
  PAL$primary_lt, PAL$primary, PAL$accent_lt, PAL$warning, PAL$danger,
  PAL$surface, PAL$line, PAL$muted, PAL$ink, PAL$muted,
  PAL$primary))
}

#' Shared ggplot2 theme for screen and export.
#'
#' `base_size` is raised for exported figures so that text scales with the
#' larger canvas instead of becoming unreadably small at 600 dpi.
#' @noRd
theme_trial <- function(base_size = 12, grid = "xy") {
  th <- ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      text             = ggplot2::element_text(colour = PAL$body),
      plot.title       = ggplot2::element_text(face = "bold", size = base_size * 1.2,
                                               colour = PAL$ink,
                                               margin = ggplot2::margin(b = 3)),
      plot.subtitle    = ggplot2::element_text(size = base_size * 0.92, colour = PAL$muted,
                                               margin = ggplot2::margin(b = 9)),
      plot.caption     = ggplot2::element_text(size = base_size * 0.78, colour = PAL$muted,
                                               hjust = 0,
                                               margin = ggplot2::margin(t = 9)),
      plot.title.position   = "plot",
      plot.caption.position = "plot",
      axis.title       = ggplot2::element_text(size = base_size * 0.92, colour = PAL$body),
      axis.text        = ggplot2::element_text(size = base_size * 0.84, colour = PAL$muted),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(colour = PAL$line, linewidth = 0.35),
      panel.background = ggplot2::element_rect(fill = PAL$surface, colour = NA),
      plot.background  = ggplot2::element_rect(fill = PAL$surface, colour = NA),
      legend.position  = "bottom",
      legend.title     = ggplot2::element_text(size = base_size * 0.84, face = "bold"),
      legend.text      = ggplot2::element_text(size = base_size * 0.82),
      # A horizontal colour bar at the bottom needs a wide, short key or its
      # break labels collide with one another.
      legend.key.height = grid::unit(0.6, "lines"),
      legend.key.width  = grid::unit(3.2, "lines"),
      strip.text       = ggplot2::element_text(face = "bold", size = base_size * 0.9,
                                               colour = PAL$ink),
      strip.background = ggplot2::element_rect(fill = PAL$primary_lt, colour = NA),
      plot.margin      = ggplot2::margin(10, 12, 8, 10)
    )
  if (grid == "x") th <- th + ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
  if (grid == "y") th <- th + ggplot2::theme(panel.grid.major.x = ggplot2::element_blank())
  if (grid == "none") th <- th + ggplot2::theme(panel.grid.major = ggplot2::element_blank())
  th
}

#' Effect-type colour scale shared by all figures.
#' @noRd
effect_colours <- function() {
  c(Direct = PAL$direct, Competition = PAL$competition, `Pure stand` = PAL$pure,
    Neighbour = PAL$competition)
}
