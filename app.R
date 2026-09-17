# ===========================================================================
# Inter-plot Competition Analysis for Plant Breeding Trials
#
# A Shiny front end to ASReml-R for fitting direct-competition mixed models to
# unbordered single-row-plot trials, in a single trial or across a series of
# environments.
#
# ASReml-R is commercial software licensed by VSNi. This application supplies
# no licence of any kind: it uses whatever ASReml-R installation and activated
# licence already exist in the R process running it, and it will not compute
# without one. It never reads, writes, embeds, stores or transmits a licence key.
#
# Source layout (R/ is loaded in numeric order by the loop below):
#   00-packages       dependency checks, optional-feature flags
#   01-utils          small shared helpers
#   02-theme          Bootstrap 5 theme, palette, ggplot theme
#   03-data-prep      validation, field indexing, grid padding, neighbours
#   04-simulate       worked example data
#   05-model-core     ASReml interface, variance extraction, exact PEVs
#   06-model-single   single-trial model
#   07-model-met      multi-environment model
#   08-plots          publication-quality figures
#   09-download       high-resolution figure and table export
#   10-ui-components  reusable interface pieces
#   11-mod-single     single-trial workspace
#   12-mod-met        multi-environment workspace
#   13-mod-guide      guide and about
# ===========================================================================

# R/_disable_autoload.R stops Shiny auto-sourcing R/, so the load order here is
# authoritative and the app also works when sourced directly.
for (.f in sort(list.files("R", pattern = "^[0-9].*[.][Rr]$", full.names = TRUE))) {
  source(.f, local = FALSE, encoding = "UTF-8")
}
rm(.f)

check_required_packages()

# 250 MB covers a large multi-environment fieldbook; adjust for bigger uploads.
options(shiny.maxRequestSize = 250 * 1024^2)

# Match static plot styling to the Bootstrap theme where thematic is available.
if (has_pkg("thematic")) {
  thematic::thematic_shiny(font = "auto", bg = PAL$surface, fg = PAL$ink,
                           accent = PAL$primary)
}

ui <- bslib::page_navbar(
  title = shiny::tagList(ic("bounding-box"), " Inter-plot Competition"),
  id = "main_nav",
  theme = app_theme(),
  # Normal document flow, not a fillable flex layout. Every plot in this
  # application already has an explicit height, so filling the viewport buys
  # nothing, while it makes short cards collapse to zero height inside the
  # flex container - which silently hid the summary tables.
  fillable = FALSE,
  window_title = "Inter-plot Competition Analysis",
  header = shiny::tags$head(
    shiny::tags$style(app_css()),
    shiny::tags$meta(name = "viewport",
                     content = "width=device-width, initial-scale=1, viewport-fit=cover")
  ),

  bslib::nav_panel("Single trial", icon = ic("bounding-box-circles"),
                   single_ui("single")),
  bslib::nav_panel("Multi-environment", icon = ic("globe-americas"),
                   met_ui("met")),
  bslib::nav_panel("Guide", icon = ic("book"), guide_ui("guide")),

  bslib::nav_spacer(),
  bslib::nav_item(
    shiny::tags$span(class = "navbar-text small text-white-50",
                     sprintf("v%s · ASReml-R engine", APP_VERSION))
  )
)

server <- function(input, output, session) {
  single_server("single")
  met_server("met")
  guide_server("guide")

  # A missing or unlicensed ASReml-R is not fatal to the interface - users can
  # still explore the data panels - but it must be stated immediately rather
  # than discovered when a long upload finally fails to fit.
  if (!asreml_installed()) {
    shiny::showNotification(
      shiny::tagList(
        shiny::strong("ASReml-R was not found in this R library."), shiny::br(),
        "Data inspection will work, but no model can be fitted. Install and ",
        "activate ASReml-R in this R installation, then restart the application."
      ),
      type = "error", duration = NULL)
  }
}

shiny::shinyApp(ui, server)
