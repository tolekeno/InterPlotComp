# Development entry point: run the application straight from a source checkout
# without installing the package.
#
#   shiny::runApp()
#
# R/_disable_autoload.R stops Shiny auto-sourcing R/, so the load order here is
# authoritative. Installed users should call InterPlotComp::run_app() instead;
# this file is excluded from the built package by .Rbuildignore.
for (.f in sort(list.files("R", pattern = "[.][Rr]$", full.names = TRUE))) {
  source(.f, local = FALSE, encoding = "UTF-8")
}
rm(.f)

check_required_packages()
options(shiny.maxRequestSize = 250 * 1024^2)
if (has_pkg("thematic")) {
  thematic::thematic_shiny(font = "auto", bg = PAL$surface, fg = PAL$ink,
                           accent = PAL$primary)
}

shiny::shinyApp(ui = app_ui(), server = app_server)
