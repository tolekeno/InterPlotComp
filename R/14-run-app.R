# ---------------------------------------------------------------------------
# Application entry point
#
# The interface is assembled by functions rather than by a top-level app.R, so
# that it can be launched from an installed package, from a development
# checkout, and from a Shiny Server deployment without three copies of the
# layout.
# ---------------------------------------------------------------------------

#' User interface for the inter-plot competition application
#'
#' Assembles the navigation bar, theme and the three workspaces. Exposed mainly
#' so that a Shiny Server or 'shinyapps.io' deployment can build the interface
#' directly; most users call [run_app()] instead.
#'
#' @return A [shiny::tagList()]-compatible UI definition.
#' @export
#' @examples
#' ui <- app_ui()
#' class(ui)
app_ui <- function() {
  bslib::page_navbar(
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
      shiny::tags$meta(
        name = "viewport",
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
                       sprintf("v%s \u00b7 ASReml-R engine", APP_VERSION))
    )
  )
}

#' Server logic for the inter-plot competition application
#'
#' @param input,output,session Standard Shiny server arguments, supplied by
#'   [shiny::shinyApp()].
#' @return Called for its side effects; returns `NULL` invisibly.
#' @export
app_server <- function(input, output, session) {
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
  invisible(NULL)
}

#' Launch the inter-plot competition application
#'
#' Starts the 'Shiny' interface for fitting direct-competition mixed models to
#' single-row-plot breeding trials.
#'
#' Model fitting requires 'ASReml-R', which is commercial software licensed by
#' VSNi and is not supplied by this package. The interface opens and data can be
#' inspected without it, so a missing installation is visible immediately rather
#' than after a long upload; but no model can be fitted until 'ASReml-R' is
#' installed and licensed in the same R installation that runs the application.
#'
#' @param launch.browser Open the application in the default browser.
#' @param port Port to listen on. `NULL` lets Shiny choose a free one.
#' @param host Interface to bind to. The default binds to the loopback address
#'   only, so the application is not exposed to the network.
#' @param max_upload_mb Largest file that may be uploaded, in megabytes.
#' @param quiet Suppress the start-up report of missing optional packages.
#' @param ... Further arguments passed to [shiny::runApp()].
#'
#' @return Called for its side effect of running the application. Does not
#'   return until the application is stopped.
#' @export
#' @examples
#' # Launch the interface (interactive sessions only):
#' if (interactive()) {
#'   run_app()
#' }
run_app <- function(launch.browser = interactive(), port = NULL,
                    host = "127.0.0.1", max_upload_mb = 250, quiet = FALSE,
                    ...) {
  check_required_packages()
  if (!isTRUE(quiet)) report_optional_packages()

  old <- options(shiny.maxRequestSize = max_upload_mb * 1024^2)
  on.exit(options(old), add = TRUE)

  # Match static plot styling to the Bootstrap theme where thematic is present.
  if (has_pkg("thematic")) {
    thematic::thematic_shiny(font = "auto", bg = PAL$surface, fg = PAL$ink,
                             accent = PAL$primary)
    on.exit(thematic::thematic_off(), add = TRUE)
  }

  app <- shiny::shinyApp(ui = app_ui(), server = app_server)
  shiny::runApp(app, launch.browser = launch.browser, port = port,
                host = host, ...)
}

#' Report which optional packages are absent, and what each would add.
#'
#' @return Called for its message output; returns `NULL` invisibly.
#' @noRd
report_optional_packages <- function() {
  status <- optional_package_status()
  missing <- status[!status$Installed, , drop = FALSE]
  if (!nrow(missing)) return(invisible(NULL))
  message(
    "Optional packages not installed (each adds one feature):\n",
    paste0("  ", format(missing$Package), "  ", missing$Enables, collapse = "\n"),
    "\n\nInstall them with:\n  install.packages(c(",
    paste0('"', missing$Package, '"', collapse = ", "), "))")
  if (!asreml_installed()) {
    message(
      "\nASReml-R was not found. The interface will open and data can be ",
      "inspected,\nbut no model can be fitted. ASReml-R is commercial software ",
      "licensed by VSNi\nand must be installed and licensed separately in this ",
      "R installation:\n  ", paste(.libPaths(), collapse = "\n  "))
  }
  invisible(NULL)
}
