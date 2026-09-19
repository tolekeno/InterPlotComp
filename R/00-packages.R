# ---------------------------------------------------------------------------
# Package management
#
# The application separates hard requirements (it will not start without them)
# from optional enhancements (interactive plots, faster raster devices, Excel
# export). Every optional feature degrades to a working fallback, so the app
# remains usable on a minimal installation.
#
# ASReml-R is deliberately NOT in either list. It is loaded lazily, only when a
# model is actually fitted, so that the interface opens and can display
# installation and licensing guidance even where ASReml-R is absent. The
# application never reads, writes, embeds or transmits an ASReml licence.
# ---------------------------------------------------------------------------

REQUIRED_PACKAGES <- c(
  "shiny", "bslib", "htmltools", "ggplot2", "scales", "DT", "Matrix", "grDevices"
)

OPTIONAL_PACKAGES <- c(
  bsicons         = "icons in headers and value boxes",
  shinyWidgets    = "richer input controls",
  shinycssloaders = "busy indicators on long-running outputs",
  plotly          = "interactive versions of the main figures",
  thematic        = "automatic matching of plot styling to the app theme",
  ragg            = "high-quality PNG and TIFF export devices",
  svglite         = "SVG figure export",
  ggrepel         = "non-overlapping genotype labels",
  patchwork       = "multi-panel diagnostic figures",
  writexl         = "Excel workbook export",
  rmarkdown       = "self-contained HTML analysis report",
  knitr           = "report rendering"
)

#' Is an optional package usable in this session?
#'
#' Results are cached because `requireNamespace()` is called from reactive
#' contexts that may run many times per second.
#' @param pkg package name
#' @return TRUE/FALSE
#' @noRd
has_pkg <- local({
  cache <- new.env(parent = emptyenv())
  function(pkg) {
    if (is.null(cache[[pkg]])) {
      cache[[pkg]] <- isTRUE(requireNamespace(pkg, quietly = TRUE))
    }
    cache[[pkg]]
  }
})

#' Abort with an actionable message when hard requirements are missing.
#' @noRd
check_required_packages <- function() {
  missing <- REQUIRED_PACKAGES[!vapply(REQUIRED_PACKAGES, has_pkg, logical(1))]
  if (length(missing)) {
    stop(
      "The following required packages are not installed:\n  ",
      paste(missing, collapse = ", "),
      "\n\nInstall them with:\n  install.packages(c(",
      paste0('"', missing, '"', collapse = ", "), "))",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' Report which optional packages are absent, for display in the About tab.
#' @noRd
optional_package_status <- function() {
  data.frame(
    Package   = names(OPTIONAL_PACKAGES),
    Enables   = unname(OPTIONAL_PACKAGES),
    Installed = vapply(names(OPTIONAL_PACKAGES), has_pkg, logical(1)),
    row.names = NULL,
    stringsAsFactors = FALSE
  )
}
