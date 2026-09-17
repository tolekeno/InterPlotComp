# ---------------------------------------------------------------------------
# Launcher
#
# Run this file from R or RStudio:   source("run_app.R")
#
# It must be run in the R installation where ASReml-R is installed and
# licensed. On a machine with several R versions, the wrong one is the usual
# reason the application reports that ASReml-R is missing.
# ---------------------------------------------------------------------------

app_dir <- tryCatch(dirname(normalizePath(sys.frame(1)$ofile)),
                    error = function(e) getwd())
if (!file.exists(file.path(app_dir, "app.R"))) app_dir <- getwd()

if (!requireNamespace("shiny", quietly = TRUE)) {
  stop("Install Shiny first:  install.packages('shiny')", call. = FALSE)
}

source(file.path(app_dir, "R", "00-packages.R"))

missing_required <- REQUIRED_PACKAGES[!vapply(REQUIRED_PACKAGES, has_pkg, logical(1))]
missing_optional <- names(OPTIONAL_PACKAGES)[
  !vapply(names(OPTIONAL_PACKAGES), has_pkg, logical(1))]

if (length(missing_required)) {
  message("Installing required packages: ", paste(missing_required, collapse = ", "))
  utils::install.packages(missing_required)
}
if (length(missing_optional)) {
  message("\nOptional packages not installed (each adds one feature):\n  ",
          paste(missing_optional, collapse = ", "),
          "\n\nInstall them with:\n  install.packages(c(",
          paste0('"', missing_optional, '"', collapse = ", "), "))\n")
}

if (!requireNamespace("asreml", quietly = TRUE)) {
  message(
    "\nNOTE: ASReml-R was not found in this R library.\n",
    "The interface will open and data can be inspected, but no model can be\n",
    "fitted. ASReml-R is commercial software licensed by VSNi and must be\n",
    "installed and activated separately in this R installation:\n  ",
    paste(.libPaths(), collapse = "\n  "), "\n",
    "This application does not supply or manage an ASReml licence.\n")
}

shiny::runApp(app_dir, launch.browser = TRUE)
