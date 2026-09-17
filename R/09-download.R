# ---------------------------------------------------------------------------
# Figure and table export
#
# Every figure carries its own export control, so the user chooses format,
# canvas size and resolution per figure rather than accepting one global
# default. Defaults are publication settings: 600 dpi, a single-column or
# double-column width in centimetres, and lossless compression.
#
# Devices are chosen for quality: ragg for PNG and TIFF (better text rendering
# and true LZW-compressed TIFF), cairo_pdf for PDF (proper font embedding and
# transparency), svglite for SVG. Each falls back to the grDevices equivalent
# when the package is absent, so export always works.
# ---------------------------------------------------------------------------

EXPORT_FORMATS <- c(
  "PNG (raster, presentations and web)" = "png",
  "PDF (vector, journal submission)"    = "pdf",
  "TIFF (raster, LZW, journal print)"   = "tiff",
  "SVG (vector, editable)"              = "svg",
  "EPS (vector, legacy typesetting)"    = "eps"
)

# Journal figure widths, in centimetres.
FIGURE_PRESETS <- c(
  "Single column (9 cm)"  = "9",
  "1.5 column (14 cm)"    = "14",
  "Double column (18 cm)" = "18",
  "Slide / poster (25 cm)" = "25"
)

#' Save a ggplot to file at a chosen size and resolution.
#'
#' Text is scaled with the canvas: `base_size` is derived from the requested
#' width so that a 25 cm poster figure and a 9 cm column figure both come out
#' with proportionate, legible type rather than the screen sizes stretched or
#' squashed.
#'
#' @param plot_fun function returning a ggplot, taking a `base_size` argument
#' @param file destination path
#' @param format one of EXPORT_FORMATS
#' @param width,height size in `units`
#' @param units "cm" or "in"
#' @param dpi resolution for raster formats
save_figure <- function(plot_fun, file, format = "png", width = 18, height = 12,
                        units = "cm", dpi = 600) {
  width_cm <- if (units == "in") width * 2.54 else width
  # 11 pt at 18 cm is a comfortable reference; scale linearly from there and
  # clamp so very small or very large canvases stay readable.
  base_size <- max(7, min(20, 11 * width_cm / 18))

  p <- plot_fun(base_size)
  if (!inherits(p, c("ggplot", "patchwork"))) {
    stop("Only ggplot figures can be exported.", call. = FALSE)
  }

  args <- list(filename = file, plot = p, width = width, height = height,
               units = units, dpi = dpi, bg = "white")

  device <- switch(
    format,
    png  = if (has_pkg("ragg")) ragg::agg_png else grDevices::png,
    # `ggsave()` inspects a device function's formals to decide whether to pass
    # `res` and `units`. A `...`-only wrapper therefore silently loses both and
    # produces a zero-dimension canvas, so the formals are spelled out.
    tiff = if (has_pkg("ragg")) {
      function(filename, width, height, units = "in", res = 300, ...) {
        ragg::agg_tiff(filename = filename, width = width, height = height,
                       units = units, res = res, compression = "lzw", ...)
      }
    } else {
      function(filename, width, height, units = "in", res = 300, ...) {
        grDevices::tiff(filename = filename, width = width, height = height,
                        units = units, res = res, compression = "lzw",
                        type = "cairo", ...)
      }
    },
    pdf  = if (capabilities("cairo")) grDevices::cairo_pdf else grDevices::pdf,
    svg  = if (has_pkg("svglite")) svglite::svglite else grDevices::svg,
    eps  = if (capabilities("cairo")) grDevices::cairo_ps else {
      function(filename, ...) grDevices::postscript(file = filename, ...,
                                                    onefile = FALSE,
                                                    horizontal = FALSE,
                                                    paper = "special")
    },
    stop("Unsupported export format: ", format, call. = FALSE)
  )
  args$device <- device
  # Vector devices take no dpi; passing it warns on some devices.
  if (format %in% c("pdf", "svg", "eps")) args$dpi <- NULL

  do.call(ggplot2::ggsave, args)
  invisible(file)
}

#' Export several figures to one multi-page PDF.
save_figure_pdf_report <- function(plot_funs, file, width = 18, height = 14,
                                   units = "cm") {
  to_inches <- function(x) if (units == "cm") x / 2.54 else x
  device <- if (capabilities("cairo")) grDevices::cairo_pdf else grDevices::pdf
  device(file, width = to_inches(width), height = to_inches(height),
         onefile = TRUE)
  on.exit(grDevices::dev.off(), add = TRUE)
  base_size <- max(7, min(20, 11 * (if (units == "in") width * 2.54 else width) / 18))
  for (f in plot_funs) {
    p <- try(f(base_size), silent = TRUE)
    if (!inherits(p, "try-error")) print(p)
  }
  invisible(file)
}

# ---------------------------------------------------------------------------
# Shiny module: one export control per figure
# ---------------------------------------------------------------------------

#' UI for a figure with an attached export control.
#'
#' @param id module id
#' @param height CSS height of the plot area
#' @param interactive offer a plotly version when the package is available
figure_ui <- function(id, height = "460px", interactive = FALSE) {
  ns <- shiny::NS(id)
  use_plotly <- interactive && has_pkg("plotly")

  control <- shiny::tagList(
    shiny::div(
      class = "fig-toolbar",
      if (use_plotly) {
        shiny::checkboxInput(ns("interactive"), "Interactive", value = FALSE,
                             width = "auto")
      },
      bslib::popover(
        shiny::actionButton(ns("open"), "Export figure",
                            icon = shiny::icon("download"),
                            class = "btn-sm btn-outline-primary"),
        title = "Publication-quality export",
        shiny::selectInput(ns("format"), "File format", EXPORT_FORMATS, "png"),
        shiny::selectInput(ns("preset"), "Canvas width", FIGURE_PRESETS, "18"),
        shiny::numericInput(ns("height"), "Height (cm)", 12, min = 4, max = 60,
                            step = 0.5),
        shiny::sliderInput(ns("dpi"), "Resolution (dpi)", min = 150, max = 1200,
                           value = 600, step = 50),
        shiny::helpText("PDF, SVG and EPS are vector formats; the resolution ",
                        "setting applies to PNG and TIFF only."),
        shiny::downloadButton(ns("download"), "Download",
                              class = "btn-sm btn-primary w-100")
      )
    )
  )

  plot_area <- if (use_plotly) {
    shiny::uiOutput(ns("area"))
  } else {
    shiny::plotOutput(ns("static"), height = height)
  }
  if (has_pkg("shinycssloaders")) {
    plot_area <- shinycssloaders::withSpinner(plot_area, color = PAL$primary,
                                              type = 8, size = 0.7)
  }
  shiny::tagList(control, plot_area)
}

#' Server for `figure_ui()`.
#'
#' @param id module id
#' @param plot_fun reactive-safe function(base_size) returning a ggplot
#' @param filename_base base name for the downloaded file
#' @param height CSS height of the plot area
#' @param interactive allow the plotly toggle
figure_server <- function(id, plot_fun, filename_base = "figure",
                          height = "460px", interactive = FALSE) {
  shiny::moduleServer(id, function(input, output, session) {
    use_plotly <- interactive && has_pkg("plotly")

    build <- function(base_size = 12) plot_fun(base_size)

    output$static <- shiny::renderPlot({ build(12) }, res = 110)

    if (use_plotly) {
      output$area <- shiny::renderUI({
        if (isTRUE(input$interactive)) {
          plotly::plotlyOutput(session$ns("dynamic"), height = height)
        } else {
          shiny::plotOutput(session$ns("static2"), height = height)
        }
      })
      output$static2 <- shiny::renderPlot({ build(12) }, res = 110)
      output$dynamic <- plotly::renderPlotly({
        # Titles are dropped from the interactive view: plotly renders ggplot
        # subtitles and captions poorly, and they are already on screen.
        p <- build(12) + ggplot2::labs(title = NULL, subtitle = NULL, caption = NULL)
        plotly::ggplotly(p) |> plotly::config(displaylogo = FALSE)
      })
    }

    output$download <- shiny::downloadHandler(
      filename = function() {
        stamped(filename_base, input$format %||% "png")
      },
      content = function(file) {
        save_figure(
          plot_fun = build, file = file,
          format = input$format %||% "png",
          width = as.numeric(input$preset %||% 18),
          height = as.numeric(input$height %||% 12),
          units = "cm",
          dpi = as.numeric(input$dpi %||% 600)
        )
      }
    )
  })
}

# ---------------------------------------------------------------------------
# Table export
# ---------------------------------------------------------------------------

#' Download control offering CSV and, where available, Excel.
table_download_ui <- function(id, label = "Download table") {
  ns <- shiny::NS(id)
  shiny::div(
    class = "fig-toolbar",
    shiny::downloadButton(ns("csv"), paste(label, "(CSV)"),
                          class = "btn-sm btn-outline-primary"),
    if (has_pkg("writexl")) {
      shiny::downloadButton(ns("xlsx"), "Excel",
                            class = "btn-sm btn-outline-primary")
    }
  )
}

#' Server for `table_download_ui()`.
#'
#' @param data_fun function returning the data frame to export
table_download_server <- function(id, data_fun, filename_base = "table") {
  shiny::moduleServer(id, function(input, output, session) {
    output$csv <- shiny::downloadHandler(
      filename = function() stamped(filename_base, "csv"),
      content = function(file) {
        utils::write.csv(data_fun(), file, row.names = FALSE, na = "")
      }
    )
    if (has_pkg("writexl")) {
      output$xlsx <- shiny::downloadHandler(
        filename = function() stamped(filename_base, "xlsx"),
        content = function(file) writexl::write_xlsx(data_fun(), file)
      )
    }
  })
}

#' Multi-sheet workbook of every result table from one analysis.
save_results_workbook <- function(tables, file) {
  tables <- tables[!vapply(tables, is.null, logical(1))]
  if (has_pkg("writexl")) {
    # Excel sheet names are limited to 31 characters.
    names(tables) <- substr(names(tables), 1, 31)
    writexl::write_xlsx(tables, file)
  } else {
    # Without writexl, concatenate into one annotated CSV rather than failing.
    con <- file(file, "w", encoding = "UTF-8")
    on.exit(close(con), add = TRUE)
    for (nm in names(tables)) {
      writeLines(paste0("# ", nm), con)
      utils::write.csv(tables[[nm]], con, row.names = FALSE, na = "")
      writeLines("", con)
    }
  }
  invisible(file)
}
