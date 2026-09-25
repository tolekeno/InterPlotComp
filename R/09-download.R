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

#' Base text size for a figure of a given width.
#'
#' Type scales with the canvas, so a poster and a single-column figure both
#' come out proportionate rather than the screen sizes stretched or squashed.
#' Two things set the numbers.
#'
#' The reference is 14 pt at the 18 cm double-column width. With the secondary
#' ratios in `theme_trial()` that puts axis tick labels near 12 pt and the
#' caption near 11 pt - readable when the figure is placed at full size, and
#' still readable after the shrink a figure usually takes on its way into a
#' manuscript or a slide.
#'
#' The growth is sub-linear - the square root of the width ratio - because a
#' figure is *printed* at the width it is exported at. Type on a 9 cm
#' single-column figure has to hold an absolute size on the page, so scaling it
#' in proportion to the canvas would leave it unreadable; scaling a 25 cm
#' poster in proportion would leave it absurd. The square root keeps every
#' preset inside a 10-17 pt band while still growing with the canvas, and the
#' clamp catches anything outside the presets.
#'
#' @param width_cm finished figure width in centimetres
#' @noRd
figure_base_size <- function(width_cm) {
  max(10, min(22, 14 * sqrt(width_cm / 18)))
}

# Screen resolution assumed for the preview device. The preview is then just
# an export at whatever width the browser has given it, so its proportions
# match the downloaded file rather than being a separately tuned picture.
PREVIEW_RES       <- 110
PREVIEW_BASE_SIZE <- 14    # fallback before the browser reports a width

#' Save a ggplot to file at a chosen size and resolution.
#'
#' @seealso `figure_base_size()` for how the text size is chosen.
#'
#' @param plot_fun A function of one argument, `base_size`, returning a
#'   [ggplot2::ggplot()]. Taking a function rather than a finished plot is what
#'   lets the text size follow the export width, so a figure saved at 9 cm and
#'   the same figure at 18 cm both come out legible.
#' @param file Destination path. The format is taken from `format`, not from
#'   the file extension.
#' @param format One of `"png"`, `"tiff"`, `"pdf"`, `"svg"` or `"eps"`.
#' @param width,height Size in `units`.
#' @param units `"cm"` or `"in"`.
#' @param dpi Resolution for the raster formats; ignored for vector ones.
#' @return The path, invisibly. Called for its side effect of writing the file.
#' @export
#' @examples
#' f <- tempfile(fileext = ".png")
#' save_figure(
#'   function(base_size) {
#'     plot_direct_vs_competition(
#'       data.frame(Genotype = c("A", "B", "C"),
#'                  Direct_effect = c(0.5, 0, -0.5),
#'                  Competition_effect = c(-0.2, 0.1, 0.2),
#'                  Pure_stand_effect = c(0.1, 0.2, -0.1)),
#'       base_size = base_size)
#'   },
#'   file = f, format = "png", width = 12, height = 9, dpi = 150
#' )
#' file.exists(f)
#' unlink(f)
save_figure <- function(plot_fun, file, format = "png", width = 18, height = 12,
                        units = "cm", dpi = 600) {
  width_cm <- if (units == "in") width * 2.54 else width
  p <- plot_fun(figure_base_size(width_cm))
  if (!inherits(p, c("ggplot", "patchwork"))) {
    stop("Only ggplot figures can be exported.", call. = FALSE)
  }

  args <- list(filename = file, plot = p, width = width, height = height,
               units = units, dpi = dpi, bg = "white")
  # Vector devices take no dpi; passing it warns on some devices.
  if (format %in% c("pdf", "svg", "eps")) args$dpi <- NULL

  candidates <- figure_devices(format)
  failures <- character(0)

  for (nm in names(candidates)) {
    args$device <- candidates[[nm]]
    if (file.exists(file)) unlink(file)

    err <- tryCatch({
      suppressWarnings(do.call(ggplot2::ggsave, args))
      NULL
    }, error = function(e) conditionMessage(e))

    # A device can fail without raising: `cairo_pdf()` on a macOS build whose
    # X11 libraries are absent reports capabilities("cairo") as TRUE, then
    # quietly writes nothing. Trusting ggsave's return value would let an empty
    # or missing file through as a successful export.
    if (is.null(err) && file.exists(file) && file.size(file) > 0) {
      return(invisible(file))
    }
    failures <- c(failures, sprintf(
      "%s: %s", nm, err %||% "produced no output"))
  }

  if (file.exists(file)) unlink(file)
  stop("Could not write a ", format, " figure. Devices tried:\n  ",
       paste(failures, collapse = "\n  "), call. = FALSE)
}

#' Candidate graphics devices for one export format, best first.
#'
#' Returned as an ordered, named list so [save_figure()] can fall back when a
#' device is unusable. Availability cannot be settled by asking: on macOS
#' `capabilities("cairo")` reports what R was compiled with, while `cairo_pdf()`
#' additionally needs the X11 libraries at run time and fails silently without
#' them. The only reliable test is to draw and then look for the file.
#'
#' @param format one of `EXPORT_FORMATS`
#' @noRd
figure_devices <- function(format) {
  # `ggsave()` inspects a device function's formals to decide whether to pass
  # `res` and `units`. A `...`-only wrapper therefore silently loses both and
  # produces a zero-dimension canvas, so the formals are spelled out.
  tiff_via <- function(fun, ...) {
    extra <- list(...)
    function(filename, width, height, units = "in", res = 300, ...) {
      do.call(fun, c(list(filename = filename, width = width, height = height,
                          units = units, res = res), extra, list(...)))
    }
  }

  switch(
    format,
    png = c(
      if (has_pkg("ragg")) list(`ragg::agg_png` = ragg::agg_png),
      list(`grDevices::png` = grDevices::png)
    ),
    tiff = c(
      if (has_pkg("ragg")) {
        list(`ragg::agg_tiff` = tiff_via(ragg::agg_tiff, compression = "lzw"))
      },
      list(`grDevices::tiff (cairo)` =
             tiff_via(grDevices::tiff, compression = "lzw", type = "cairo"),
           `grDevices::tiff` = tiff_via(grDevices::tiff, compression = "lzw"))
    ),
    pdf = c(
      if (capabilities("cairo")) list(`grDevices::cairo_pdf` = grDevices::cairo_pdf),
      list(`grDevices::pdf` = grDevices::pdf)
    ),
    svg = c(
      if (has_pkg("svglite")) list(`svglite::svglite` = svglite::svglite),
      # Always tried last, even without cairo, so a failure names a device
      # and carries its own error rather than an empty list.
      list(`grDevices::svg` = grDevices::svg)
    ),
    eps = c(
      if (capabilities("cairo")) list(`grDevices::cairo_ps` = grDevices::cairo_ps),
      list(`grDevices::postscript` = function(filename, ...) {
        grDevices::postscript(file = filename, ..., onefile = FALSE,
                              horizontal = FALSE, paper = "special")
      })
    ),
    stop("Unsupported export format: ", format, call. = FALSE)
  )
}

#' Export several figures to one multi-page PDF.
#' @noRd
save_figure_pdf_report <- function(plot_funs, file, width = 18, height = 14,
                                   units = "cm") {
  to_inches <- function(x) if (units == "cm") x / 2.54 else x
  base_size <- figure_base_size(if (units == "in") width * 2.54 else width)
  plots <- lapply(plot_funs, function(f) try(f(base_size), silent = TRUE))
  plots <- Filter(function(p) !inherits(p, "try-error"), plots)

  # Same fallback-and-verify logic as save_figure(): cairo_pdf() can fail
  # silently on macOS, so success is judged by the file actually written.
  candidates <- figure_devices("pdf")
  failures <- character(0)
  for (nm in names(candidates)) {
    if (file.exists(file)) unlink(file)
    err <- tryCatch({
      candidates[[nm]](file, width = to_inches(width),
                       height = to_inches(height), onefile = TRUE)
      tryCatch(for (p in plots) print(p),
               finally = grDevices::dev.off())
      NULL
    }, error = function(e) conditionMessage(e))

    if (is.null(err) && file.exists(file) && file.size(file) > 0) {
      return(invisible(file))
    }
    failures <- c(failures, sprintf(
      "%s: %s", nm, err %||% "produced no output"))
  }

  if (file.exists(file)) unlink(file)
  stop("Could not write the PDF report. Devices tried:
  ",
       paste(failures, collapse = "
  "), call. = FALSE)
}

# ---------------------------------------------------------------------------
# Shiny module: one export control per figure
# ---------------------------------------------------------------------------

#' UI for a figure with an attached export control.
#'
#' @param id module id
#' @param height CSS height of the plot area
#' @param interactive offer a plotly version when the package is available
#' @noRd
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
  # The figure sits on its own light "paper" ground in both colour modes, so
  # that what is on screen is what lands in the 600 dpi export.
  shiny::tagList(control, shiny::div(class = "fig-paper", plot_area))
}

#' Server for `figure_ui()`.
#'
#' @param id module id
#' @param plot_fun reactive-safe function(base_size) returning a ggplot
#' @param filename_base base name for the downloaded file
#' @param height CSS height of the plot area
#' @param interactive allow the plotly toggle
#' @noRd
figure_server <- function(id, plot_fun, filename_base = "figure",
                          height = "460px", interactive = FALSE) {
  shiny::moduleServer(id, function(input, output, session) {
    use_plotly <- interactive && has_pkg("plotly")

    build <- function(base_size = PREVIEW_BASE_SIZE) plot_fun(base_size)

    # The browser reports the plot area's pixel width, which at the preview
    # device resolution is a physical width in centimetres. Feeding that to the
    # same rule the export uses makes the preview an export at its own width:
    # type is in the same proportion to the canvas on screen as in the file.
    # Capped at the default 18 cm export size. A browser window is wider and
    # much shorter than any figure anyone exports, and letting a 24 cm-wide
    # preview take 24 cm-wide type would spend the whole card height on the
    # title, legend and caption and leave the panel a sliver.
    preview_size <- function(output_id) {
      px <- session$clientData[[paste0("output_", session$ns(output_id), "_width")]]
      if (is.null(px) || !is.finite(px) || px <= 0) return(PREVIEW_BASE_SIZE)
      min(figure_base_size(px / PREVIEW_RES * 2.54), PREVIEW_BASE_SIZE)
    }

    output$static <- shiny::renderPlot({ build(preview_size("static")) },
                                       res = PREVIEW_RES)

    if (use_plotly) {
      output$area <- shiny::renderUI({
        if (isTRUE(input$interactive)) {
          plotly::plotlyOutput(session$ns("dynamic"), height = height)
        } else {
          shiny::plotOutput(session$ns("static2"), height = height)
        }
      })
      output$static2 <- shiny::renderPlot({ build(preview_size("static2")) },
                                          res = PREVIEW_RES)
      output$dynamic <- plotly::renderPlotly({
        # Titles are dropped from the interactive view: plotly renders ggplot
        # subtitles and captions poorly, and they are already on screen.
        p <- build(preview_size("dynamic")) +
          ggplot2::labs(title = NULL, subtitle = NULL, caption = NULL)
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
#' @noRd
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
#' @noRd
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
#' @noRd
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
