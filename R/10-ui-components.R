# ---------------------------------------------------------------------------
# Reusable interface components
#
# Input wrappers degrade to plain Shiny controls when shinyWidgets or bsicons
# are not installed, so the application never fails to start because of an
# optional dependency.
# ---------------------------------------------------------------------------

#' Icon, or an empty span when bsicons is unavailable.
#' @noRd
ic <- function(name, ...) {
  if (has_pkg("bsicons")) bsicons::bs_icon(name, ...) else shiny::span()
}

#' Radio group.
#' @noRd
radio_input <- function(inputId, label, choices, selected = NULL, help = NULL) {
  ctl <- if (has_pkg("shinyWidgets")) {
    shinyWidgets::prettyRadioButtons(
      inputId, label, choices = choices, selected = selected,
      status = "success", icon = if (has_pkg("bsicons")) NULL else NULL,
      animation = "smooth", bigger = FALSE, outline = TRUE)
  } else {
    shiny::radioButtons(inputId, label, choices = choices, selected = selected)
  }
  if (is.null(help)) ctl else shiny::tagList(ctl, shiny::div(class = "section-note", help))
}

#' On/off switch.
#' @noRd
switch_input <- function(inputId, label, value = TRUE, help = NULL) {
  ctl <- if (has_pkg("shinyWidgets")) {
    shinyWidgets::materialSwitch(inputId, label, value = value,
                                 status = "success", right = TRUE)
  } else {
    shiny::checkboxInput(inputId, label, value = value)
  }
  if (is.null(help)) ctl else shiny::tagList(ctl, shiny::div(class = "section-note", help))
}

#' Select control with search when the list is long.
#' @noRd
select_input <- function(inputId, label, choices, selected = NULL, ...) {
  if (has_pkg("shinyWidgets") && length(choices) > 8) {
    shinyWidgets::pickerInput(inputId, label, choices = choices, selected = selected,
                              options = list(`live-search` = TRUE, size = 12),
                              width = "100%", ...)
  } else {
    shiny::selectInput(inputId, label, choices = choices, selected = selected,
                       width = "100%", ...)
  }
}

#' Coloured status banner.
#'
#' @param level "ok", "warn" or "bad"
#' @noRd
status_banner <- function(level, title, ..., icon_name = NULL) {
  icon_name <- icon_name %||% switch(level, ok = "check-circle-fill",
                                     warn = "exclamation-triangle-fill",
                                     bad = "x-octagon-fill")
  shiny::div(
    class = paste0("status status-", level),
    shiny::strong(ic(icon_name), " ", title),
    if (length(list(...))) shiny::div(style = "margin-top:.35rem;", ...)
  )
}

#' One metric in the summary strip.
#' @noRd
metric <- function(label, value, sub = NULL) {
  shiny::div(
    class = "metric",
    shiny::div(class = "metric-label", label),
    shiny::div(class = "metric-value", value),
    if (!is.null(sub)) shiny::div(class = "metric-sub", sub)
  )
}

#' Horizontal strip of metrics.
#' @noRd
metric_row <- function(...) {
  shiny::div(class = "metric-row", ...)
}

#' Small explanatory paragraph.
#' @noRd
note <- function(...) shiny::div(class = "section-note", ...)

#' Quiet landing panel for a workspace with nothing in it yet.
#'
#' An empty results card is an unhelpful first impression. This puts the next
#' action in the middle of the space instead, so the workflow is legible before
#' any data exists.
#'
#' @param title one line saying what is missing
#' @param body one short paragraph
#' @param steps optional character vector rendered as a numbered list
#' @param icon_name bsicon name
#' @noRd
empty_state <- function(title, body, steps = NULL, icon_name = "cloud-arrow-up") {
  shiny::div(
    class = "empty-state",
    shiny::div(class = "empty-icon", ic(icon_name)),
    shiny::div(class = "empty-title", title),
    shiny::div(class = "empty-body", body),
    if (length(steps)) {
      shiny::tags$ol(lapply(steps, shiny::tags$li))
    }
  )
}

#' Numbered step marker for the workflow headings.
#' @noRd
step <- function(n, text) {
  shiny::tagList(shiny::span(class = "step-badge", n), text)
}

#' DT table with settings suited to model output.
#'
#' Numeric columns are rounded for display only; the exported CSV and Excel
#' files always carry full precision.
#' @noRd
dt_table <- function(df, digits = 4, page_length = 15, scroll_y = NULL,
                     highlight = NULL) {
  if (is.null(df) || !nrow(df)) {
    return(DT::datatable(data.frame(Message = "No rows to display."),
                         rownames = FALSE, options = list(dom = "t")))
  }
  numeric_cols <- names(df)[vapply(df, is.numeric, logical(1))]
  # Integer-valued columns (counts, ranks) are shown without decimals.
  integer_cols <- numeric_cols[vapply(df[numeric_cols], function(x) {
    all(is.na(x) | abs(x - round(x)) < 1e-9)
  }, logical(1))]
  decimal_cols <- setdiff(numeric_cols, integer_cols)

  # Underscores are an artefact of R naming, not something a user should read.
  display <- df
  names(display) <- gsub("_", " ", names(display))

  # DataTables renders an empty table when pageLength is absent from
  # lengthMenu, so the menu is always built to contain it.
  lengths <- sort(unique(c(page_length, 10, 25, 50)))
  length_labels <- c(as.character(lengths), "All")
  lengths <- c(lengths, -1)

  # Column filters build a range slider for every numeric column. A column with
  # a single distinct value gives a slider whose minimum equals its maximum,
  # which throws in noUiSlider and aborts the draw, leaving a visible "Show N
  # entries" control above an empty table. Summary tables routinely contain
  # such columns, so filters are dropped whenever one is present.
  degenerate <- vapply(df[numeric_cols], function(x) {
    r <- suppressWarnings(range(x, na.rm = TRUE))
    !all(is.finite(r)) || isTRUE(r[1] == r[2])
  }, logical(1))
  # A filter row above a short reference table is pure chrome, and on a column
  # with one distinct value the range slider throws and aborts the draw.
  filter_mode <- if (nrow(df) < 12L || any(degenerate)) "none" else "top"

  # Numbers are compared column-wise and so are right-aligned; text is read
  # left to right and so is not. Right-aligning a label column leaves it
  # floating against the gutter and makes wrapped cells read as ragged blocks.
  right <- which(names(df) %in% numeric_cols) - 1L
  left  <- setdiff(seq_along(df), which(names(df) %in% numeric_cols)) - 1L
  col_defs <- list()
  if (length(right)) col_defs <- c(col_defs, list(list(className = "dt-right", targets = right)))
  if (length(left))  col_defs <- c(col_defs, list(list(className = "dt-left",  targets = left)))

  # A table that fits on one page needs neither a page-length menu nor paging
  # controls; one that does not, needs all of them.
  dom <- if (nrow(df) <= page_length) {
    if (nrow(df) <= 8L) "t" else "ft"
  } else "lftip"

  tab <- DT::datatable(
    display, rownames = FALSE, filter = filter_mode, class = "compact stripe hover",
    options = list(
      pageLength = page_length, scrollX = TRUE, scrollY = scroll_y,
      lengthMenu = list(lengths, length_labels),
      dom = dom, autoWidth = FALSE,
      columnDefs = col_defs
    )
  )
  if (length(decimal_cols)) {
    tab <- DT::formatRound(tab, gsub("_", " ", decimal_cols), digits)
  }
  if (!is.null(highlight) && highlight %in% names(df)) {
    tab <- DT::formatStyle(
      tab, gsub("_", " ", highlight),
      background = DT::styleColorBar(range(df[[highlight]], na.rm = TRUE),
                                     PAL$primary_lt),
      backgroundSize = "98% 82%", backgroundRepeat = "no-repeat",
      backgroundPosition = "center")
  }
  tab
}

#' Card wrapper with a consistent header.
#' @noRd
panel_card <- function(title, ..., subtitle = NULL, icon_name = NULL,
                       full_screen = TRUE, fill = FALSE) {
  # `fill = FALSE` is deliberate. The application runs inside a fillable
  # page_navbar, where a card that is a direct flex child is stretched or
  # squashed by the flex layout: a short table card collapses to zero height
  # and its contents disappear even though they are present in the DOM. Cards
  # that genuinely should grow (plots) pass fill = TRUE explicitly.
  bslib::card(
    full_screen = full_screen,
    fill = fill,
    bslib::card_header(
      if (!is.null(icon_name)) shiny::tagList(ic(icon_name), " "),
      shiny::span(class = "card-title-text", title),
      if (!is.null(subtitle)) shiny::span(class = "card-subtitle-text", subtitle)
    ),
    bslib::card_body(..., fill = fill)
  )
}

#' Licence notice shown in every workspace and in the About tab.
#'
#' The wording is deliberately explicit: users must understand that they supply
#' the licence, that it stays on their machine, and that the application has no
#' part in obtaining or managing it.
#' @noRd
licence_notice <- function() {
  s <- asreml_status()
  shiny::tagList(
    status_banner(if (s$ok) "ok" else "bad", s$title, s$detail),
    note(
      shiny::strong("Licensing. "),
      "ASReml-R is commercial software licensed by VSNi and must already be ",
      "installed and activated in the R installation running this application. ",
      "This application does not supply, embed, store, transmit or manage an ",
      "ASReml licence, and it will not run the analysis without one. Do not ",
      "commit a licence file or activation key to a shared or public repository."
    )
  )
}

#' Convert a data frame of file-reading options into the shared upload panel.
#' @noRd
upload_panel <- function(ns, multi_env = FALSE) {
  shiny::tagList(
    shiny::fileInput(ns("file"), "Trial data file (CSV or text)",
                     accept = c(".csv", ".txt", ".tsv", "text/csv"),
                     width = "100%"),
    shiny::fluidRow(
      shiny::column(6, shiny::selectInput(
        ns("separator"), "Separator",
        c("Comma" = ",", "Semicolon" = ";", "Tab" = "\t", "Space" = " "), ",")),
      # Aligned to the baseline of the neighbouring select by a class rather
      # than a magic margin, so it stays put when the type scale changes.
      shiny::column(6, shiny::div(
        class = "input-align-bottom",
        shiny::checkboxInput(ns("header"), "First row is a header", TRUE)))
    ),
    shiny::downloadButton(
      ns("sample"),
      if (multi_env) "Download worked MET example" else "Download worked example",
      class = "btn-outline-primary btn-sm w-100"),
    note(
      "The example is simulated from known parameters, so you can confirm the ",
      "model recovers them before trusting it on your own data."
    )
  )
}
