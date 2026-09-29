# ---------------------------------------------------------------------------
# Interface pieces shared by both workspaces: outlier screening, the global
# field-trend adjustment, the fixed-effects card and the ASReml script.
#
# Each workspace calls the same builders, so a control or a results card looks
# and behaves identically whichever workspace the user is in.
# ---------------------------------------------------------------------------

#' Sidebar controls for the outlier screen.
#' @noRd
outlier_controls <- function(ns) {
  shiny::tagList(
    radio_input(ns("outliers"), "Phenotypic outliers", OUTLIER_MODES, "none"),
    shiny::conditionalPanel(
      sprintf("input['%s'] != 'none'", ns("outliers")),
      shiny::numericInput(ns("outlier_threshold"),
                          "Flag when |standardised residual| exceeds",
                          DEFAULT_OUTLIER_THRESHOLD, min = 2, max = 10,
                          step = 0.5, width = "100%")
    ),
    note("The selected model is fitted and every observation's standardised ",
         "conditional residual is computed; those beyond the threshold (4 by ",
         "default) are listed under ", shiny::em("Diagnostics \u203a Outliers"), ". ",
         shiny::strong("Detect, remove, then refit"),
         " sets their response to missing - the plot stays in the field, so ",
         "it still competes with its neighbours - and fits the model again. ",
         "Every removed record is kept in the report, the workbook and the ",
         "ASReml script.")
  )
}

#' Explanatory note beneath the field-trend controls.
#' @noRd
field_trend_note <- function(multi_env = TRUE) {
  note("A separable spatial process models local, patchy variation. A smooth ",
       "gradient running the length of a field is not stationary; left to the ",
       "spatial process it inflates the autocorrelation and can leak into the ",
       "competitive effects. ", shiny::code("lrow"), " and ", shiny::code("lcol"),
       " remove it first as fixed linear slopes along rows and columns ",
       "(Gilmour, Cullis & Verbyla 1997).",
       if (multi_env) {
         shiny::tagList(" Each selected site gets its own slope, written ",
                        shiny::code("at(Env, site):lrow"), ", because fields ",
                        "do not share a gradient.")
       },
       " Check the Wald test after fitting: a slope that is not significant ",
       "can be dropped.")
}

#' The Outliers diagnostics panel.
#' @noRd
outlier_panel_ui <- function(ns) {
  bslib::nav_panel(
    "Outliers",
    shiny::uiOutput(ns("outlier_status")),
    shiny::conditionalPanel(
      condition = "output.has_outliers === true", ns = ns,
      panel_card("Outliers by site",
                 DT::DTOutput(ns("outlier_summary")),
                 icon_name = "geo-alt", full_screen = FALSE),
      panel_card("Flagged observations",
                 table_download_ui(ns("dl_outliers"), "Download flagged observations"),
                 DT::DTOutput(ns("outlier_table")),
                 note("Field row and column are the labels from your file; the ",
                      "index columns give the position on the analysis grid. ",
                      shiny::strong("Action"), " records what was done with each ",
                      "record, so the analysis can be reproduced exactly."),
                 icon_name = "exclamation-diamond", full_screen = FALSE)
    )
  )
}

#' Server side of the Outliers panel.
#'
#' @param res function returning the fitted result (validated)
#' @param safe_result reactive holding list(ok, value)
#' @param relabel optional function(table) applied before display, used by the
#'   single-trial workspace to show the analysed site's name
#' @noRd
register_outlier_outputs <- function(output, res, safe_result, file_base,
                                     relabel = identity) {
  outlier_result <- function() {
    z <- safe_result()
    if (!isTRUE(z$ok)) return(NULL)
    o <- z$value$outliers
    if (is.null(o)) return(NULL)
    o$table <- relabel(o$table)
    o$summary <- relabel(o$summary)
    o
  }

  output$has_outliers <- shiny::reactive({
    o <- outlier_result()
    !is.null(o) && nrow(o$table) > 0
  })
  shiny::outputOptions(output, "has_outliers", suspendWhenHidden = FALSE)

  output$outlier_status <- shiny::renderUI({
    r <- res()
    o <- outlier_result()
    if (is.null(o)) {
      return(status_banner("warn", "Outlier screening was off for this fit",
                           "Choose ", shiny::em("Detect and report only"), " or ",
                           shiny::em("Detect, remove, then refit"),
                           " under Outlier screening in the sidebar, then refit.",
                           icon_name = "info-circle"))
    }
    removed <- o$n_removed > 0
    status_banner(
      if (o$n_detected == 0) "ok" else "warn",
      if (o$n_detected == 0) {
        sprintf("No observation exceeds |standardised residual| > %g", o$threshold)
      } else if (removed) {
        sprintf("%d outlier(s) removed and the model refitted", o$n_removed)
      } else {
        sprintf("%d outlier(s) flagged and retained in the fit", o$n_detected)
      },
      shiny::div(sprintf("Screened on %s.", o$method)),
      if (removed && o$n_remaining > 0) {
        shiny::div(sprintf(paste(
          "%d further observation(s) exceed the threshold in the refitted model.",
          "They are listed but were not removed: one pass is made, because",
          "removing points until none remain cuts into the genuine tails of",
          "the distribution."), o$n_remaining))
      },
      if (removed) {
        shiny::div("Every result in this workspace comes from the refitted ",
                   "model; the removed records are listed below and written ",
                   "to the workbook and the ASReml script.")
      },
      if (!removed && o$n_detected > 0) {
        shiny::div("Check each record against the field book before removing ",
                   "it. A genuine data error should be corrected at source; a ",
                   "real but extreme plot may belong in the analysis.")
      })
  })

  output$outlier_summary <- DT::renderDT({
    o <- outlier_result(); shiny::req(o)
    dt_table(o$summary, digits = 2, page_length = 10)
  })
  output$outlier_table <- DT::renderDT({
    o <- outlier_result(); shiny::req(o)
    dt_table(o$table, digits = 3, page_length = 10)
  })
  table_download_server("dl_outliers", function() outlier_result()$table, file_base)
  invisible(outlier_result)
}

#' Fixed-effects card: covariate slopes and field-trend slopes with Wald tests.
#'
#' Shown whenever the fixed model holds more than the intercept and the
#' environment means, so the user can see whether each term earns its place.
#' @noRd
fixed_effects_card <- function(ns, r, multi_env = FALSE) {
  has_covariate <- length(r$covariate_terms) > 0
  has_trend <- length(r$field_trend_terms) > 0
  if (!has_covariate && !has_trend) return(NULL)
  panel_card(
    if (has_covariate && has_trend) "Covariate and field trend"
    else if (has_covariate) "Covariate" else "Global field trend",
    shiny::h6("Wald test"),
    DT::DTOutput(ns("wald")),
    note(shiny::strong("Wald test. "),
         "A conditional F-test of each fixed term, adjusted for the terms ",
         "above it. ", shiny::strong("Retain = Yes"),
         " means the term is significant at p < 0.05 and is earning its ",
         "place; ", shiny::strong("No"),
         " means it is not explaining variation in yield and can be dropped. ",
         "The denominator degrees of freedom are computed rather than ",
         "assumed infinite, so the test is not anti-conservative on a ",
         "trial-sized dataset."),
    shiny::h6("Estimated slopes"),
    DT::DTOutput(ns("fixed_effects")),
    if (has_trend) {
      note(shiny::strong("Field-trend slopes "),
           "are the change in yield per row (lrow) or per column (lcol), ",
           if (multi_env) "estimated separately for each selected site. " else "",
           "Row and column positions are centred on the middle of the field, so ",
           "the fitted mean still refers to the centre of the trial.")
    },
    if (has_covariate) {
      note("The neighbour slope is the change in a plot's yield per unit of ",
           "the covariate summed over its neighbours",
           if (multi_env) ", fitted in common across environments" else "",
           ". A negative slope means larger neighbours suppress the focal plot.")
    },
    note(shiny::strong("Do not compare log-likelihood, AIC or BIC "),
         "between runs with different fixed terms - a covariate or a field ",
         "trend added or removed. REML likelihoods are only comparable when ",
         "the fixed effects are identical; use the Wald test instead. The ",
         "likelihood-ratio test reported elsewhere is unaffected: it compares ",
         "two models that share whatever fixed effects are in force."),
    icon_name = "rulers", full_screen = FALSE)
}

#' Rows for the "What was fitted" definition list.
#' @noRd
model_detail_extras <- function(r, multi_env = FALSE) {
  trend <- r$field_trend
  o <- r$outliers
  shiny::tagList(
    shiny::tags$dt("Global field trend"),
    shiny::tags$dd(if (is.null(trend) || !nrow(trend)) {
      "None: field trend is left to the spatial residual."
    } else {
      shiny::tagList(
        paste0(sentence_case(describe_field_trend(trend, multi_env)), ". "),
        "Fixed terms: ", shiny::code(paste(r$field_trend_terms, collapse = " + ")))
    }),
    shiny::tags$dt("Outlier screening"),
    shiny::tags$dd(if (is.null(o)) {
      "Off."
    } else if (o$n_removed > 0) {
      sprintf(paste("%d observation(s) with |standardised residual| > %g were",
                    "removed and the model refitted; %d further observation(s)",
                    "exceed the threshold in the refit. Screened on %s."),
              o$n_removed, o$threshold, o$n_remaining, o$method)
    } else {
      sprintf("%d observation(s) with |standardised residual| > %g flagged and retained. Screened on %s.",
              o$n_detected, o$threshold, o$method)
    })
  )
}

#' Upper-case the first letter.
#' @noRd
sentence_case <- function(x) paste0(toupper(substring(x, 1, 1)), substring(x, 2))

#' Card holding the ASReml script for the fitted model.
#' @noRd
asreml_code_ui <- function(ns) {
  panel_card(
    "ASReml-R script",
    shiny::div(class = "fig-toolbar",
               shiny::downloadButton(ns("dl_script"), "Download script (.R)",
                                     class = "btn-sm btn-outline-primary")),
    shiny::verbatimTextOutput(ns("script")),
    note("Rebuilds the analysis data with this package's own functions, applies ",
         "the same field-trend covariates and outlier removal, and calls ",
         shiny::code("asreml()"), " with the formulae that were actually fitted ",
         "- the model the simplification ladder settled on. Edit the file path ",
         "if the data are stored elsewhere."),
    icon_name = "file-earmark-code", full_screen = TRUE)
}

#' Server side of the ASReml script card.
#'
#' @param script_fun function() returning the script lines
#' @noRd
register_asreml_code <- function(output, script_fun, file_base) {
  output$script <- shiny::renderText(paste(script_fun(), collapse = "\n"))
  output$dl_script <- shiny::downloadHandler(
    filename = function() stamped(file_base, "R"),
    content = function(file) writeLines(script_fun(), file)
  )
}
