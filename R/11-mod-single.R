# ---------------------------------------------------------------------------
# Single-trial workspace (Shiny module)
#
# Layout: a persistent sidebar carrying the whole workflow - upload, column
# mapping, competition structure, model options and the Run button - with the
# results in a tabbed card area beside it. The previous version spread these
# across four top-level tabs, which forced users to navigate away from their
# results to change one option and refit.
# ---------------------------------------------------------------------------

single_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 360, open = "open", title = "Analysis set-up",

      bslib::accordion(
        open = c("data", "mapping", "competition"),
        multiple = TRUE,

        bslib::accordion_panel(
          "1. Trial data", value = "data", icon = ic("file-earmark-arrow-up"),
          upload_panel(ns)
        ),

        bslib::accordion_panel(
          "2. Column mapping", value = "mapping", icon = ic("diagram-3"),
          shiny::uiOutput(ns("mapping"))
        ),

        bslib::accordion_panel(
          "3. Competition structure", value = "competition", icon = ic("grid-3x3"),
          radio_input(
            ns("axis"), "Which plots compete?",
            choices = stats::setNames(names(NEIGHBOUR_OFFSETS),
                                      unname(NEIGHBOUR_LABELS[names(NEIGHBOUR_OFFSETS)])),
            selected = "rows",
            help = paste("Single-row plots normally compete along the row",
                         "direction: each plot borders the plots at Row - 1 and",
                         "Row + 1 in the same column. Check this against your",
                         "field plan, because the wrong direction gives",
                         "meaningless competitive effects.")),
          select_input(
            ns("structure"), "Direct-competition covariance",
            SINGLE_STRUCTURES, "us"),
          note("us(2) estimates the direct variance, the competitive variance ",
               "and their covariance. It is the recommended structure.")
        ),

        bslib::accordion_panel(
          "4. Genetic relationship", value = "relationship",
          icon = ic("diagram-3"),
          relationship_ui(ns("rel"))
        ),

        bslib::accordion_panel(
          "5. Spatial and error model", value = "spatial", icon = ic("layers"),
          switch_input(ns("spatial"), "Separable spatial residual", TRUE,
                       help = paste("Models field trend as one correlation",
                                    "process along rows and another along",
                                    "columns. Missing grid positions are added",
                                    "automatically with a missing response.")),
          shiny::conditionalPanel(
            sprintf("input['%s'] == true", ns("spatial")),
            select_input(ns("row_process"), "Process along field rows",
                         RESIDUAL_PROCESSES, "ar1"),
            select_input(ns("col_process"), "Process along field columns",
                         RESIDUAL_PROCESSES, "ar1"),
            note("Competition leaves a signature in the residuals along the ",
                 "direction it acts in: a plot that gives up yield to its ",
                 "neighbour is negatively correlated with it at lag 1, while ",
                 "lag 2 is positive. AR1 imposes a decay of one sign and ",
                 "cannot represent that, so the unmodelled part is pushed ",
                 "into the competitive effects. Choose ",
                 shiny::strong("AR2"), " or ", shiny::strong("SAR2"),
                 " on the competition axis when that is a concern."),
            switch_input(ns("nugget"), "Independent nugget variance", TRUE,
                         help = "Separates plot measurement error from the spatial trend.")
          ),
          switch_input(ns("auto_simplify"), "Simplify automatically if singular", TRUE,
                       help = paste("Steps down a ladder of nested models and",
                                    "reports exactly which one was fitted. Never",
                                    "silently uses ai.sing.")),
          switch_input(ns("exact_se"), "Exact pure-stand standard errors", TRUE,
                       help = paste("Requests the inverse coefficient matrix so",
                                    "pure-stand intervals and reliabilities are",
                                    "exact. Turn off for very large trials.")),
          switch_input(ns("compare_baseline"), "Compare against a no-competition model",
                       TRUE,
                       help = "Adds a likelihood-ratio test of whether competition matters."),
          shiny::numericInput(ns("maxit"), "Maximum ASReml iterations", 60,
                              min = 10, max = 1000, step = 10, width = "100%")
        )
      ),

      shiny::hr(),
      shiny::actionButton(ns("run"), "Fit competition model",
                          icon = shiny::icon("play"),
                          class = "btn-primary w-100 btn-lg"),
      shiny::uiOutput(ns("run_note"))
    ),

    # ---- main results area -------------------------------------------------
    bslib::navset_card_tab(
      id = ns("tabs"),

      bslib::nav_panel(
        "Data & layout", icon = ic("table"),
        shiny::uiOutput(ns("data_status")),
        # Data-dependent cards stay hidden until a file is loaded, so the
        # landing view is the upload prompt rather than four empty panels.
        shiny::conditionalPanel(
          condition = "output.has_data === true", ns = ns,
          panel_card("Field layout summary", DT::DTOutput(ns("field_summary")),
                     icon_name = "rulers", full_screen = FALSE),
          bslib::layout_columns(
            col_widths = c(7, 5),
            panel_card("Observed yield on the field plan",
                       figure_ui(ns("fig_field"), "420px"), icon_name = "map"),
            panel_card("Uploaded data", DT::DTOutput(ns("preview")),
                       icon_name = "file-earmark-spreadsheet")
          )
        ),
        shiny::conditionalPanel(
          condition = "output.has_relationship === true", ns = ns,
          panel_card("Genetic relationship matrix",
                     figure_ui(ns("fig_relationship"), "520px"),
                     icon_name = "diagram-3")
        )
      ),

      bslib::nav_panel(
        "Genetic values", icon = ic("award"),
        shiny::uiOutput(ns("result_status")),
        shiny::uiOutput(ns("metrics")),
        panel_card(
          "Evaluated genotypes",
          table_download_ui(ns("dl_genetic"), "Download evaluated genotypes"),
          DT::DTOutput(ns("genetic")),
          note(shiny::strong("Pure-stand value"), " is the direct effect plus ",
               "k times the competitive effect: what the genotype would express ",
               "if every neighbour were itself. Genotypes are ranked on ",
               shiny::strong("predicted pure-stand performance"), ". ",
               shiny::strong("Reliability"), " is the squared correlation between ",
               "the predicted and true genetic value; above about 0.5 the ",
               "prediction is dependable enough to select on."),
          icon_name = "award"
        ),
        shiny::conditionalPanel(
          condition = "output.has_inferred === true", ns = ns,
          panel_card(
            "Relatives predicted from the relationship matrix",
            table_download_ui(ns("dl_inferred"), "Download predicted relatives"),
            DT::DTOutput(ns("inferred")),
            note("Genotypes with plots in this trial are shown above. Individuals that appear only in the relationship matrix - parents and other relatives with no plot - are predicted from their relatives and are listed separately below, because their values rest on pedigree or marker information rather than on their own performance."),
            icon_name = "diagram-3")
        )
      ),

      bslib::nav_panel(
        "Figures", icon = ic("graph-up"),
        bslib::navset_pill(
          bslib::nav_panel("Direct vs competitive",
                           figure_ui(ns("fig_scatter"), "540px", interactive = TRUE)),
          bslib::nav_panel("Ranking",
                           shiny::div(
                             class = "fig-toolbar",
                             shiny::sliderInput(ns("top_n"), "Genotypes shown",
                                                min = 5, max = 60, value = 25,
                                                step = 5, width = "260px")),
                           figure_ui(ns("fig_ranking"), "620px")),
          bslib::nav_panel("Rank change",
                           figure_ui(ns("fig_rankchange"), "620px")),
          bslib::nav_panel("Variance components",
                           figure_ui(ns("fig_varcomp"), "460px"))
        )
      ),

      bslib::nav_panel(
        "Variance & heritability", icon = ic("bar-chart-steps"),
        bslib::layout_columns(
          col_widths = c(12),
          panel_card("Interpreted variance components",
                     table_download_ui(ns("dl_variance"), "Download variances"),
                     DT::DTOutput(ns("variance")), icon_name = "calculator",
                     full_screen = FALSE)
        ),
        shiny::uiOutput(ns("fixed_card")),
        panel_card("Heritability and accuracy",
                   table_download_ui(ns("dl_h2"), "Download heritability"),
                   DT::DTOutput(ns("heritability")),
                   note("Cullis generalised heritability, computed from the ",
                        "prediction error variances rather than from a balanced ",
                        "-design formula, so it is valid for unequal replication. ",
                        shiny::strong("Accuracy"), " is the square root of ",
                        "reliability, the correlation between the predicted and ",
                        "true genetic value. The pure-stand row is the one that ",
                        "matters for selection, and it is usually the lower of ",
                        "the two."),
                   icon_name = "percent", full_screen = FALSE),
        panel_card("All ASReml variance parameters", DT::DTOutput(ns("varcomp")),
                   icon_name = "list-columns",
                   full_screen = FALSE),
        shiny::uiOutput(ns("comparison_card"))
      ),

      bslib::nav_panel(
        "Diagnostics", icon = ic("activity"),
        bslib::navset_pill(
          bslib::nav_panel("Residual diagnostics",
                           figure_ui(ns("fig_diag"), "720px")),
          bslib::nav_panel("Sample variogram",
                           figure_ui(ns("fig_vario"), "520px")),
          bslib::nav_panel("Residual table",
                           table_download_ui(ns("dl_resid"), "Download residuals"),
                           DT::DTOutput(ns("resid")))
        )
      ),

      bslib::nav_panel(
        "Model detail", icon = ic("code-square"),
        panel_card("What was fitted", shiny::uiOutput(ns("model_detail")),
                   icon_name = "info-circle", full_screen = FALSE),
        panel_card("Fitting log", shiny::verbatimTextOutput(ns("log")),
                   icon_name = "journal-text", full_screen = FALSE),
        panel_card("Model summary", shiny::verbatimTextOutput(ns("summary")),
                   icon_name = "terminal"),
        panel_card("Random effect solutions", DT::DTOutput(ns("solutions")),
                   icon_name = "list-ol")
      ),

      bslib::nav_panel(
        "Export", icon = ic("box-arrow-down"),
        panel_card(
          "Download the complete analysis",
          note("One workbook containing every result table, and a multi-page ",
               "vector PDF containing every figure at publication quality."),
          shiny::downloadButton(ns("dl_workbook"), "All tables (Excel or CSV)",
                                class = "btn-primary"),
          shiny::downloadButton(ns("dl_figures"), "All figures (multi-page PDF)",
                                class = "btn-primary"),
          icon_name = "download", full_screen = FALSE
        )
      )
    )
  )
}

single_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ---- data ------------------------------------------------------------
    relationship <- relationship_server("rel")

    raw <- shiny::reactive({
      shiny::req(input$file)
      read_trial_file(input$file$datapath, isTRUE(input$header), input$separator)
    })

    output$mapping <- shiny::renderUI({
      nms <- names(raw())
      optional <- c("(none)" = "", stats::setNames(nms, nms))
      shiny::tagList(
        select_input(ns("yield_col"), "Response (yield)", nms,
                     guess_column(nms, c("yield", "grain", "gy", "response", "trait"), nms[1])),
        select_input(ns("geno_col"), "Genotype", nms,
                     guess_column(nms, c("^geno", "genotype", "entry", "variety", "hybrid", "line"), nms[1])),
        shiny::fluidRow(
          shiny::column(6, select_input(ns("row_col"), "Field row", nms,
                                        guess_column(nms, c("^row$", "field.*row", "^row"), nms[1]))),
          shiny::column(6, select_input(ns("column_col"), "Field column", nms,
                                        guess_column(nms, c("^col", "column", "range"), nms[1])))
        ),
        shiny::fluidRow(
          shiny::column(6, select_input(ns("rep_col"), "Replicate (optional)", optional,
                                        guess_column(nms, c("^rep", "replicate"), ""))),
          shiny::column(6, select_input(ns("block_col"), "Block (optional)", optional,
                                        guess_column(nms, c("block", "iblk", "incomplete"), "")))
        ),
        select_input(ns("covariate_col"),
                     "Covariate (optional)", optional,
                     guess_column(nms, c("height", "^ph$", "plant.?height",
                                         "canopy", "vigour", "vigor", "biomass"), "")),
        shiny::conditionalPanel(
          sprintf("input['%s'] != ''", ns("covariate_col")),
          switch_input(ns("adjust_own_covariate"),
                       "Also adjust for the plot's own value", FALSE,
                       help = paste("Off by default: the focal plot's own covariate",
                                    "value absorbs genetic variation in that",
                                    "covariate and so removes part of the direct",
                                    "effect being estimated."))
        ),
        note("A measured proxy for the physical cause of interference \u2014 plant ",
             "height, canopy width, root vigour. The neighbouring plots' values ",
             "enter as a fixed covariate, so competition attributable to the ",
             "covariate is removed before the genetic competitive effects are ",
             "estimated. Leave blank for a single-trait model."),
        select_input(ns("site_col"), "Site / environment column (optional)", optional,
                     guess_column(nms, c("^env", "environment", "^site", "location", "trial"), "")),
        shiny::uiOutput(ns("site_pick")),
        note("Use physical field coordinates, not spreadsheet line numbers. ",
             "Each row-column position must identify exactly one plot. ",
             "If the file holds several sites, name the site column above ",
             "and pick one to analyse on its own.")
      )
    })

    # Sites available in the uploaded file, when a site column is named.
    site_levels <- shiny::reactive({
      if (is_blank(input$site_col)) return(NULL)
      shiny::req(raw())
      v <- raw()[[input$site_col]]
      if (is.null(v)) return(NULL)
      ordered_unique(trimws(as.character(v)))
    })

    output$site_pick <- shiny::renderUI({
      levs <- site_levels()
      if (is.null(levs)) return(NULL)
      select_input(ns("site_value"),
                   sprintf("Analyse which site? (%d in this file)", length(levs)),
                   levs, levs[1])
    })
    shiny::outputOptions(output, "site_pick", suspendWhenHidden = FALSE)

    # One site of a multi-site file, or the file as uploaded.
    single_site <- shiny::reactive({
      d <- raw()
      levs <- site_levels()
      if (is.null(levs)) return(d)
      shiny::req(input$site_value)
      keep <- trimws(as.character(d[[input$site_col]])) == input$site_value
      out <- d[keep, , drop = FALSE]
      if (!nrow(out)) {
        stop("No plots remain after filtering to site '", input$site_value,
             "'.", call. = FALSE)
      }
      rownames(out) <- NULL
      out
    })

    prepared <- shiny::reactive({
      shiny::req(raw(), input$yield_col, input$geno_col, input$row_col, input$column_col)
      prepare_trial_data(single_site(), list(
        yield = input$yield_col, geno = input$geno_col, row = input$row_col,
        column = input$column_col, rep = input$rep_col, block = input$block_col,
        covariate = input$covariate_col))
    })

    # Validated data, or the error message, without aborting the whole session.
    # A `req()` further up raises a silent error, which means "not ready yet",
    # not "invalid data". The two are reported very differently to the user.
    safe_prepared <- shiny::reactive({
      tryCatch(
        list(ok = TRUE, data = prepared()),
        error = function(e) {
          list(ok = FALSE, pending = inherits(e, "shiny.silent.error"),
               message = conditionMessage(e))
        })
    })

    output$data_status <- shiny::renderUI({
      if (is.null(input$file)) {
        return(status_banner(
          "warn", "No data loaded yet",
          "Upload a CSV in the sidebar, or download the worked example to see ",
          "the expected layout.", icon_name = "upload"))
      }
      z <- safe_prepared()
      if (isTRUE(z$pending)) {
        return(status_banner("warn", "Waiting for the column mapping",
                             "Open 'Column mapping' in the sidebar and choose ",
                             "the response, genotype and field-coordinate columns.",
                             icon_name = "hourglass-split"))
      }
      if (!isTRUE(z$ok)) {
        return(status_banner("bad", "The data could not be prepared", z$message))
      }
      d <- z$data
      fs <- attr(d, "field_summary")
      gaps <- sum(fs$Gaps_in_grid)
      nb <- add_neighbours(d, input$axis %||% "rows")
      diag <- competition_diagnostics(nb$data, nb$names)
      warns <- competition_warnings(diag)

      shiny::tagList(
        metric_row(
          # When a multi-site file has been filtered, say so plainly: every
          # number below refers to that site alone.
          if (!is.null(site_levels())) {
            metric("Site", input$site_value %||% "\u2013",
                   sprintf("1 of %d in the file", length(site_levels())))
          },
          metric("Plots", nrow(d), sprintf("%d with a response", sum(!is.na(d$Yield)))),
          metric("Genotypes", nlevels(d$Geno),
                 sprintf("%d\u2013%d plots each", min(fs$Min_reps), max(fs$Max_reps))),
          metric("Field", fs$Grid[1], "rows x columns"),
          metric("Full neighbour sets", sprintf("%.0f%%", diag$full_neighbour_pct),
                 sprintf("%d border plots", diag$border_plots)),
          metric("Grid gaps", gaps,
                 if (gaps) "padded automatically" else "grid is complete")
        ),
        if (length(warns)) {
          status_banner("warn", "Points to check before interpreting the results",
                        shiny::tags$ul(lapply(warns, shiny::tags$li)))
        } else {
          status_banner("ok", "The layout supports a competition model",
                        sprintf("%s. Each genotype borders %.1f distinct neighbour genotypes on average.",
                                nb$label, diag$mean_distinct_neighbours))
        }
      )
    })

    output$has_data <- shiny::reactive({ isTRUE(safe_prepared()$ok) })
    shiny::outputOptions(output, "has_data", suspendWhenHidden = FALSE)
    # The mapping controls sit inside an accordion panel the user may collapse.
    # Without this, Shiny suspends the renderUI, the inputs never exist, and
    # data preparation fails with an empty message.
    shiny::outputOptions(output, "mapping", suspendWhenHidden = FALSE)

    output$has_relationship <- shiny::reactive({
      rel <- relationship()
      !is.null(rel) && !is.null(rel$matrix)
    })
    shiny::outputOptions(output, "has_relationship", suspendWhenHidden = FALSE)

    figure_server("fig_relationship", function(bs) {
      rel <- relationship()
      shiny::validate(shiny::need(!is.null(rel), "No relationship matrix supplied."))
      plot_relationship(rel, bs, rel$label)
    }, "relationship_matrix", "520px")

    output$field_summary <- DT::renderDT({
      shiny::req(safe_prepared()$ok)
      dt_table(attr(prepared(), "field_summary"), digits = 1, page_length = 5)
    })

    output$preview <- DT::renderDT({ dt_table(raw(), digits = 3, page_length = 10) })

    figure_server("fig_field", function(bs) {
      shiny::req(safe_prepared()$ok)
      d <- prepared()
      plot_field_map(
        data.frame(Row = d$Row_i, Column = d$Col_i, Env = d$Env, Observed = d$Yield),
        "Observed", "Observed response on the field plan",
        "Strong blocks of colour indicate field trend that the spatial model should absorb",
        diverging = FALSE, base_size = bs, fill_label = "Response")
    }, "field_plan_observed", "420px")

    # ---- fitting ----------------------------------------------------------
    result <- shiny::eventReactive(input$run, {
      z <- safe_prepared()
      if (!isTRUE(z$ok)) stop(z$message)
      d <- z$data

      blocked <- relationship_blocking_message(input[["rel-source"]], relationship())
      if (!is.null(blocked)) stop(blocked)

      prog <- shiny::Progress$new(session, min = 0, max = 1)
      on.exit(prog$close(), add = TRUE)
      prog$set(0.05, message = "Preparing the field layout")

      if (isTRUE(input$spatial)) d <- complete_field_grid(d)
      nb <- add_neighbours(d, input$axis)

      prog$set(0.15, message = "Fitting with ASReml-R",
               detail = "Checking the licence")

      fit_single_model(
        nb$data, nb$names,
        opts = list(
          structure = input$structure, spatial = isTRUE(input$spatial),
          nugget = isTRUE(input$spatial) && isTRUE(input$nugget),
          auto_simplify = isTRUE(input$auto_simplify),
          exact_se = isTRUE(input$exact_se),
          compare_baseline = isTRUE(input$compare_baseline),
          maxit = input$maxit, workspace = "2gb", cinv_limit = 6000L,
          row_process = input$row_process %||% "ar1",
          col_process = input$col_process %||% "ar1",
          adjust_own_covariate = isTRUE(input$adjust_own_covariate),
          relationship = relationship()),
        progress = function(i, n, reason) {
          prog$set(0.15 + 0.7 * i / max(n, 1),
                   message = sprintf("Fitting model %d of at most %d", i, n),
                   detail = reason)
        })
    }, ignoreInit = TRUE)

    # Evaluated once per change and reused, so a fitting error is rendered as a
    # banner in every panel rather than a red Shiny stack trace.
    safe_result <- shiny::reactive({
      tryCatch(list(ok = TRUE, value = result()),
               error = function(e) list(ok = FALSE, message = conditionMessage(e)))
    })
    res <- function() {
      z <- safe_result()
      shiny::validate(shiny::need(isTRUE(z$ok), z$message %||% "Fit the model first."))
      z$value
    }

    # Move the user to the results as soon as a fit succeeds, rather than
    # leaving them on the data panel wondering whether anything happened.
    shiny::observeEvent(safe_result(), {
      if (isTRUE(safe_result()$ok)) {
        bslib::nav_select("tabs", selected = "Genetic values", session = session)
      }
    }, ignoreInit = TRUE)

    output$run_note <- shiny::renderUI({
      shiny::tagList(shiny::br(), licence_notice())
    })

    output$result_status <- shiny::renderUI({
      shiny::req(input$run)
      z <- safe_result()
      if (!isTRUE(z$ok)) {
        return(status_banner("bad", "The model could not be fitted",
                             shiny::pre(style = "white-space:pre-wrap;font-size:.8rem;",
                                        z$message)))
      }
      r <- z$value
      items <- shiny::tagList(
        shiny::div(shiny::strong("Fitted: "), r$description),
        if (isTRUE(r$convergence_rounds > 0)) {
          shiny::div(shiny::strong("Convergence: "),
                     sprintf(paste("ASReml had not converged when it reached the",
                                   "iteration limit, so the fit was continued for",
                                   "%d further round(s)%s."),
                             r$convergence_rounds,
                             if (r$converged) " until it converged"
                             else ", and still reports no convergence"))
        },
        if (r$fallback_used) {
          shiny::div(shiny::strong("Note: "),
                     "the requested model was not identifiable, so the ",
                     "application stepped down to a nested model. See ",
                     shiny::em("Model detail"), " for every attempt.")
        },
        if (!r$exact_se) {
          shiny::div(shiny::strong("Standard errors: "),
                     "exact pure-stand errors were unavailable, so they are ",
                     "left blank rather than approximated.")
        }
      )
      status_banner(
        if (r$converged && !r$fallback_used) "ok" else if (r$converged) "warn" else "bad",
        if (r$converged) "Model converged" else "Model stopped before converging",
        items)
    })

    output$metrics <- shiny::renderUI({
      r <- res()
      cmp <- r$comparison
      metric_row(
        metric("Direct variance", fmt(r$components$direct, 3), "own-plot genetic variance"),
        metric("Competitive variance", fmt(r$components$competition, 3),
               "effect on neighbours"),
        metric("Direct-competition r", fmt(r$components$correlation, 2),
               if (isTRUE(r$components$correlation < 0)) "high yielders suppress neighbours"
               else "high yielders favour neighbours"),
        metric("Pure-stand variance", fmt(r$components$pure, 3),
               sprintf("Var(D + %d C)", r$k)),
        metric("Heritability (direct)", fmt(r$heritability[["direct"]], 2),
               if (is.null(r$relationship)) "entry-mean, Cullis"
               else "narrow-sense, Cullis"),
        if (!is.null(cmp)) {
          metric("Competition LRT",
                 if (is.na(cmp$lrt$p_value)) "\u2013"
                 else format.pval(cmp$lrt$p_value, digits = 2, eps = 1e-12),
                 sprintf("chi-square %.1f on %d df", cmp$lrt$LR_statistic, cmp$lrt$df))
        }
      )
    })

    # Evaluated genotypes and relatives predicted from the relationship matrix
    # are reported apart: the second group has no plots of its own, so their
    # values rest entirely on pedigree or marker information.
    genetic_split <- shiny::reactive({
      g <- res()$genetic
      if (!"Tested" %in% names(g)) {
        return(list(evaluated = g, inferred = g[0, , drop = FALSE]))
      }
      list(evaluated = g[g$Tested == "In trial", setdiff(names(g), "Tested"), drop = FALSE],
           inferred  = g[g$Tested != "In trial", setdiff(names(g), "Tested"), drop = FALSE])
    })

    output$has_inferred <- shiny::reactive({ nrow(genetic_split()$inferred) > 0 })
    shiny::outputOptions(output, "has_inferred", suspendWhenHidden = FALSE)

    output$genetic <- DT::renderDT({
      dt_table(genetic_split()$evaluated, digits = 4,
               highlight = "Predicted_pure_stand_yield")
    })
    output$inferred <- DT::renderDT({
      dt_table(genetic_split()$inferred, digits = 4,
               highlight = "Predicted_pure_stand_yield")
    })
    table_download_server("dl_genetic", function() genetic_split()$evaluated,
                          "interplot_genetic_values_evaluated")
    table_download_server("dl_inferred", function() genetic_split()$inferred,
                          "interplot_genetic_values_relatives")

    output$variance <- DT::renderDT({ dt_table(res()$variance, digits = 5, page_length = 10) })
    table_download_server("dl_variance", function() res()$variance,
                          "interplot_variance_components")

    output$varcomp <- DT::renderDT({ dt_table(res()$varcomp, digits = 5, page_length = 12) })
    output$heritability <- DT::renderDT({
      dt_table(single_heritability_table(res()), digits = 4, page_length = 5)
    })
    output$fixed_card <- shiny::renderUI({
      if (!length(res()$covariate_terms)) return(NULL)
      panel_card(
        "Covariate",
        shiny::h6("Wald test"),
        DT::DTOutput(ns("wald")),
        note(shiny::strong("Wald test. "),
             "A conditional F-test of each fixed term, adjusted for the terms ",
             "above it. ", shiny::strong("Retain = Yes"),
             " means the term is significant at p < 0.05 and is earning its ",
             "place; ", shiny::strong("No"),
             " means the covariate is not explaining variation in yield and can ",
             "be dropped, which returns the model to a single-trait analysis. ",
             "The denominator degrees of freedom are computed rather than ",
             "assumed infinite, so the test is not anti-conservative on a ",
             "trial-sized dataset."),
        shiny::h6("Estimated slopes"),
        DT::DTOutput(ns("fixed_effects")),
        note("The neighbour slope is the adjustment: it is the change in a ",
             "plot's yield per unit of the covariate summed over its neighbours. ",
             "A negative slope means larger neighbours suppress the focal plot, ",
             "which is interference the model has now removed before estimating ",
             "the genetic competitive effects. Compare the direct-competition ",
             "correlation with and without the covariate to see how much of the ",
             "competition it explains."),
        note(shiny::strong("Do not compare log-likelihood, AIC or BIC "),
             "between a run with the covariate and one without. Adding a ",
             "covariate changes the fixed model, and REML likelihoods are only ",
             "comparable when the fixed effects are identical. Compare the ",
             "variance components and the direct-competition correlation ",
             "instead. The likelihood-ratio test reported elsewhere is ",
             "unaffected: it compares two models that share whatever fixed ",
             "effects are in force."),
        icon_name = "rulers", full_screen = FALSE)
    })
    output$fixed_effects <- DT::renderDT({
      dt_table(res()$fixed_effects, digits = 5, page_length = 10)
    })
    output$wald <- DT::renderDT({
      w <- res()$wald
      shiny::validate(shiny::need(!is.null(w),
        "ASReml did not return a Wald table for this model."))
      dt_table(w, digits = 4, page_length = 10)
    })
    table_download_server("dl_h2", function() single_heritability_table(res()),
                          "interplot_heritability")

    output$comparison_card <- shiny::renderUI({
      r <- res()
      if (is.null(r$comparison)) return(NULL)
      panel_card(
        "Does competition matter?",
        DT::DTOutput(ns("comparison")),
        note("The two models share an identical fixed and residual structure ",
             "and differ only by the competitive effects, so the REML ",
             "likelihood-ratio test is valid. Because variance parameters are ",
             "tested at a boundary, the p-value is conservative: a significant ",
             "result is trustworthy, a marginal one may understate the ",
             "evidence. A large drop in AIC is the practical signal that ",
             "modelling competition has improved the analysis."),
        icon_name = "clipboard-data", full_screen = FALSE)
    })
    output$comparison <- DT::renderDT({
      r <- res()
      shiny::req(r$comparison)
      dt_table(rbind(
        cbind(r$comparison$table, Test = ""),
        data.frame(Model = "Likelihood-ratio test", LogLik = NA_real_,
                   Parameters = r$comparison$lrt$df, AIC = NA_real_, BIC = NA_real_,
                   Converged = NA, Test = sprintf("chi-square = %.2f, df = %d, p = %s",
                                                  r$comparison$lrt$LR_statistic,
                                                  r$comparison$lrt$df,
                                                  format.pval(r$comparison$lrt$p_value,
                                                              digits = 3, eps = 1e-12)))
      ), digits = 3, page_length = 5)
    })

    # ---- figures ----------------------------------------------------------
    cap <- function() model_caption(res())

    figure_server("fig_scatter", function(bs) {
      plot_direct_vs_competition(res()$genetic, res()$k, 12, bs, cap())
    }, "direct_vs_competitive", "540px", interactive = TRUE)

    figure_server("fig_ranking", function(bs) {
      plot_ranking(genetic_split()$evaluated, "Predicted_pure_stand_yield",
                   top_n = input$top_n %||% 25, base_size = bs, caption = cap())
    }, "pure_stand_ranking", "620px")

    figure_server("fig_rankchange", function(bs) {
      plot_rank_change(res()$genetic, 25, bs, cap())
    }, "rank_change", "620px")

    figure_server("fig_varcomp", function(bs) {
      plot_variance_components(res()$varcomp, bs, cap())
    }, "variance_components", "460px")

    figure_server("fig_diag", function(bs) {
      plot_residual_diagnostics(res()$residuals, bs, cap())
    }, "residual_diagnostics", "720px")

    variogram <- shiny::reactive(residual_variogram(res()$fit))
    figure_server("fig_vario", function(bs) {
      v <- variogram()
      shiny::validate(shiny::need(
        !is.null(v) && nrow(v),
        "A sample variogram is only produced for a spatial (AR1 x AR1) model."))
      plot_variogram(v, bs, cap())
    }, "sample_variogram", "520px")

    output$resid <- DT::renderDT({ dt_table(res()$residuals, digits = 4) })
    table_download_server("dl_resid", function() res()$residuals,
                          "interplot_residuals")

    # ---- model detail -----------------------------------------------------
    output$model_detail <- shiny::renderUI({
      r <- res()
      shiny::tagList(
        shiny::tags$dl(
          shiny::tags$dt("Fitted model"), shiny::tags$dd(r$description),
          shiny::tags$dt("Genetic covariance"), shiny::tags$dd(r$structure_note),
          shiny::tags$dt("Competing neighbours"),
          shiny::tags$dd(sprintf("%d per interior plot", r$k)),
          shiny::tags$dt("Genetic relationship"),
          shiny::tags$dd(if (is.null(r$relationship)) {
            "None: genotypes are treated as unrelated."
          } else {
            sprintf(paste("%s. %d individuals, of which %d have plots in this",
                          "trial and %d are predicted from their relatives.",
                          "The direct variance is therefore an additive genetic",
                          "variance and the heritability is narrow-sense."),
                    r$relationship$label, r$coverage$n_ids,
                    r$coverage$n_in_trial, r$coverage$n_extra)
          }),
          shiny::tags$dt("Standard errors"),
          shiny::tags$dd(if (r$exact_se) {
            paste("Exact, from the inverse mixed-model coefficient matrix.",
                  "The pure-stand error correctly includes the covariance",
                  "between the direct and competitive solutions.")
          } else {
            paste("Approximate for individual effects; pure-stand errors are",
                  "not reported because the required covariance was unavailable.")
          })
        ),
        if (length(r$warnings)) {
          status_banner("warn", "ASReml warnings",
                        shiny::tags$ul(lapply(unique(r$warnings), shiny::tags$li)))
        }
      )
    })

    output$log <- shiny::renderText({ format_attempt_log(res()$log) })
    output$summary <- shiny::renderPrint({ print_model_summary(res()) })
    output$solutions <- DT::renderDT({
      r <- res()
      cr <- as.data.frame(r$summary$coef.random)
      cr <- data.frame(Coefficient = rownames(cr), cr, row.names = NULL,
                       check.names = FALSE)
      dt_table(cr, digits = 5, page_length = 20)
    })

    # ---- exports ----------------------------------------------------------
    output$sample <- shiny::downloadHandler(
      filename = function() "example_single_trial_competition.csv",
      content = function(file) {
        utils::write.csv(sample_single_trial(), file, row.names = FALSE, na = "")
      }
    )

    output$dl_workbook <- shiny::downloadHandler(
      filename = function() {
        stamped("interplot_single_trial_results", if (has_pkg("writexl")) "xlsx" else "csv")
      },
      content = function(file) {
        r <- res()
        save_results_workbook(list(
          `Genetic values` = genetic_split()$evaluated,
          `Predicted relatives` = genetic_split()$inferred,
          `Heritability` = single_heritability_table(r),
          `Wald tests` = r$wald,
          `Variance summary` = r$variance,
          `ASReml variance parameters` = r$varcomp,
          `Model comparison` = r$comparison$table,
          `Residuals` = r$residuals,
          `Field summary` = attr(prepared(), "field_summary"),
          `Relationship` = relationship_export(r),
          `Fitting log` = data.frame(Step = r$log)
        ), file)
      }
    )

    output$dl_figures <- shiny::downloadHandler(
      filename = function() stamped("interplot_single_trial_figures", "pdf"),
      content = function(file) {
        r <- res()
        v <- variogram()
        figs <- list(
          function(bs) plot_direct_vs_competition(r$genetic, r$k, 12, bs, cap()),
          function(bs) plot_ranking(genetic_split()$evaluated,
                                    "Predicted_pure_stand_yield",
                                    top_n = 30, base_size = bs, caption = cap()),
          function(bs) plot_rank_change(r$genetic, 25, bs, cap()),
          function(bs) plot_variance_components(r$varcomp, bs, cap()),
          function(bs) plot_residual_diagnostics(r$residuals, bs, cap())
        )
        if (!is.null(v) && nrow(v)) {
          figs <- c(figs, list(function(bs) plot_variogram(v, bs, cap())))
        }
        save_figure_pdf_report(figs, file, width = 22, height = 16)
      }
    )
  })
}
