# ---------------------------------------------------------------------------
# Multi-environment workspace (Shiny module)
#
# Mirrors the single-trial layout so that a user moving between the two
# workspaces finds the same controls in the same places.
# ---------------------------------------------------------------------------

met_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 360, open = "open", title = "MET set-up",

      bslib::accordion(
        open = c("data", "mapping", "competition"), multiple = TRUE,

        bslib::accordion_panel(
          "1. MET data", value = "data", icon = ic("file-earmark-arrow-up"),
          upload_panel(ns, multi_env = TRUE)
        ),
        bslib::accordion_panel(
          "2. Column mapping", value = "mapping", icon = ic("diagram-3"),
          shiny::uiOutput(ns("mapping"))
        ),
        bslib::accordion_panel(
          "3. Competition and G x E", value = "competition", icon = ic("grid-3x3"),
          radio_input(
            ns("axis"), "Which plots compete?",
            choices = stats::setNames(names(NEIGHBOUR_OFFSETS),
                                      unname(NEIGHBOUR_LABELS[names(NEIGHBOUR_OFFSETS)])),
            selected = "rows"),
          select_input(ns("structure"), "Genetic covariance across environments",
                       MET_STRUCTURES, "facv"),
          shiny::uiOutput(ns("rank_ui")),
          note("The joint structure lets direct and competitive effects have ",
               "different patterns of genotype-by-environment interaction. The ",
               "separable structure assumes one shared pattern but uses far ",
               "fewer parameters, so it fits when the joint model cannot. ",
               shiny::strong("Separate fa()"), " fits an ordinary ",
               shiny::code("fa()"), " term to each effect, which ASReml ",
               "cannot do inside the joint block; the price is that the ",
               "direct-competition covariance becomes a structural zero ",
               "rather than an estimate.")
        ),
        bslib::accordion_panel(
          "4. Genetic relationship", value = "relationship",
          icon = ic("diagram-3"),
          relationship_ui(ns("rel"))
        ),
        bslib::accordion_panel(
          "5. Spatial and error model", value = "spatial", icon = ic("layers"),
          switch_input(ns("spatial"), "Separable spatial residual per environment",
                       TRUE,
                       help = paste("Each environment gets its own spatial",
                                    "section, so trials may differ in size.")),
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
            switch_input(ns("nugget"), "Common independent nugget", FALSE,
                         help = paste("Often weakly identified alongside",
                                      "environment-specific spatial sections; off",
                                      "by default for the MET model."))
          ),
          switch_input(ns("auto_simplify"), "Simplify automatically if singular", TRUE),
          switch_input(ns("exact_se"), "Exact pure-stand standard errors", TRUE),
          switch_input(ns("compare_baseline"),
                       "Compare against a no-competition model", TRUE),
          shiny::numericInput(ns("maxit"), "Maximum ASReml iterations", 80,
                              min = 10, max = 1000, step = 10, width = "100%")
        )
      ),

      shiny::hr(),
      shiny::actionButton(ns("run"), "Fit MET competition model",
                          icon = shiny::icon("play"),
                          class = "btn-primary w-100 btn-lg"),
      shiny::uiOutput(ns("run_note"))
    ),

    bslib::navset_card_tab(
      id = ns("tabs"),

      bslib::nav_panel(
        "Data & layout", icon = ic("table"),
        shiny::uiOutput(ns("data_status")),
        shiny::conditionalPanel(
          condition = "output.has_data === true", ns = ns,
          panel_card("Environment summary", DT::DTOutput(ns("field_summary")),
                     icon_name = "rulers", full_screen = FALSE),
          bslib::layout_columns(
            col_widths = c(7, 5),
            panel_card("Field plans", figure_ui(ns("fig_field"), "520px"),
                       icon_name = "map"),
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
          table_download_ui(ns("dl_values"), "Download evaluated genotypes"),
          DT::DTOutput(ns("values")),
          note("Predicted pure-stand yield is the fitted environment mean plus ",
               "the environment-specific pure-stand genetic effect, and is what ",
               "the within-environment ranking is built on. Use the column ",
               "filters to isolate one environment or one genotype."),
          icon_name = "award"),
        shiny::conditionalPanel(
          condition = "output.has_inferred === true", ns = ns,
          panel_card(
            "Relatives predicted from the relationship matrix",
            table_download_ui(ns("dl_inferred"), "Download predicted relatives"),
            DT::DTOutput(ns("inferred")),
            note("Individuals that appear only in the relationship matrix - ",
                 "parents and other relatives with no plot in any environment - ",
                 "are predicted from their relatives and listed separately, ",
                 "because their values rest on pedigree or marker information ",
                 "rather than on their own performance."),
            icon_name = "diagram-3")
        )
      ),

      bslib::nav_panel(
        "Genetic correlations", icon = ic("diagram-2"),
        bslib::navset_pill(
          bslib::nav_panel("Direct", figure_ui(ns("fig_cor_direct"), "540px")),
          bslib::nav_panel("Competitive", figure_ui(ns("fig_cor_comp"), "540px")),
          bslib::nav_panel("Pure stand", figure_ui(ns("fig_cor_pure"), "540px")),
          bslib::nav_panel(
            "Matrices",
            table_download_ui(ns("dl_cor"), "Download all correlations"),
            shiny::h5("Direct effects"), DT::DTOutput(ns("cor_direct")),
            shiny::h5("Competitive effects"), DT::DTOutput(ns("cor_comp")),
            shiny::h5("Pure stand"), DT::DTOutput(ns("cor_pure")))
        ),
        note("A high positive correlation means genotypes rank similarly in the ",
             "two environments, so they can be treated as one selection ",
             "environment. Low or negative values indicate crossover ",
             "interaction and argue for environment-specific recommendations. ",
             "The pure-stand correlations combine the direct and competitive ",
             "matrices with their cross-covariance, and are the ones to use for ",
             "monoculture or on-farm decisions.")
      ),

      bslib::nav_panel(
        "Figures", icon = ic("graph-up"),
        bslib::navset_pill(
          bslib::nav_panel("Stability across environments",
                           shiny::div(class = "fig-toolbar",
                                      shiny::sliderInput(ns("top_n"), "Genotypes shown",
                                                         min = 4, max = 30, value = 12,
                                                         step = 2, width = "260px")),
                           figure_ui(ns("fig_stability"), "560px", interactive = TRUE)),
          bslib::nav_panel("Direct vs competitive",
                           figure_ui(ns("fig_scatter"), "620px")),
          bslib::nav_panel("Environment variances",
                           figure_ui(ns("fig_envvar"), "480px")),
          bslib::nav_panel("Factor-analytic fit",
                           figure_ui(ns("fig_fa"), "480px"))
        )
      ),

      bslib::nav_panel(
        "Variance & heritability", icon = ic("bar-chart-steps"),
        panel_card("Genetic variance by environment",
                   table_download_ui(ns("dl_variance"), "Download variances"),
                   DT::DTOutput(ns("variance")),
                   shiny::uiOutput(ns("dc_note")),
                   icon_name = "calculator",
                   full_screen = FALSE),
        shiny::uiOutput(ns("fixed_card")),
        panel_card("Heritability and accuracy by environment",
                   table_download_ui(ns("dl_h2"), "Download heritability"),
                   DT::DTOutput(ns("heritability")),
                   note("Cullis generalised heritability, computed from the ",
                        "prediction error variances, so it is valid under the ",
                        "unequal replication a MET usually has. The pure-stand ",
                        "column is the one selection acts on."),
                   icon_name = "percent", full_screen = FALSE),
        shiny::uiOutput(ns("fa_card")),
        panel_card("All ASReml variance parameters", DT::DTOutput(ns("varcomp")),
                   icon_name = "list-columns", full_screen = FALSE),
        shiny::uiOutput(ns("comparison_card"))
      ),

      bslib::nav_panel(
        "Diagnostics", icon = ic("activity"),
        bslib::navset_pill(
          bslib::nav_panel("Residual diagnostics", figure_ui(ns("fig_diag"), "720px")),
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
                   icon_name = "terminal")
      ),

      bslib::nav_panel(
        "Export", icon = ic("box-arrow-down"),
        panel_card(
          "Download the complete MET analysis",
          shiny::downloadButton(ns("dl_workbook"), "All tables (Excel or CSV)",
                                class = "btn-primary"),
          shiny::downloadButton(ns("dl_figures"), "All figures (multi-page PDF)",
                                class = "btn-primary"),
          icon_name = "download", full_screen = FALSE)
      )
    )
  )
}

met_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    relationship <- relationship_server("rel")

    raw <- shiny::reactive({
      shiny::req(input$file)
      read_trial_file(input$file$datapath, isTRUE(input$header), input$separator)
    })

    output$mapping <- shiny::renderUI({
      nms <- names(raw())
      optional <- c("(none)" = "", stats::setNames(nms, nms))
      shiny::tagList(
        select_input(ns("env_col"), "Environment", nms,
                     guess_column(nms, c("^env", "environment", "site", "location", "trial"), nms[1])),
        select_input(ns("yield_col"), "Response (yield)", nms,
                     guess_column(nms, c("yield", "grain", "response", "trait"), nms[1])),
        select_input(ns("geno_col"), "Genotype", nms,
                     guess_column(nms, c("^geno", "genotype", "entry", "variety", "hybrid"), nms[1])),
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
                                        guess_column(nms, c("block", "iblk"), "")))
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

        note("Row-column positions must be unique within each environment. ",
             "Environments may differ in size and in the genotypes they carry.")
      )
    })

    prepared <- shiny::reactive({
      shiny::req(raw(), input$env_col, input$yield_col, input$geno_col,
                 input$row_col, input$column_col)
      prepare_trial_data(raw(), list(
        env = input$env_col, yield = input$yield_col, geno = input$geno_col,
        row = input$row_col, column = input$column_col,
        rep = input$rep_col, block = input$block_col,
        covariate = input$covariate_col), multi_env = TRUE)
    })

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

    output$rank_ui <- shiny::renderUI({
      n_env <- tryCatch(nlevels(prepared()$Env), error = function(e) 2L)
      max_r <- max_fa_rank(n_env, input$structure %||% "facv")
      shiny::selectInput(ns("rank"), "Factor-analytic rank",
                         choices = seq_len(max_r),
                         selected = min(1L, max_r), width = "100%")
    })

    output$data_status <- shiny::renderUI({
      if (is.null(input$file)) {
        return(empty_state(
          "No multi-environment data loaded yet",
          paste("This workspace fits direct and competitive effects jointly",
                "across environments, so that the genetic correlations between",
                "sites are estimated rather than assumed."),
          steps = c(
            "Upload a CSV whose rows are plots and which carries an environment column, or download the worked example.",
            "Map the environment, response, genotype and field-coordinate columns.",
            "Choose the across-environment covariance structure.",
            "Fit the model, then read the genetic correlations before pooling sites.")))
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
      nb <- add_neighbours(d, input$axis %||% "rows")
      diag <- competition_diagnostics(nb$data, nb$names)
      warns <- competition_warnings(diag)
      connect <- table(d$Geno[!is.na(d$Yield)], d$Env[!is.na(d$Yield)]) > 0
      shared <- sum(rowSums(connect) == ncol(connect))

      shiny::tagList(
        metric_row(
          metric("Environments", nlevels(d$Env)),
          metric("Genotypes", nlevels(d$Geno),
                 sprintf("%d in every environment", shared)),
          metric("Plots", nrow(d), sprintf("%d with a response", sum(!is.na(d$Yield)))),
          metric("Grid gaps", sum(fs$Gaps_in_grid), "padded automatically"),
          metric("Full neighbour sets", sprintf("%.0f%%", diag$full_neighbour_pct))
        ),
        if (shared < 5) {
          status_banner("warn", "Environments are weakly connected",
                        sprintf(paste("Only %d genotype(s) appear in every environment.",
                                      "Genetic correlations between environments rest on",
                                      "the shared genotypes, so they will be imprecise."),
                                shared))
        } else if (length(warns)) {
          status_banner("warn", "Points to check before interpreting the results",
                        shiny::tags$ul(lapply(warns, shiny::tags$li)))
        } else {
          status_banner("ok", "The data support a MET competition model",
                        sprintf("%s. %d genotypes are common to all environments.",
                                nb$label, shared))
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
      dt_table(attr(prepared(), "field_summary"), digits = 1, page_length = 8)
    })
    output$preview <- DT::renderDT({ dt_table(raw(), digits = 3, page_length = 10) })

    figure_server("fig_field", function(bs) {
      shiny::req(safe_prepared()$ok)
      d <- prepared()
      plot_field_map(
        data.frame(Row = d$Row_i, Column = d$Col_i, Env = d$Env, Observed = d$Yield),
        "Observed", "Observed response by environment", NULL,
        diverging = FALSE, facet = TRUE, base_size = bs, fill_label = "Response")
    }, "met_field_plans", "520px")

    # ---- fitting ----------------------------------------------------------
    result <- shiny::eventReactive(input$run, {
      z <- safe_prepared()
      if (!isTRUE(z$ok)) stop(z$message)
      d <- z$data

      blocked <- relationship_blocking_message(input[["rel-source"]], relationship())
      if (!is.null(blocked)) stop(blocked)

      prog <- shiny::Progress$new(session, min = 0, max = 1)
      on.exit(prog$close(), add = TRUE)
      prog$set(0.05, message = "Preparing the field layouts")

      if (isTRUE(input$spatial)) d <- complete_field_grid(d)
      nb <- add_neighbours(d, input$axis)

      prog$set(0.15, message = "Fitting with ASReml-R", detail = "Checking the licence")
      fit_met_model(
        nb$data, nb$names,
        opts = list(
          structure = input$structure, rank = as.integer(input$rank %||% 1L),
          spatial = isTRUE(input$spatial),
          nugget = isTRUE(input$spatial) && isTRUE(input$nugget),
          auto_simplify = isTRUE(input$auto_simplify),
          exact_se = isTRUE(input$exact_se),
          compare_baseline = isTRUE(input$compare_baseline),
          maxit = input$maxit, workspace = "4gb", cinv_limit = 8000L,
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

    output$run_note <- shiny::renderUI({ shiny::tagList(shiny::br(), licence_notice()) })

    output$result_status <- shiny::renderUI({
      shiny::req(input$run)
      z <- safe_result()
      if (!isTRUE(z$ok)) {
        return(status_banner("bad", "The MET model could not be fitted",
                             shiny::pre(class = "error-detail",
                                        z$message)))
      }
      r <- z$value
      status_banner(
        if (r$converged && !r$fallback_used) "ok" else if (r$converged) "warn" else "bad",
        if (r$converged) "MET model converged" else "MET model stopped before converging",
        shiny::tagList(
          shiny::div(shiny::strong("Fitted: "), r$description),
          if (isTRUE(r$convergence_rounds > 0)) {
            shiny::div(shiny::strong("Convergence: "),
                       sprintf(paste("the fit was continued for %d further",
                                     "round(s)%s after reaching the iteration",
                                     "limit."),
                               r$convergence_rounds,
                               if (r$converged) " until it converged"
                               else ", and still reports no convergence"))
          },
          if (r$fallback_used) {
            shiny::div(shiny::strong("Note: "),
                       "the requested model was not identifiable; a nested model ",
                       "was used. See ", shiny::em("Model detail"), ".")
          },
          if (isTRUE(r$dc_covariance_fixed)) {
            shiny::div(
              shiny::strong("The direct-competition covariance is fixed at zero. "),
              "Separate fa() terms make the direct and competitive effects ",
              "independent by construction, so that covariance is not an ",
              "estimate and cannot be tested. Pure-stand variance reduces to ",
              "Var(D) + k\u00b2 Var(C) and will be overstated wherever the two ",
              "effects are in fact negatively correlated. Use the joint ",
              "factor-analytic structure to estimate it.")
          },
          if (r$correlations_assumed) {
            shiny::div(shiny::strong("Genetic correlations were not estimated. "),
                       "The successful model assumes environments are ",
                       "independent, so the heatmaps show that assumption ",
                       "(ones on the diagonal, zeros elsewhere) rather than ",
                       "estimated correlations.")
          }))
    })

    output$metrics <- shiny::renderUI({
      r <- res()
      v <- r$variance
      off <- r$matrices$pure_cor[upper.tri(r$matrices$pure_cor)]
      metric_row(
        metric("Environments", length(r$environments)),
        metric("Mean direct variance", fmt(mean(v$Direct_variance, na.rm = TRUE), 3)),
        metric("Mean competitive variance", fmt(mean(v$Competition_variance, na.rm = TRUE), 3)),
        metric("Mean pure-stand r", fmt(mean(off, na.rm = TRUE), 2),
               "between environments"),
        metric("Mean heritability", fmt(mean(r$heritability, na.rm = TRUE), 2),
               if (is.null(r$relationship)) "direct effects, entry-mean"
               else "direct effects, narrow-sense"),
        if (!is.null(r$comparison)) {
          metric("Competition LRT",
                 format.pval(r$comparison$lrt$p_value, digits = 2, eps = 1e-12),
                 sprintf("chi-square %.1f on %d df", r$comparison$lrt$LR_statistic,
                         r$comparison$lrt$df))
        }
      )
    })

    values_split <- shiny::reactive({
      v <- res()$values
      if (!"Tested" %in% names(v)) {
        return(list(evaluated = v, inferred = v[0, , drop = FALSE]))
      }
      keep <- setdiff(names(v), "Tested")
      list(evaluated = v[v$Tested == "In trial", keep, drop = FALSE],
           inferred  = v[v$Tested != "In trial", keep, drop = FALSE])
    })
    output$has_inferred <- shiny::reactive({ nrow(values_split()$inferred) > 0 })
    shiny::outputOptions(output, "has_inferred", suspendWhenHidden = FALSE)

    output$values <- DT::renderDT({
      dt_table(values_split()$evaluated, digits = 4,
               highlight = "Predicted_pure_stand_yield")
    })
    output$inferred <- DT::renderDT({
      dt_table(values_split()$inferred, digits = 4,
               highlight = "Predicted_pure_stand_yield")
    })
    table_download_server("dl_values", function() values_split()$evaluated,
                          "met_genetic_values_evaluated")
    table_download_server("dl_inferred", function() values_split()$inferred,
                          "met_genetic_values_relatives")

    output$variance <- DT::renderDT({ dt_table(res()$variance, digits = 5, page_length = 10) })
    output$dc_note <- shiny::renderUI({
      if (!isTRUE(res()$dc_covariance_fixed)) return(NULL)
      note(shiny::strong("Direct-competition covariance and correlation are ",
                         "zero by construction, not estimated. "),
           "The fitted structure makes the two effects independent, so the ",
           "pure-stand variance omits the 2k Cov(D, C) term. Where that ",
           "covariance is negative, as it usually is, the pure-stand ",
           "variance shown here is an overestimate.")
    })
    table_download_server("dl_variance", function() res()$variance, "met_environment_variances")
    output$varcomp <- DT::renderDT({ dt_table(res()$varcomp, digits = 5, page_length = 15) })
    output$heritability <- DT::renderDT({
      dt_table(met_heritability_table(res()), digits = 4, page_length = 10)
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
        note("The neighbour slope is the change in a plot's yield per unit of ",
             "the covariate summed over its neighbours, fitted in common across ",
             "environments. A negative slope means larger neighbours suppress ",
             "the focal plot."),
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
      dt_table(res()$fixed_effects, digits = 5, page_length = 12)
    })
    output$wald <- DT::renderDT({
      w <- res()$wald
      shiny::validate(shiny::need(!is.null(w),
        "ASReml did not return a Wald table for this model."))
      dt_table(w, digits = 4, page_length = 10)
    })
    table_download_server("dl_h2", function() met_heritability_table(res()),
                          "met_heritability")

    output$fa_card <- shiny::renderUI({
      if (is.null(res()$fa_summary)) return(NULL)
      panel_card("Factor-analytic fit by environment", DT::DTOutput(ns("fa")),
                 note("The percentage of genetic variance explained by the ",
                      "common factors. An environment below about 75% is poorly ",
                      "described by the shared factors and behaves ",
                      "idiosyncratically; treat its predictions separately."),
                 icon_name = "diagram-3", full_screen = FALSE)
    })
    output$fa <- DT::renderDT({ dt_table(res()$fa_summary, digits = 4, page_length = 10) })

    output$comparison_card <- shiny::renderUI({
      if (is.null(res()$comparison)) return(NULL)
      panel_card("Does competition matter?", DT::DTOutput(ns("comparison")),
                 note("Both models share the same fixed and residual structure, ",
                      "so the REML likelihood-ratio test is valid. Testing ",
                      "variance parameters at a boundary makes the p-value ",
                      "conservative."),
                 icon_name = "clipboard-data", full_screen = FALSE)
    })
    output$comparison <- DT::renderDT({
      r <- res(); shiny::req(r$comparison)
      dt_table(cbind(r$comparison$table,
                     Test = c(sprintf("chi-square = %.2f, df = %d, p = %s",
                                      r$comparison$lrt$LR_statistic, r$comparison$lrt$df,
                                      format.pval(r$comparison$lrt$p_value,
                                                  digits = 3, eps = 1e-12)), "")),
               digits = 3, page_length = 5)
    })

    # ---- correlation outputs ----------------------------------------------
    cap <- function() model_caption(res())
    cor_note <- function() {
      if (isTRUE(res()$correlations_assumed)) {
        "Correlations were not estimable; the fitted model assumes independent environments."
      } else NULL
    }

    figure_server("fig_cor_direct", function(bs) {
      plot_correlation_heatmap(res()$matrices$direct_cor,
                               "Direct-effect genetic correlations",
                               "Similarity of own-plot genotype ranking between environments",
                               bs, paste(c(cor_note(), cap()), collapse = " "))
    }, "met_correlations_direct", "540px")

    figure_server("fig_cor_comp", function(bs) {
      plot_correlation_heatmap(res()$matrices$competition_cor,
                               "Competitive-effect genetic correlations",
                               "Similarity of a genotype's effect on its neighbours between environments",
                               bs, paste(c(cor_note(), cap()), collapse = " "))
    }, "met_correlations_competitive", "540px")

    figure_server("fig_cor_pure", function(bs) {
      plot_correlation_heatmap(res()$matrices$pure_cor,
                               "Pure-stand genetic correlations",
                               "The correlations to use for monoculture and on-farm recommendations",
                               bs, paste(c(cor_note(), cap()), collapse = " "))
    }, "met_correlations_pure_stand", "540px")

    output$cor_direct <- DT::renderDT({
      dt_table(matrix_to_wide(res()$matrices$direct_cor), digits = 3, page_length = 10)
    })
    output$cor_comp <- DT::renderDT({
      dt_table(matrix_to_wide(res()$matrices$competition_cor), digits = 3, page_length = 10)
    })
    output$cor_pure <- DT::renderDT({
      dt_table(matrix_to_wide(res()$matrices$pure_cor), digits = 3, page_length = 10)
    })

    correlation_long <- function() {
      m <- res()$matrices
      rbind(matrix_to_long(m$direct_cor, "Correlation", "Direct"),
            matrix_to_long(m$competition_cor, "Correlation", "Competitive"),
            matrix_to_long(m$pure_cor, "Correlation", "Pure stand"))
    }
    table_download_server("dl_cor", correlation_long, "met_genetic_correlations")

    # ---- other figures ----------------------------------------------------
    figure_server("fig_stability", function(bs) {
      plot_stability(values_split()$evaluated, input$top_n %||% 12, bs, cap())
    }, "met_stability", "560px", interactive = TRUE)

    figure_server("fig_scatter", function(bs) {
      plot_met_scatter(values_split()$evaluated, res()$k, bs, cap())
    }, "met_direct_vs_competitive", "620px")

    figure_server("fig_envvar", function(bs) {
      plot_environment_variances(res()$variance, bs, cap())
    }, "met_environment_variances", "480px")

    figure_server("fig_fa", function(bs) {
      fa <- res()$fa_summary
      shiny::validate(shiny::need(
        !is.null(fa),
        "This figure is only produced for a factor-analytic genetic covariance."))
      plot_fa_summary(fa, bs, cap())
    }, "met_factor_analytic_fit", "480px")

    figure_server("fig_diag", function(bs) {
      plot_residual_diagnostics(res()$residuals, bs, cap())
    }, "met_residual_diagnostics", "720px")

    output$resid <- DT::renderDT({ dt_table(res()$residuals, digits = 4) })
    table_download_server("dl_resid", function() res()$residuals, "met_residuals")

    # ---- model detail -----------------------------------------------------
    output$model_detail <- shiny::renderUI({
      r <- res()
      shiny::tagList(
        shiny::tags$dl(
          shiny::tags$dt("Fitted model"), shiny::tags$dd(r$description),
          shiny::tags$dt("Genetic covariance"), shiny::tags$dd(r$structure_note),
          shiny::tags$dt("Pure-stand covariance"),
          shiny::tags$dd(sprintf(
            "G_pure = G_direct + %d^2 G_competition + %d (G_dc + G_dc'), the variance of D + %d C.",
            r$k, r$k, r$k)),
          shiny::tags$dt("Genetic relationship"),
          shiny::tags$dd(if (is.null(r$relationship)) {
            "None: genotypes are treated as unrelated."
          } else {
            sprintf(paste("%s. %d individuals, of which %d have plots and %d are",
                          "predicted from their relatives."),
                    r$relationship$label, r$coverage$n_ids,
                    r$coverage$n_in_trial, r$coverage$n_extra)
          }),
          shiny::tags$dt("Standard errors"),
          shiny::tags$dd(if (r$exact_se) {
            "Exact, from the inverse mixed-model coefficient matrix."
          } else {
            "Pure-stand errors are not reported because the required covariance was unavailable."
          })
        ),
        if (length(r$warnings)) {
          status_banner("warn", "ASReml warnings",
                        shiny::tags$ul(lapply(unique(r$warnings), shiny::tags$li)))
        })
    })
    output$log <- shiny::renderText({ format_attempt_log(res()$log) })
    output$summary <- shiny::renderPrint({ print_model_summary(res()) })

    # ---- exports ----------------------------------------------------------
    output$sample <- shiny::downloadHandler(
      filename = function() "example_met_competition.csv",
      content = function(file) {
        utils::write.csv(sample_met_trial(), file, row.names = FALSE, na = "")
      })

    output$dl_workbook <- shiny::downloadHandler(
      filename = function() {
        stamped("interplot_met_results", if (has_pkg("writexl")) "xlsx" else "csv")
      },
      content = function(file) {
        r <- res()
        save_results_workbook(list(
          `Genetic values` = values_split()$evaluated,
          `Predicted relatives` = values_split()$inferred,
          `Heritability` = met_heritability_table(r),
          `Wald tests` = r$wald,
          `Environment variances` = r$variance,
          `Correlations` = correlation_long(),
          `Factor analytic fit` = r$fa_summary,
          `ASReml variance parameters` = r$varcomp,
          `Model comparison` = r$comparison$table,
          `Residuals` = r$residuals,
          `Environment summary` = attr(prepared(), "field_summary"),
          `Relationship` = relationship_export(r),
          `Fitting log` = data.frame(Step = r$log)
        ), file)
      })

    output$dl_figures <- shiny::downloadHandler(
      filename = function() stamped("interplot_met_figures", "pdf"),
      content = function(file) {
        r <- res()
        figs <- list(
          function(bs) plot_correlation_heatmap(r$matrices$direct_cor,
                                                "Direct-effect genetic correlations",
                                                NULL, bs, cap()),
          function(bs) plot_correlation_heatmap(r$matrices$competition_cor,
                                                "Competitive-effect genetic correlations",
                                                NULL, bs, cap()),
          function(bs) plot_correlation_heatmap(r$matrices$pure_cor,
                                                "Pure-stand genetic correlations",
                                                NULL, bs, cap()),
          function(bs) plot_environment_variances(r$variance, bs, cap()),
          function(bs) plot_stability(values_split()$evaluated, 12, bs, cap()),
          function(bs) plot_met_scatter(values_split()$evaluated, r$k, bs, cap()),
          function(bs) plot_residual_diagnostics(r$residuals, bs, cap())
        )
        if (!is.null(r$fa_summary)) {
          figs <- append(figs, list(function(bs) plot_fa_summary(r$fa_summary, bs, cap())), 4)
        }
        save_figure_pdf_report(figs, file, width = 22, height = 16)
      })
  })
}
