# ---------------------------------------------------------------------------
# Guide and About tabs
#
# The guide is written for a breeder rather than a statistician: what the model
# assumes, what each number means, and what would make the result untrustworthy.
# ---------------------------------------------------------------------------

guide_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_columns(
    col_widths = c(8, 4),

    shiny::div(
      bslib::accordion(
        open = "model", multiple = TRUE,

        bslib::accordion_panel(
          "The competition model", value = "model", icon = ic("diagram-3"),
          shiny::withMathJax(shiny::helpText(
            "$$y_i = \\mu + \\text{design}_i + d_{g(i)} + \\sum_{j \\in N(i)} c_{g(j)} + s_i + e_i$$")),
          shiny::tags$ul(
            shiny::tags$li(shiny::strong("Direct effect "), shiny::em("d"),
                           " \u2014 how a genotype performs in its own plot."),
            shiny::tags$li(shiny::strong("Competitive effect "), shiny::em("c"),
                           " \u2014 how much a genotype raises or lowers the yield ",
                           "of the plots next to it. A negative value means it ",
                           "suppresses its neighbours, typically by competing ",
                           "for light, water or nutrients."),
            shiny::tags$li(shiny::strong("Pure-stand value "), shiny::em("d + k c"),
                           " \u2014 what the genotype would express if every ",
                           "neighbour were itself, with ", shiny::em("k"),
                           " the number of competing neighbours. This is the ",
                           "quantity that carries forward to a monoculture ",
                           "block or a farmer's field."),
            shiny::tags$li(shiny::strong("Spatial term "), shiny::em("s"),
                           " \u2014 a separable AR1 x AR1 field trend, fitted so ",
                           "that smooth fertility gradients are not mistaken for ",
                           "competition.")
          ),
          shiny::p("Why this matters: in unbordered single-row plots, part of a ",
                   "plot's yield is taken from, or given to, its neighbours. ",
                   "Selecting on plot yield alone therefore favours aggressive ",
                   "genotypes whose advantage disappears once every plot is ",
                   "planted to the same entry."),
          shiny::p(shiny::strong("Key references. "),
                   "Besag & Kempton (1986) Biometrics 42:231-251; ",
                   "Stringer, Cullis & Thompson (2011) JABES 16:269-281; ",
                   "Gilmour, Cullis & Verbyla (1997) JABES 2:269-293; ",
                   "Smith, Cullis & Thompson (2001) Biometrics 57:1138-1147; ",
                   "Cullis, Smith & Coombes (2006) JABES 11:381-393; ",
                   "Keno, Mace, Godwin, Jordan & Kelly (2026) ",
                   "Theor Appl Genet 139:174.")
        ),

        bslib::accordion_panel(
          "Preparing your data", value = "data", icon = ic("table"),
          shiny::tags$ol(
            shiny::tags$li(shiny::strong("Use physical field coordinates."),
                           " Row and column must describe where the plot ",
                           "actually stood, not the order of lines in the ",
                           "spreadsheet. This is the single most common cause ",
                           "of a meaningless competition analysis."),
            shiny::tags$li(shiny::strong("One plot per row-column position"),
                           " within each environment."),
            shiny::tags$li(shiny::strong("Keep failed plots."),
                           " Leave the genotype and set the response to blank ",
                           "or NA. The plot still competes with its neighbours, ",
                           "so deleting the row loses real information."),
            shiny::tags$li(shiny::strong("Leave gaps as gaps."),
                           " Positions absent from the field (alleys, paths) are ",
                           "padded internally with a missing response so the ",
                           "spatial grid stays rectangular. They contribute no ",
                           "genotype and no competitive effect."),
            shiny::tags$li(shiny::strong("Check the competition direction."),
                           " Single-row plots normally compete along the row ",
                           "direction. Confirm against the field plan.")
          ),
          shiny::p("Replicate and block columns are optional. When both are ",
                   "supplied, blocks are nested within replicate (and within ",
                   "environment) automatically, which avoids the empty cells ",
                   "that otherwise destabilise the fit.")
        ),

        bslib::accordion_panel(
          "Reading the results", value = "results", icon = ic("graph-up"),
          shiny::tags$dl(
            shiny::tags$dt("Direct-competition correlation"),
            shiny::tags$dd("Usually negative: genotypes that yield well in their ",
                           "own plot tend to do so partly at their neighbours' ",
                           "expense. A strongly negative value is the clearest ",
                           "evidence that plot yield overstates true genetic merit."),
            shiny::tags$dt("Pure-stand genetic variance"),
            shiny::tags$dd("Var(D + kC). Often much smaller than the direct ",
                           "variance, because the direct and competitive ",
                           "effects partly cancel. This is the variance that ",
                           "genuine genetic gain can act on."),
            shiny::tags$dt("Reliability"),
            shiny::tags$dd("The squared correlation between the predicted and ",
                           "true genetic value, computed from the exact ",
                           "prediction error variance. Above roughly 0.5 the ",
                           "prediction is dependable enough to select on."),
            shiny::tags$dt("Rank change"),
            shiny::tags$dd("Genotypes that move a long way between the direct ",
                           "ranking and the pure-stand ranking are exactly ",
                           "those that would have been mis-selected on plot ",
                           "yield alone. This is the practical payoff of the model."),
            shiny::tags$dt("Likelihood-ratio test"),
            shiny::tags$dd("Compares the fitted model against one without ",
                           "competitive effects. Because variance parameters ",
                           "are tested at a boundary the p-value is ",
                           "conservative, so a significant result is ",
                           "trustworthy and a marginal one may understate the ",
                           "evidence.")
          )
        ),

        bslib::accordion_panel(
          "When not to trust the result", value = "caveats", icon = ic("exclamation-triangle"),
          shiny::tags$ul(
            shiny::tags$li("Each genotype borders only one or two distinct ",
                           "neighbour genotypes. Direct and competitive effects ",
                           "are then poorly separated and the covariance in ",
                           "particular is unreliable. The data panel warns about this."),
            shiny::tags$li("Most plots are on a border, so few records carry a ",
                           "complete neighbour set."),
            shiny::tags$li("Fewer than about 15 genotypes contribute observations."),
            shiny::tags$li("The direct-competition correlation is pinned at -1 ",
                           "or +1, or a variance sits at a boundary. The ",
                           "variance table flags boundary components; their ",
                           "standard errors are not valid."),
            shiny::tags$li("The residual field plan still shows large patches ",
                           "of one colour, meaning the spatial model has not ",
                           "absorbed the field trend and some of it may have ",
                           "been absorbed into the competitive effects instead."),
            shiny::tags$li("The application had to fall back to a simpler ",
                           "model. Every fallback is reported; read the fitting ",
                           "log before quoting the estimates.")
          )
        ),

        bslib::accordion_panel(
          "Pedigree and genomic relationships", value = "relationship",
          icon = ic("diagram-3"),
          shiny::p("By default the model treats genotypes as unrelated. ",
                   "Supplying a relationship matrix replaces that assumption ",
                   "with the known covariance between relatives, so each ",
                   "genotype's effects are estimated partly from its family. ",
                   "This helps the ", shiny::strong("competitive effect most"),
                   ", because it is the harder of the two to estimate and it ",
                   "borrows the most information from relatives."),
          shiny::tags$dl(
            shiny::tags$dt("Pedigree"),
            shiny::tags$dd("Three columns: individual, male parent, female ",
                           "parent. Unknown parents may be blank, NA or 0, and ",
                           "parents without a row of their own are added as ",
                           "founders. The numerator relationship matrix A is ",
                           "built with ASReml's ainverse()."),
            shiny::tags$dt("Relationship / kinship matrix"),
            shiny::tags$dd("A square matrix whose first column holds the ",
                           "genotype identifiers. Use this for a relationship ",
                           "matrix computed elsewhere."),
            shiny::tags$dt("Marker matrix"),
            shiny::tags$dd("One row per genotype, the first column the ",
                           "identifier and the rest marker scores coded 0/1/2 ",
                           "or -1/0/1. A VanRaden genomic relationship matrix ",
                           "is computed, with missing calls mean-imputed and ",
                           "monomorphic markers dropped.")
          ),
          shiny::h5("What changes in the output"),
          shiny::tags$ul(
            shiny::tags$li("The direct variance becomes an ", shiny::strong("additive"),
                           " genetic variance, so the reported heritability is ",
                           "narrow-sense rather than entry-mean."),
            shiny::tags$li("Individuals in the relationship matrix that have no ",
                           "plot in the trial, such as parents, still receive ",
                           "predicted effects from their relatives. They are ",
                           "flagged ", shiny::em("Relative only"), " in the ",
                           "results table, which is how you obtain parental ",
                           "breeding values from a progeny trial."),
            shiny::tags$li("Every genotype with an observed plot must appear in ",
                           "the relationship matrix. The application refuses to ",
                           "fit rather than quietly dropping the missing ones.")
          ),
          shiny::h5("Practical notes"),
          shiny::tags$ul(
            shiny::tags$li("A genomic relationship matrix is singular whenever ",
                           "there are fewer markers than genotypes, or ",
                           "duplicated lines. A small ridge is blended toward ",
                           "the identity before inversion; raise it if the ",
                           "matrix still cannot be inverted."),
            shiny::tags$li("A relationship matrix does not rescue a layout that ",
                           "cannot separate direct from competitive effects. ",
                           "Read the warnings on the data panel first."),
            shiny::tags$li("Fits are slower, because the genetic term now spans ",
                           "every individual in the relationship matrix rather ",
                           "than only those with plots.")
          )
        ),

        bslib::accordion_panel(
          "Multi-environment analysis", value = "met", icon = ic("globe"),
          shiny::p("The MET workspace estimates environment-specific direct and ",
                   "competitive effects and the genetic correlations between ",
                   "environments. Trials may differ in size, in the genotypes ",
                   "they carry and in replication."),
          shiny::tags$ul(
            shiny::tags$li(shiny::strong("Joint factor-analytic"), " lets the ",
                           "direct and competitive effects have different ",
                           "patterns of genotype-by-environment interaction. ",
                           "Most general; needs the most data."),
            shiny::tags$li(shiny::strong("Separable us(2) x FA"), " assumes one ",
                           "shared environment correlation pattern. Far fewer ",
                           "parameters, so it fits when the joint model cannot."),
            shiny::tags$li(shiny::strong("Diagonal"), " estimates no ",
                           "between-environment correlation at all. Use it as ",
                           "the null model, not as a result.")
          ),
          shiny::p("Genetic correlations rest on the genotypes shared between ",
                   "environments. With fewer than about five genotypes in ",
                   "common, treat them as indicative only.")
        )
      )
    ),

    shiny::div(
      panel_card("ASReml-R licence", licence_notice(), icon_name = "shield-lock",
                 full_screen = FALSE),
      panel_card(
        "Worked examples", icon_name = "download", full_screen = FALSE,
        shiny::p("Both examples are simulated from known parameters, listed ",
                 "below, so you can confirm the model recovers them."),
        DT::DTOutput(ns("truth")),
        note("Download the examples from the sidebar of either workspace.")
      ),
      panel_card(
        "Optional packages", icon_name = "puzzle", full_screen = FALSE,
        note("The application runs without these; each adds one feature."),
        DT::DTOutput(ns("optional"))
      ),
      panel_card(
        "Session", icon_name = "cpu", full_screen = FALSE,
        shiny::verbatimTextOutput(ns("session"))
      )
    )
  )
}

guide_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    output$truth <- DT::renderDT({
      dt_table(data.frame(
        Parameter = c("Direct genetic variance", "Competitive genetic variance",
                      "Direct-competition correlation", "Block variance",
                      "Spatial variance", "AR1 row correlation",
                      "AR1 column correlation", "Nugget variance", "Grand mean"),
        Value = unlist(SIM_TRUTH[c("direct_var", "competition_var", "direct_comp_cor",
                                   "block_var", "spatial_var", "ar_row", "ar_col",
                                   "nugget_var", "grand_mean")]),
        row.names = NULL), digits = 2, page_length = 9)
    })

    output$optional <- DT::renderDT({
      dt_table(optional_package_status(), page_length = 15)
    })

    output$session <- shiny::renderPrint({
      cat("Application version:", APP_VERSION, "\n")
      cat(R.version.string, "\n")
      cat("ASReml-R:", asreml_version() %||% "not installed", "\n")
      cat("Library paths:\n  ", paste(.libPaths(), collapse = "\n  "), "\n")
    })
  })
}
