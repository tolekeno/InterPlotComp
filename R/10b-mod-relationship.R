# ---------------------------------------------------------------------------
# Relationship-matrix input (nested Shiny module)
#
# Used identically by both workspaces. The server returns a reactive that
# yields either NULL (independent genotypes) or the relationship object built
# by `build_relationship()`, so the calling module only has to pass it through
# to the fitting options.
# ---------------------------------------------------------------------------

relationship_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    radio_input(
      ns("source"), "Genetic relationship between genotypes",
      choices = RELATIONSHIP_SOURCES, selected = "none",
      help = paste(
        "By default genotypes are treated as unrelated. Supplying a pedigree or",
        "a marker set lets the model borrow information between relatives,",
        "which helps the competitive effect most because it is the harder of",
        "the two to estimate.")),

    shiny::conditionalPanel(
      sprintf("input['%s'] != 'none'", ns("source")),

      shiny::fileInput(ns("file"), "Relationship file (CSV or text)",
                       accept = c(".csv", ".txt", ".tsv", "text/csv"),
                       width = "100%"),
      shiny::fluidRow(
        shiny::column(6, shiny::selectInput(
          ns("separator"), "Separator",
          c("Comma" = ",", "Semicolon" = ";", "Tab" = "\t", "Space" = " "), ",")),
        shiny::column(6, shiny::div(
          style = "margin-top:1.9rem;",
          shiny::checkboxInput(ns("header"), "First row is a header", TRUE)))
      ),

      shiny::conditionalPanel(
        sprintf("input['%s'] == 'pedigree'", ns("source")),
        shiny::uiOutput(ns("pedigree_map")),
        shiny::downloadButton(ns("sample"), "Download example pedigree",
                              class = "btn-outline-primary btn-sm w-100")
      ),

      shiny::conditionalPanel(
        sprintf("input['%s'] == 'kinship'", ns("source")),
        note("A square matrix whose first column holds the genotype ",
             "identifiers and whose remaining columns are the relationship ",
             "values, in the same order.")
      ),

      shiny::conditionalPanel(
        sprintf("input['%s'] == 'markers'", ns("source")),
        note("One row per genotype: the first column is the identifier, the ",
             "rest are marker scores coded 0/1/2 or -1/0/1. A VanRaden genomic ",
             "relationship matrix is computed from them.")
      ),

      shiny::conditionalPanel(
        sprintf("input['%s'] == 'markers' || input['%s'] == 'kinship'",
                ns("source"), ns("source")),
        shiny::numericInput(
          ns("blend"), "Blending toward the identity matrix", 0.01,
          min = 0, max = 0.5, step = 0.01, width = "100%"),
        note("A genomic relationship matrix is singular whenever there are ",
             "fewer markers than genotypes, or duplicated lines, so a small ",
             "ridge is added before it is inverted. Raise this if the matrix ",
             "cannot be inverted.")
      ),

      shiny::uiOutput(ns("status"))
    )
  )
}

#' @return a reactive yielding the relationship object, or NULL
relationship_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    raw <- shiny::reactive({
      shiny::req(input$file)
      read_trial_file(input$file$datapath, isTRUE(input$header), input$separator)
    })

    output$pedigree_map <- shiny::renderUI({
      nms <- names(raw())
      shiny::tagList(
        select_input(ns("id_col"), "Individual identifier", nms,
                     guess_column(nms, c("^geno", "^id$", "^line", "individual", "entry"), nms[1])),
        shiny::fluidRow(
          shiny::column(6, select_input(
            ns("sire_col"), "Male parent", nms,
            guess_column(nms, c("male", "sire", "father", "^p1$"),
                         if (length(nms) > 1) nms[2] else nms[1]))),
          shiny::column(6, select_input(
            ns("dam_col"), "Female parent", nms,
            guess_column(nms, c("female", "dam", "mother", "^p2$"),
                         if (length(nms) > 2) nms[3] else nms[1])))
        ),
        note("Unknown parents may be blank, NA or 0. Parents that have no row ",
             "of their own are added as founders automatically.")
      )
    })
    shiny::outputOptions(output, "pedigree_map", suspendWhenHidden = FALSE)

    relationship <- shiny::reactive({
      type <- input$source %||% "none"
      if (identical(type, "none")) return(NULL)
      shiny::req(input$file)
      map <- if (identical(type, "pedigree")) {
        shiny::req(input$id_col, input$sire_col, input$dam_col)
        list(id = input$id_col, sire = input$sire_col, dam = input$dam_col)
      } else NULL
      build_relationship(type, raw(), map, blend = input$blend %||% 0.01)
    })

    safe_relationship <- shiny::reactive({
      tryCatch(
        list(ok = TRUE, value = relationship()),
        error = function(e) {
          list(ok = FALSE, pending = inherits(e, "shiny.silent.error"),
               message = conditionMessage(e))
        })
    })

    output$status <- shiny::renderUI({
      z <- safe_relationship()
      if (isTRUE(z$pending)) {
        return(status_banner("warn", "Waiting for the relationship file",
                             "Upload the file and confirm the column mapping.",
                             icon_name = "hourglass-split"))
      }
      if (!isTRUE(z$ok)) {
        return(status_banner("bad", "The relationship matrix could not be built",
                             z$message))
      }
      rel <- z$value
      if (is.null(rel)) return(NULL)
      dg <- rel$diagnostics
      status_banner(
        "ok", rel$label,
        shiny::tags$ul(
          shiny::tags$li(sprintf("%d individuals in the relationship matrix.", dg$n)),
          if (!is.na(dg$n_founders)) {
            shiny::tags$li(sprintf("%d founders with unknown parents.", dg$n_founders))
          },
          if (is.finite(dg$mean_inbreeding)) {
            shiny::tags$li(sprintf(
              "Mean inbreeding %.3f; %d individual(s) inbred (maximum %.3f).",
              dg$mean_inbreeding, dg$n_inbred, dg$max_inbreeding))
          }
        ))
    })

    output$sample <- shiny::downloadHandler(
      filename = function() "example_pedigree.csv",
      content = function(file) {
        utils::write.csv(sample_pedigree(), file, row.names = FALSE, na = "")
      })

    # NULL rather than an error when the file is not ready, so the rest of the
    # workspace stays usable while the user assembles the pedigree.
    shiny::reactive({
      z <- safe_relationship()
      if (isTRUE(z$ok)) z$value else NULL
    })
  })
}

#' Was a relationship matrix requested but not yet usable?
#'
#' The fit must not silently fall back to independent genotypes when the user
#' has asked for a pedigree and the file is broken.
relationship_blocking_message <- function(source, rel) {
  if (is_blank(source) || identical(source, "none")) return(NULL)
  if (!is.null(rel)) return(NULL)
  paste("A genetic relationship matrix was selected but could not be built.",
        "Correct the relationship file, or set the relationship source back to",
        "'None' to fit with independent genotypes.")
}
