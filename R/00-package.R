#' InterPlotComp: inter-plot competition analysis for plant breeding trials
#'
#' Fits direct-competition mixed models to unbordered single-row-plot breeding
#' trials. For every genotype the model separates the effect expressed in its
#' own plot (the *direct* effect) from the effect it imposes on neighbouring
#' plots (the *competitive* effect), and combines them into the value the
#' genotype would express in a pure stand.
#'
#' @section Getting started:
#' [run_app()] launches the interface. To script an analysis instead, the
#' pipeline is [prepare_trial_data()], then [complete_field_grid()] for a
#' spatial model, then [add_neighbours()], then [fit_single_model()] or
#' [fit_met_model()]. [build_relationship()] supplies a pedigree or genomic
#' relationship matrix. [sample_single_trial()], [sample_met_trial()] and
#' [sample_pedigree()] generate worked examples from known parameters.
#'
#' @section ASReml-R licence:
#' ASReml-R is commercial software licensed by VSNi. This package is a front
#' end: it uses whatever ASReml-R installation and already-activated licence
#' exist in the R process that runs it, and it never reads, writes, embeds,
#' stores or transmits a licence key. Without a valid licence no model can be
#' fitted, and there is no fallback engine.
#'
#' @keywords internal
#' @importFrom rlang .data
"_PACKAGE"

# `Env` is a column of the prepared trial data, referred to unquoted inside
# ggplot2 facet specifications; declaring it here keeps R CMD check quiet
# without rewriting those calls.
utils::globalVariables("Env")
