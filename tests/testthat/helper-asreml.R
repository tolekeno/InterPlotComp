# ---------------------------------------------------------------------------
# Test helpers
#
# ASReml-R is commercial, is not on CRAN, and needs an activated licence even
# where it is installed. Every test that fits a model therefore goes through
# `skip_without_asreml()`, which checks both conditions. On CRAN, on a
# continuous-integration runner, and on a contributor's machine without a
# licence, those tests skip cleanly and the rest of the suite still runs.
# ---------------------------------------------------------------------------

#' Skip unless ASReml-R is installed *and* its licence can be checked out.
skip_without_asreml <- function() {
  testthat::skip_on_cran()
  if (!nzchar(system.file(package = "asreml"))) {
    testthat::skip("ASReml-R is not installed")
  }
  ok <- tryCatch({
    suppressPackageStartupMessages(loadNamespace("asreml"))
    TRUE
  }, error = function(e) FALSE)
  if (!ok) testthat::skip("ASReml-R is installed but not licensed in this session")
  invisible(TRUE)
}

#' The standard column mapping for `sample_single_trial()`.
single_map <- function(...) {
  utils::modifyList(
    list(yield = "Yield_t_ha", geno = "Genotype", row = "Row",
         column = "Column", rep = "Rep", block = "Block"),
    list(...)
  )
}

#' The standard column mapping for `sample_met_trial()`.
met_map <- function(...) {
  utils::modifyList(
    list(yield = "Yield_t_ha", geno = "Genotype", row = "Row",
         column = "Column", env = "Environment", rep = "Rep", block = "Block"),
    list(...)
  )
}

#' A small prepared single trial, grid-completed with neighbours attached.
prepared_single <- function(map = single_map(), complete = TRUE, axis = "rows") {
  d <- prepare_trial_data(sample_single_trial(), map)
  if (complete) d <- complete_field_grid(d)
  add_neighbours(d, axis)
}

#' Deparse a formula to a single line with whitespace squashed.
#'
#' `deparse()` wraps at 60 characters and indents the continuation, which puts
#' stray spaces inside terms such as `and(vm(N2, .kinship))` and makes a
#' literal match fail for reasons that have nothing to do with the model.
formula_text <- function(f) {
  gsub("[[:space:]]+", "", paste(deparse(f, width.cutoff = 500L), collapse = ""))
}
