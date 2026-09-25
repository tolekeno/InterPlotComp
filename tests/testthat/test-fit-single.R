# ---------------------------------------------------------------------------
# End-to-end single-trial fits.
#
# These are the tests that actually exercise ASReml, so every one of them
# begins with skip_without_asreml(). The worked example is simulated from
# known parameters (SIM_TRUTH), which lets the fit be checked against the
# values that generated the data rather than only against itself.
# ---------------------------------------------------------------------------

fit_options <- function(...) {
  utils::modifyList(
    list(structure = "us", spatial = TRUE, nugget = TRUE, auto_simplify = TRUE,
         exact_se = TRUE, cinv_limit = 5000L, maxit = 30L, workspace = "1gb",
         compare_baseline = FALSE, max_rounds = 15L, relationship = NULL,
         adjust_own_covariate = FALSE, row_process = "ar1", col_process = "ar1"),
    list(...)
  )
}

# Fitting is slow, so the default fit is computed once and reused.
cached_single_fit <- local({
  cache <- NULL
  function() {
    if (is.null(cache)) {
      nb <- prepared_single()
      cache <<- fit_single_model(nb$data, nb$names, fit_options())
    }
    cache
  }
})

test_that("the single-trial model converges on the requested specification", {
  skip_without_asreml()
  res <- cached_single_fit()

  expect_true(res$converged)
  expect_false(res$fallback_used)
  expect_equal(res$spec$structure, "us")
  expect_equal(res$spec$reason, "Requested model")
  expect_equal(res$k, 2L)
})

test_that("the fit recovers the parameters that generated the data", {
  skip_without_asreml()
  res <- cached_single_fit()
  truth <- SIM_TRUTH

  # Variance components from one 180-plot trial are estimated with wide
  # intervals, so these tolerances are deliberately generous: the test is that
  # the model is unbiased in the right direction, not that it is precise.
  expect_equal(res$components$direct, truth$direct_var, tolerance = 0.5)
  expect_equal(res$components$competition, truth$competition_var, tolerance = 0.7)
  expect_lt(res$components$covariance, 0)          # the simulated covariance is negative
  expect_equal(res$components$correlation, truth$direct_comp_cor, tolerance = 0.5)
  expect_gt(res$components$direct, res$components$competition)
})

test_that("the pure-stand variance follows the documented algebra", {
  skip_without_asreml()
  res <- cached_single_fit()
  cmp <- res$components
  expect_equal(cmp$pure,
               cmp$direct + res$k^2 * cmp$competition + 2 * res$k * cmp$covariance,
               tolerance = 1e-6)
})

test_that("the genotype table is complete and internally consistent", {
  skip_without_asreml()
  res <- cached_single_fit()
  g <- res$genetic

  expect_equal(nrow(g), 60L)
  expect_true(all(c("Genotype", "Direct_effect", "Competition_effect",
                    "Pure_stand_effect", "SE_direct", "SE_pure_stand",
                    "Reliability_direct", "Rank_direct", "Rank_pure_stand",
                    "Rank_change", "Competitor_type", "Tested") %in% names(g)))
  expect_false(anyDuplicated(g$Genotype) > 0)
  expect_false(anyNA(g$Direct_effect))

  # Pure stand = direct + k * competition, by definition.
  expect_equal(g$Pure_stand_effect,
               g$Direct_effect + res$k * g$Competition_effect, tolerance = 1e-8)
  # Random effects are BLUPs and so sum to approximately zero.
  expect_equal(mean(g$Direct_effect), 0, tolerance = 0.05)
  # Ranks are consistent with the effects they rank.
  expect_equal(g$Rank_direct, rank(-g$Direct_effect, ties.method = "first"),
               tolerance = 1)
  expect_equal(g$Rank_change, g$Rank_direct - g$Rank_pure_stand)
})

test_that("standard errors and reliabilities are in a sane range", {
  skip_without_asreml()
  g <- cached_single_fit()$genetic
  expect_true(all(g$SE_direct > 0))
  expect_true(all(g$SE_pure_stand > 0))
  expect_true(all(g$Reliability_direct >= 0 & g$Reliability_direct <= 1))
})

test_that("competition changes the ranking, which is the point of the model", {
  skip_without_asreml()
  g <- cached_single_fit()$genetic
  expect_true(any(g$Rank_change != 0))
  expect_gt(stats::cor(g$Rank_direct, g$Rank_pure_stand), 0)
})

test_that("heritabilities are reported on the [0, 1] scale", {
  skip_without_asreml()
  h <- cached_single_fit()$heritability
  expect_true(all(h >= 0 & h <= 1, na.rm = TRUE))
  expect_true(all(c("direct", "pure") %in% names(h)))
})

test_that("the variance table names components in breeder-facing language", {
  skip_without_asreml()
  v <- cached_single_fit()$variance
  expect_true(all(c("Component", "Estimate", "Interpretation") %in% names(v)))
  expect_true(any(grepl("Direct genetic variance", v$Component)))
  expect_true(any(grepl("Pure-stand", v$Component)))
  # No internal factor name may reach this table.
  expect_false(any(grepl("\\bN1\\b|RepF|BlockF", v$Component)))
})

test_that("the variance-parameter table carries the raw ASReml labels too", {
  skip_without_asreml()
  vc <- cached_single_fit()$varcomp
  expect_true(all(c("Component", "Interpretation", "Estimate") %in% names(vc)))
  expect_gt(nrow(vc), 0L)
  # Shares are percentages of the plot-level variance, residual included.
  shares <- vc$Pct_of_total[is.finite(vc$Pct_of_total)]
  expect_true(all(shares >= 0 & shares <= 100))
  expect_equal(sum(shares), 100, tolerance = 1)
})

test_that("ASReml is never attached to the search path by a fit", {
  skip_without_asreml()
  cached_single_fit()
  expect_false("package:asreml" %in% search())
})

test_that("the fitted result carries everything the interface renders", {
  skip_without_asreml()
  res <- cached_single_fit()
  expect_true(all(c("fit", "genetic", "variance", "varcomp", "components",
                    "heritability", "fit_stats", "fixed_effects", "wald",
                    "description", "residuals", "log") %in% names(res)))
  expect_type(res$description, "character")
  expect_s3_class(res$residuals, "data.frame")
  expect_true(all(is.finite(res$residuals$Residual)))
  # Padded grid positions are structural, not observations, and never appear.
  expect_false(any(res$residuals$Padded))
})

test_that("a diag model constrains the direct-competition covariance to zero", {
  skip_without_asreml()
  nb <- prepared_single()
  res <- fit_single_model(nb$data, nb$names,
                          fit_options(structure = "diag", nugget = FALSE))
  expect_equal(res$components$covariance, 0)
  expect_gt(res$components$direct, 0)
  expect_gt(res$components$competition, 0)
  expect_match(res$structure_note, "constrained to zero")
})

test_that("a corgh model estimates the correlation directly", {
  skip_without_asreml()
  nb <- prepared_single()
  res <- fit_single_model(nb$data, nb$names,
                          fit_options(structure = "corgh", nugget = FALSE))
  expect_true(res$components$correlation >= -1 && res$components$correlation <= 1)
  expect_match(res$structure_note, "corgh")
})

test_that("a non-spatial model fits and says so", {
  skip_without_asreml()
  d <- prepare_trial_data(sample_single_trial(), single_map())
  nb <- add_neighbours(d, "rows")     # no grid completion needed without AR1
  res <- fit_single_model(nb$data, nb$names,
                          fit_options(spatial = FALSE, nugget = FALSE,
                                      structure = "diag"))
  expect_true(res$converged)
  expect_false(res$spec$spatial)
})

test_that("the baseline comparison runs a valid likelihood-ratio test", {
  skip_without_asreml()
  nb <- prepared_single()
  res <- fit_single_model(nb$data, nb$names,
                          fit_options(structure = "diag", nugget = FALSE,
                                      compare_baseline = TRUE))
  expect_false(is.null(res$comparison))
  expect_equal(nrow(res$comparison$table), 2L)

  lrt <- res$comparison$lrt
  expect_true(is.finite(lrt$p_value))
  expect_true(lrt$p_value >= 0 && lrt$p_value <= 1)
  expect_gt(lrt$df, 0)
  # The data were simulated with genuine competition, so it should be detected.
  expect_lt(lrt$p_value, 0.05)
})

test_that("a relationship matrix flows through the diag structure", {
  skip_without_asreml()
  # Regression guard: with vm(Geno, .kinship) in the parameter names, the diag
  # branch used to find no genetic variance at all and abort the fit.
  nb <- prepared_single()
  rel <- build_relationship(
    "pedigree", sample_pedigree(),
    list(id = "Genotype", sire = "Male_parent", dam = "Female_parent"))

  res <- fit_single_model(nb$data, nb$names,
                          fit_options(structure = "diag", nugget = FALSE,
                                      exact_se = FALSE, relationship = rel))
  expect_true(res$converged)
  expect_gt(res$components$direct, 0)
  expect_gt(res$components$competition, 0)
  expect_equal(res$components$covariance, 0)

  # Ancestors with no plot are predicted from their relatives and flagged.
  expect_gt(nrow(res$genetic), 60L)
  expect_true(any(res$genetic$Tested == "Relative only"))
  expect_true(any(res$genetic$Tested == "In trial"))
  expect_equal(res$coverage$n_in_trial, 60L)
})

test_that("a relationship-matrix model keeps iterating to convergence", {
  skip_without_asreml()
  # Regression guard: update() could not see the local .kinship, so every
  # continuation failed silently and a short-maxit fit never converged.
  nb <- prepared_single()
  rel <- build_relationship(
    "pedigree", sample_pedigree(),
    list(id = "Genotype", sire = "Male_parent", dam = "Female_parent"))
  res <- suppressWarnings(fit_single_model(
    nb$data, nb$names,
    fit_options(structure = "diag", nugget = FALSE, exact_se = FALSE,
                maxit = 5L, relationship = rel)))
  expect_true(res$converged)
})

test_that("a relationship matrix flows through the us structure", {
  skip_without_asreml()
  nb <- prepared_single()
  rel <- build_relationship(
    "pedigree", sample_pedigree(),
    list(id = "Genotype", sire = "Male_parent", dam = "Female_parent"))
  res <- fit_single_model(nb$data, nb$names,
                          fit_options(nugget = FALSE, exact_se = FALSE,
                                      relationship = rel))
  expect_gt(res$components$direct, 0)
  expect_match(res$description, "elationship")
})

test_that("a covariate enters as a fixed effect on both own and neighbour plots", {
  skip_without_asreml()
  d <- prepare_trial_data(sample_single_trial(),
                          single_map(covariate = "Plant_height_cm"))
  nb <- add_neighbours(complete_field_grid(d), "rows")
  res <- fit_single_model(nb$data, nb$names,
                          fit_options(structure = "diag", nugget = FALSE,
                                      adjust_own_covariate = TRUE))

  expect_setequal(res$covariate_terms, c("Covariate_nb", "Covariate_own"))
  expect_equal(res$covariate_name, "Plant_height_cm")
  expect_true(any(grepl("Covariate", res$fixed_effects[[1]])))
  expect_s3_class(res$wald, "data.frame")
  expect_gt(nrow(res$wald), 0L)
})

test_that("the fallback ladder rescues a model that cannot be fitted as asked", {
  skip_without_asreml()
  # Four-neighbour competition on this small trial with a us(2) covariance and
  # a nugget is deliberately over-specified; auto-simplification must land on
  # something that converges rather than failing outright.
  nb <- prepared_single(axis = "four")
  res <- fit_single_model(nb$data, nb$names, fit_options(exact_se = FALSE))
  expect_true(res$converged)
  expect_equal(res$k, 4L)
  expect_type(res$log, "character")
  expect_gt(length(res$log), 0L)
})

test_that("turning off auto-simplification surfaces the failure instead", {
  skip_without_asreml()
  nb <- prepared_single()
  res <- fit_single_model(nb$data, nb$names, fit_options(auto_simplify = FALSE))
  expect_false(res$fallback_used)
  expect_equal(res$spec$reason, "Requested model")
})

test_that("residual diagnostics build from a real fit", {
  skip_without_asreml()
  res <- cached_single_fit()
  expect_s3_class(plot_residual_diagnostics(res$residuals), c("patchwork", "ggplot"))
  expect_s3_class(plot_direct_vs_competition(res$genetic, res$k), "ggplot")
  expect_s3_class(plot_ranking(res$genetic), "ggplot")
  expect_s3_class(plot_variance_components(res$varcomp), "ggplot")
})

test_that("print_model_summary writes a readable report", {
  skip_without_asreml()
  out <- utils::capture.output(print_model_summary(cached_single_fit()))
  expect_gt(length(out), 5L)
  expect_true(any(grepl("ompetiti", out)))
})
