# ---------------------------------------------------------------------------
# End-to-end multi-environment fits. Slow, and every one needs a licensed
# ASReml, so each begins with skip_without_asreml().
# ---------------------------------------------------------------------------

met_options <- function(...) {
  utils::modifyList(
    list(structure = "facv", rank = 1L, spatial = TRUE, nugget = FALSE,
         auto_simplify = TRUE, exact_se = FALSE, cinv_limit = 0L,
         maxit = 25L, workspace = "2gb", compare_baseline = FALSE,
         max_rounds = 10L, relationship = NULL, adjust_own_covariate = FALSE,
         row_process = "ar1", col_process = "ar1"),
    list(...)
  )
}

prepared_met <- function(map = met_map(), axis = "rows") {
  d <- prepare_trial_data(sample_met_trial(), map, multi_env = TRUE)
  add_neighbours(complete_field_grid(d), axis)
}

cached_met_fit <- local({
  cache <- NULL
  function() {
    if (is.null(cache)) {
      nb <- prepared_met()
      cache <<- fit_met_model(nb$data, nb$names, met_options(structure = "diag"))
    }
    cache
  }
})

test_that("the MET model fits across all four environments", {
  skip_without_asreml()
  res <- cached_met_fit()

  expect_true(res$converged)
  expect_equal(res$environments, c("Env01", "Env02", "Env03", "Env04"))
  expect_equal(res$k, 2L)
  expect_length(res$genotypes, 52L)
  expect_false("package:asreml" %in% search())
})

test_that("the MET covariance matrices have the right shape and algebra", {
  skip_without_asreml()
  res <- cached_met_fit()
  e <- length(res$environments)
  m <- res$matrices

  for (part in c("direct", "competition", "direct_competition", "pure")) {
    expect_equal(dim(m[[part]]), c(e, e), info = part)
  }
  expect_equal(rownames(m$direct), res$environments)

  # Pure stand = G_D + k^2 G_C + k (G_DC + G_DC').
  expect_equal(
    unname(m$pure),
    unname(m$direct + res$k^2 * m$competition +
             res$k * (m$direct_competition + t(m$direct_competition))),
    tolerance = 1e-6
  )
  # Variances on the diagonal must be non-negative.
  expect_true(all(diag(m$direct) >= 0))
  expect_true(all(diag(m$competition) >= 0))
})

test_that("a diag MET structure assumes no between-environment correlation", {
  skip_without_asreml()
  res <- cached_met_fit()
  expect_true(res$correlations_assumed)
  # Off-diagonal genetic covariances are structurally zero here.
  off <- res$matrices$direct[upper.tri(res$matrices$direct)]
  expect_true(all(abs(off) < 1e-10))
})

test_that("the MET genotype table reports every entry in every environment", {
  skip_without_asreml()
  res <- cached_met_fit()
  v <- res$values

  expect_true(all(c("Genotype", "Environment", "Direct_effect",
                    "Competition_effect", "Pure_stand_effect",
                    "Status") %in% names(v)))
  expect_setequal(unique(v$Environment), res$environments)
  expect_equal(nrow(v), length(res$genotypes) * length(res$environments))

  est <- v[v$Status == "Estimable", ]
  expect_gt(nrow(est), 0L)
  expect_equal(est$Pure_stand_effect,
               est$Direct_effect + res$k * est$Competition_effect,
               tolerance = 1e-8)
})

test_that("a genotype missing from an environment still gets a shrunken BLUP", {
  skip_without_asreml()
  # The worked example gives each environment a different panel, so some
  # genotype-by-environment cells carry no plots. They are still predicted,
  # because the effects are random and borrow from the other environments --
  # the table is a complete grid, with the effect shrunk towards zero rather
  # than a gap.
  res <- cached_met_fit()
  v <- res$values
  expect_true(all(v$Status == "Estimable"))

  observed <- unique(paste(as.character(res$data$Geno), as.character(res$data$Env)))
  absent <- v[!paste(v$Genotype, v$Environment) %in% observed, ]
  expect_gt(nrow(absent), 0L)
  # Shrunk towards zero relative to the genotypes that were actually grown.
  expect_lt(stats::sd(absent$Direct_effect),
            stats::sd(v$Direct_effect[!v$Genotype %in% absent$Genotype]))
})

test_that("the MET variance table is per environment and breeder-facing", {
  skip_without_asreml()
  res <- cached_met_fit()
  v <- res$variance
  expect_true(all(c("Environment", "Direct_variance", "Competition_variance",
                    "Pure_stand_variance") %in% names(v)))
  expect_equal(nrow(v), length(res$environments))
  expect_s3_class(plot_environment_variances(v), "ggplot")
})

test_that("a factor-analytic MET estimates between-environment correlation", {
  skip_without_asreml()
  nb <- prepared_met()
  res <- fit_met_model(nb$data, nb$names, met_options(structure = "facv", rank = 1L))

  expect_true(res$converged)
  expect_false(res$correlations_assumed)

  cors <- res$matrices$direct_cor
  expect_equal(unname(diag(cors)), rep(1, length(res$environments)),
               tolerance = 1e-6)
  off <- cors[upper.tri(cors)]
  expect_true(all(off >= -1 & off <= 1))
  # The environments share a genetic core, so they must not come out unrelated.
  expect_gt(max(abs(off)), 0.1)

  expect_false(is.null(res$fa_summary))
  expect_s3_class(plot_correlation_heatmap(cors, "Direct effects"), "ggplot")
})

test_that("a separable MET crosses one 2x2 covariance with the environments", {
  skip_without_asreml()
  nb <- prepared_met()
  res <- fit_met_model(nb$data, nb$names,
                       met_options(structure = "separable", rank = 1L))
  expect_true(res$converged)
  expect_match(res$structure_note, "[Ss]eparable")
  expect_equal(dim(res$matrices$pure),
               rep(length(res$environments), 2L))
})

test_that("separate fa() terms leave the direct-competition covariance at zero", {
  skip_without_asreml()
  nb <- prepared_met()
  res <- fit_met_model(nb$data, nb$names, met_options(structure = "fa", rank = 1L))
  expect_true(res$converged)
  # Structurally zero, not estimated: the interface must be able to say so.
  expect_true(res$dc_covariance_fixed)
  expect_true(all(abs(res$matrices$direct_competition) < 1e-10))
  expect_match(res$structure_note, "zero by construction")
})

test_that("the MET fallback ladder records what it gave up", {
  skip_without_asreml()
  nb <- prepared_met()
  # Rank 3 on four environments with a nugget is over-specified on purpose.
  res <- fit_met_model(nb$data, nb$names,
                       met_options(structure = "facv", rank = 3L, nugget = TRUE))
  expect_true(res$converged)
  expect_type(res$log, "character")
  if (res$fallback_used) expect_false(identical(res$spec$reason, "Requested model"))
})

test_that("the MET result carries everything the interface renders", {
  skip_without_asreml()
  res <- cached_met_fit()
  expect_true(all(c("fit", "values", "matrices", "variance", "varcomp",
                    "heritability", "fit_stats", "fixed_effects", "wald",
                    "description", "residuals", "log") %in% names(res)))
  expect_type(res$description, "character")
  expect_s3_class(res$residuals, "data.frame")
  expect_false(any(res$residuals$Padded))
  expect_setequal(unique(as.character(res$residuals$Env)), res$environments)
})

test_that("MET stability and scatter figures build from a real fit", {
  skip_without_asreml()
  res <- cached_met_fit()
  expect_s3_class(plot_stability(res$values, top_n = 10), "ggplot")
  expect_s3_class(plot_met_scatter(res$values, res$k), "ggplot")
})

test_that("the MET baseline comparison runs a valid likelihood-ratio test", {
  skip_without_asreml()
  nb <- prepared_met()
  res <- fit_met_model(nb$data, nb$names,
                       met_options(structure = "diag", compare_baseline = TRUE))
  expect_false(is.null(res$comparison))
  lrt <- res$comparison$lrt
  expect_true(lrt$p_value >= 0 && lrt$p_value <= 1)
  expect_gt(lrt$df, 0)
})
