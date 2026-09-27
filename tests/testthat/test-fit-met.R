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

test_that("the withdrawn MET structures are refused before anything is fitted", {
  # No ASReml needed: the check runs before the licence is touched.
  nb <- prepared_met()
  expect_error(fit_met_model(nb$data, nb$names, met_options(structure = "separable")),
               "removed in InterPlotComp 3.9.0")
  expect_error(fit_met_model(nb$data, nb$names, met_options(structure = "fa")),
               "structure = \"facv\"", fixed = TRUE)
})

test_that("the MET fallback ladder never lands on a withdrawn structure", {
  skip_without_asreml()
  nb <- prepared_met()
  res <- fit_met_model(nb$data, nb$names,
                       met_options(structure = "facv", rank = 3L, nugget = TRUE))
  expect_true(res$spec$structure %in% c("facv", "diag"))
  expect_false(any(grepl("[Ss]eparable|fa\\(\\) per effect", res$log)))
})

test_that("a per-site field trend enters the fixed model as at(Env, site) slopes", {
  skip_without_asreml()
  nb <- prepared_met()
  res <- fit_met_model(nb$data, nb$names, met_options(
    structure = "diag", field_trend = list(Env01 = "both", Env03 = "lrow")))

  expect_true(res$converged)
  fixed <- formula_text(res$fit$formulae$fixed)
  expect_match(fixed, 'at(Env,c("Env01","Env03")):lrow', fixed = TRUE)
  expect_match(fixed, 'at(Env,"Env01"):lcol', fixed = TRUE)
  # One slope per selected site, and none for the sites left alone.
  wald_terms <- res$wald$Term
  expect_true("Linear row trend (lrow), Env01" %in% wald_terms)
  expect_true("Linear row trend (lrow), Env03" %in% wald_terms)
  expect_true("Linear column trend (lcol), Env01" %in% wald_terms)
  expect_false(any(grepl("Env02|Env04", wald_terms[grepl("trend", wald_terms)])))

  expect_match(res$description, "linear row and column trends at Env01")
  expect_equal(res$field_trend$Environment, c("Env01", "Env03"))
  expect_true(all(c("lrow", "lcol") %in% names(res$data)))
})

test_that("the field trend is carried by the no-competition baseline too", {
  skip_without_asreml()
  nb <- prepared_met()
  res <- fit_met_model(nb$data, nb$names, met_options(
    structure = "diag", compare_baseline = TRUE, field_trend = c("lrow", "lcol")))
  expect_false(is.null(res$comparison))
  expect_true(all(c("at(Env):lrow", "at(Env):lcol") %in% res$field_trend_terms))
  expect_true(res$comparison$lrt$p_value >= 0 && res$comparison$lrt$p_value <= 1)
})

# A MET with one plot pushed far off its expected value.
met_with_outlier <- function() {
  raw <- sample_met_trial()
  i <- which(raw$Environment == "Env02")[10]
  raw$Yield_t_ha[i] <- raw$Yield_t_ha[i] + 6
  d <- prepare_trial_data(raw, met_map(), multi_env = TRUE)
  list(raw = raw, row = raw$Row[i], column = raw$Column[i],
       genotype = raw$Genotype[i],
       nb = add_neighbours(complete_field_grid(d), "rows"))
}

test_that("outlier detection flags the corrupted plot and leaves the fit alone", {
  skip_without_asreml()
  x <- met_with_outlier()
  res <- fit_met_model(x$nb$data, x$nb$names,
                       met_options(structure = "diag", outliers = "detect"))

  o <- res$outliers
  expect_equal(o$mode, "detect")
  expect_equal(o$threshold, 4)
  expect_match(o$method, "stdCond")
  expect_gte(o$n_detected, 1L)
  expect_equal(o$n_removed, 0L)
  top <- o$table[1, ]
  expect_equal(top$Environment, "Env02")
  expect_equal(top$Genotype, as.character(x$genotype))
  expect_equal(top$Field_row, as.character(x$row))
  expect_gt(abs(top$Std_residual), 4)
  expect_equal(top$Action, "Flagged, retained")
  # Detection only: the record is still in the fitted data.
  hit <- res$data$Env == "Env02" & res$data$Row_i == top$Row_index &
    res$data$Col_i == top$Column_index
  expect_false(is.na(res$data$Yield[hit]))
  expect_equal(o$summary$Outliers_detected[o$summary$Environment == "Env02"],
               sum(o$table$Environment == "Env02"))
  expect_true("Outlier" %in% names(res$residuals))
  expect_match(res$description, "outlier screen")
})

test_that("outlier removal refits without the flagged records and records them", {
  skip_without_asreml()
  x <- met_with_outlier()
  res <- fit_met_model(x$nb$data, x$nb$names,
                       met_options(structure = "diag", outliers = "remove"))

  o <- res$outliers
  expect_gte(o$n_removed, 1L)
  expect_equal(nrow(o$removed), o$n_removed)
  expect_true(all(o$removed$Action == "Removed before refit"))
  # The removed plots are still in the data - still competing - but have no
  # response in the refit.
  key <- paste(res$data$Env, res$data$Row_i, res$data$Col_i)
  gone <- key %in% paste(o$removed$Environment, o$removed$Row_index,
                         o$removed$Column_index)
  expect_equal(sum(gone), o$n_removed)
  expect_true(all(is.na(res$data$Yield[gone])))
  expect_false(any(paste(res$residuals$Env, res$residuals$Row, res$residuals$Column) %in%
                     key[gone]))

  expect_true(any(grepl("^Initial fit:", res$log)))
  expect_true(any(grepl("^Refit without them:", res$log)))
  expect_false(is.null(o$initial_fit_stats))
  expect_match(res$description, "removed before refitting")
  expect_equal(nrow(res$model_code$removed), o$n_removed)
})

test_that("the exported ASReml script reproduces the fitted MET", {
  skip_without_asreml()
  x <- met_with_outlier()
  res <- fit_met_model(x$nb$data, x$nb$names, met_options(
    structure = "facv", outliers = "remove",
    field_trend = list(Env01 = "both", Env03 = "lrow")))

  csv <- withr::local_tempfile(fileext = ".csv")
  utils::write.csv(x$raw, csv, row.names = FALSE)
  script <- asreml_script(res, data_file = csv, map = met_map(), axis = "rows")
  expect_match(paste(script, collapse = "\n"), "removed <- data.frame", fixed = TRUE)

  run <- new.env()
  utils::capture.output(suppressWarnings(eval(parse(text = script), envir = run)))
  expect_equal(run$fit$loglik, res$fit$loglik, tolerance = 1e-4)
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

test_that("the MET workspace passes field trend and outlier settings to the fit", {
  skip_without_asreml()
  csv <- withr::local_tempfile(fileext = ".csv")
  utils::write.csv(sample_met_trial(), csv, row.names = FALSE)
  shiny::testServer(met_server, {
    session$setInputs(file = list(datapath = csv, name = "met.csv"), header = TRUE,
                      separator = ",", env_col = "Environment",
                      yield_col = "Yield_t_ha", geno_col = "Genotype",
                      row_col = "Row", column_col = "Column", rep_col = "Rep",
                      block_col = "Block", covariate_col = "", axis = "rows",
                      structure = "diag", rank = "1", spatial = TRUE,
                      nugget = FALSE, auto_simplify = TRUE, exact_se = FALSE,
                      compare_baseline = FALSE, maxit = 25,
                      row_process = "ar1", col_process = "ar1",
                      outliers = "detect", outlier_threshold = 4)
    session$setInputs(trend_on = TRUE, trend_sites = c("Env02", "Env04"),
                      trend_site_2 = "lcol", trend_site_4 = "both")
    session$setInputs(run = 1)

    r <- result()
    expect_equal(r$field_trend$Environment, c("Env02", "Env04"))
    expect_equal(r$field_trend_terms,
                 c('at(Env, "Env04"):lrow', 'at(Env, c("Env02", "Env04")):lcol'))
    expect_equal(r$outliers$mode, "detect")
    expect_match(output$script, 'at(Env, "Env04"):lrow', fixed = TRUE)
    expect_match(output$script, '"met.csv"', fixed = TRUE)
  })
})

test_that("the MET workspace refuses a field trend with no site selected", {
  skip_without_asreml()
  csv <- withr::local_tempfile(fileext = ".csv")
  utils::write.csv(sample_met_trial(), csv, row.names = FALSE)
  shiny::testServer(met_server, {
    session$setInputs(file = list(datapath = csv, name = "met.csv"), header = TRUE,
                      separator = ",", env_col = "Environment",
                      yield_col = "Yield_t_ha", geno_col = "Genotype",
                      row_col = "Row", column_col = "Column", rep_col = "",
                      block_col = "", covariate_col = "", axis = "rows",
                      structure = "diag", maxit = 25, spatial = TRUE)
    session$setInputs(trend_on = TRUE, trend_sites = character(0))
    session$setInputs(run = 1)
    expect_false(safe_result()$ok)
    expect_match(safe_result()$message, "no site is selected")
  })
})
