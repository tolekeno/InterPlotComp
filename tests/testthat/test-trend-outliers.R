# ---------------------------------------------------------------------------
# Global field trend, outlier screening and the ASReml script: everything that
# can be checked without fitting a model. The end-to-end fits are in
# test-fit-single.R and test-fit-met.R.
# ---------------------------------------------------------------------------

envs <- c("Env01", "Env02", "Env03")

# ---- field trend ----------------------------------------------------------

test_that("normalise_field_trend treats an empty request as no adjustment", {
  for (x in list(NULL, character(0), FALSE, list())) {
    out <- normalise_field_trend(x, envs)
    expect_equal(nrow(out), 0L)
    expect_named(out, c("Environment", "lrow", "lcol"))
  }
})

test_that("an unnamed request applies the same terms to every site", {
  out <- normalise_field_trend(c("lrow", "lcol"), envs)
  expect_equal(out$Environment, envs)
  expect_true(all(out$lrow) && all(out$lcol))

  out <- normalise_field_trend("lcol", envs)
  expect_false(any(out$lrow))
  expect_true(all(out$lcol))

  expect_equal(normalise_field_trend("both", envs), normalise_field_trend(c("lrow", "lcol"), envs))
})

test_that("a per-site request keeps only the sites and terms asked for", {
  out <- normalise_field_trend(list(Env03 = "lrow", Env01 = "both", Env02 = "none"), envs)
  # Returned in the order of the data, not of the request; "none" drops out.
  expect_equal(out$Environment, c("Env01", "Env03"))
  expect_equal(out$lrow, c(TRUE, TRUE))
  expect_equal(out$lcol, c(TRUE, FALSE))

  vec <- normalise_field_trend(c(Env02 = "lcol"), envs)
  expect_equal(vec$Environment, "Env02")
  expect_equal(vec$lcol, TRUE)
})

test_that("the canonical table form round-trips", {
  x <- normalise_field_trend(list(Env01 = "both", Env03 = "lcol"), envs)
  expect_equal(normalise_field_trend(x, envs), x)
})

test_that("unknown sites and terms are refused with the valid choices", {
  expect_error(normalise_field_trend(list(Nowhere = "lrow"), envs),
               "not in the data.*Env01, Env02, Env03")
  expect_error(normalise_field_trend(list(Env01 = "quadratic"), envs),
               "Unknown field-trend term")
  expect_error(normalise_field_trend(data.frame(lrow = TRUE), envs),
               "Environment")
})

test_that("field_trend_fixed_terms writes plain terms for a single trial", {
  tr <- normalise_field_trend(c("lrow", "lcol"), "Trial")
  expect_equal(field_trend_fixed_terms(tr, "Trial", multi_env = FALSE),
               c("lrow", "lcol"))
  expect_length(field_trend_fixed_terms(tr[0, ], "Trial", FALSE), 0L)
})

test_that("field_trend_fixed_terms gives each MET site its own slope", {
  all_sites <- normalise_field_trend("lrow", envs)
  expect_equal(field_trend_fixed_terms(all_sites, envs), "at(Env):lrow")

  some <- normalise_field_trend(list(Env01 = "both", Env03 = "lrow"), envs)
  expect_equal(field_trend_fixed_terms(some, envs),
               c('at(Env, c("Env01", "Env03")):lrow', 'at(Env, "Env01"):lcol'))
})

test_that("site names that need quoting still give a parseable formula", {
  odd <- c('Site "A"', "Site B's", "C")
  tr <- normalise_field_trend(stats::setNames(list("lrow", "lcol"), odd[1:2]), odd)
  terms <- field_trend_fixed_terms(tr, odd)
  f <- stats::as.formula(paste("Yield ~ Env +", paste(terms, collapse = " + ")))
  expect_s3_class(f, "formula")
  expect_true(any(grepl("Site \\\"A\\\"", terms, fixed = TRUE)))
})

test_that("add_trend_covariates centres each site on its own grid", {
  d <- data.frame(Env = factor(rep(c("A", "B"), c(6, 12))),
                  Row_i = c(rep(1:3, 2), rep(1:4, 3)),
                  Col_i = c(rep(1:2, each = 3), rep(1:3, each = 4)))
  out <- add_trend_covariates(d)
  expect_equal(out$lrow[out$Env == "A"], rep(c(-1, 0, 1), 2))
  expect_equal(out$lcol[out$Env == "A"], rep(c(-0.5, 0.5), each = 3))
  expect_equal(unique(out$lrow[out$Env == "B"]), c(-1.5, -0.5, 0.5, 1.5))
  expect_equal(as.numeric(tapply(out$lcol, out$Env, mean)), c(0, 0))
})

test_that("describe_field_trend groups sites that share an adjustment", {
  tr <- normalise_field_trend(list(Env01 = "both", Env02 = "both", Env03 = "lrow"), envs)
  expect_equal(describe_field_trend(tr),
               "linear row and column trends at Env01, Env02; linear row trend at Env03")
  expect_equal(describe_field_trend(tr[0, ]), "")
  expect_match(describe_field_trend(normalise_field_trend("lcol", "Trial"), FALSE),
               "linear column trend")
})

test_that("field_trend_table reports the fitted terms for each site", {
  tr <- normalise_field_trend(list(Env01 = "both", Env03 = "lrow"), envs)
  tab <- field_trend_table(tr, envs)
  expect_equal(tab$Linear_column_lcol, c("Yes", "No"))
  expect_match(tab$Fixed_terms[1], "lcol")
  expect_false(grepl("lcol", tab$Fixed_terms[2]))
  expect_null(field_trend_table(tr[0, ], envs))
})

test_that("pretty_fixed_term names the per-site slopes", {
  expect_equal(pretty_fixed_term("at(Env, 'Env01'):lrow"), "Linear row trend (lrow), Env01")
  expect_equal(pretty_fixed_term("at(Env, 'Env 2'):lcol"), "Linear column trend (lcol), Env 2")
  expect_equal(pretty_fixed_term("lrow"), "Linear row trend (lrow)")
})

# ---- outlier screening ------------------------------------------------------

test_that("outlier_settings validates the mode and threshold", {
  expect_equal(outlier_settings(list()), list(mode = "none", threshold = 4))
  expect_equal(outlier_settings(list(outliers = TRUE))$mode, "detect")
  expect_equal(outlier_settings(list(outliers = "remove", outlier_threshold = "3.5"))$threshold, 3.5)
  expect_error(outlier_settings(list(outliers = "delete")), "Unknown outlier option")
  expect_error(outlier_settings(list(outlier_threshold = 1)), "at least 2")
  expect_error(outlier_settings(list(outlier_threshold = NA)), "at least 2")
})

# A small analysis grid with two sites.
toy_data <- function() {
  d <- expand.grid(Row_i = 1:4, Col_i = 1:3, Env = c("A", "B"),
                   KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  d$Env <- factor(d$Env)
  d$Geno <- factor(sprintf("G%02d", seq_len(nrow(d)) %% 7 + 1))
  d$Row_label <- as.character(d$Row_i + 100)
  d$Column_label <- as.character(d$Col_i)
  d$Yield <- 10
  d$Padded <- FALSE
  d
}

test_that("outlier_table flags only observed plots beyond the threshold", {
  d <- toy_data()
  std <- rep(0.5, nrow(d))
  std[3] <- -4.5
  std[15] <- 6
  std[20] <- 4          # at the threshold, not beyond it
  std[7] <- 9; d$Yield[7] <- NA          # no response
  std[9] <- 9; d$Padded[9] <- TRUE       # padded grid position
  tab <- outlier_table(d, rep(10, nrow(d)), std * 0.2, std, threshold = 4)

  expect_equal(nrow(tab), 2L)
  expect_equal(tab$Std_residual, c(6, -4.5))            # largest first
  expect_equal(tab$Environment, c("B", "A"))
  expect_equal(tab$Field_row[2], d$Row_label[3])
  expect_equal(tab$Row_index[2], d$Row_i[3])
  expect_named(tab, c("Environment", "Genotype", "Field_row", "Field_column",
                      "Row_index", "Column_index", "Observed", "Fitted",
                      "Residual", "Std_residual"))
})

test_that("drop_outlier_records withdraws the response but keeps the plot", {
  d <- toy_data()
  flagged <- data.frame(Environment = "B", Row_index = 2L, Column_index = 3L)
  out <- drop_outlier_records(d, flagged)
  hit <- out$Env == "B" & out$Row_i == 2 & out$Col_i == 3
  expect_equal(nrow(out), nrow(d))
  expect_true(is.na(out$Yield[hit]))
  expect_equal(sum(is.na(out$Yield)), 1L)
  expect_equal(attr(out, "outliers_removed"), 1L)
  # Same position in the other site is untouched.
  expect_false(is.na(out$Yield[out$Env == "A" & out$Row_i == 2 & out$Col_i == 3]))
  expect_identical(drop_outlier_records(d, flagged[0, ]), d)
})

# A stand-in for a fitting function: flags any yield above 50 as an outlier,
# so the screening workflow can be tested without ASReml.
fake_fit <- function(calls) {
  function(data, opts) {
    calls$n <- calls$n + 1L
    calls$data[[calls$n]] <- data
    settings <- outlier_settings(opts)
    std <- ifelse(!is.na(data$Yield) & data$Yield > 50, 8, 0.3)
    tab <- outlier_table(data, rep(10, nrow(data)), std, std, settings$threshold)
    tab$Action <- rep(if (settings$mode == "remove") "Removed before refit"
                      else "Flagged, retained", nrow(tab))
    list(description = "a model", log = sprintf("pass %d", calls$n),
         fit_stats = data.frame(LogLik = -calls$n),
         outliers = if (settings$mode == "none") NULL else list(
           mode = settings$mode, threshold = settings$threshold,
           method = "fake", std = std, table = tab),
         model_code = list(removed = NULL))
  }
}

test_that("screening off fits once and adds nothing", {
  calls <- new.env(); calls$n <- 0L; calls$data <- list()
  d <- toy_data(); d$Yield[5] <- 99
  res <- with_outlier_screening(d, list(outliers = "none"), fake_fit(calls))
  expect_equal(calls$n, 1L)
  expect_null(res$outliers)
  expect_equal(res$description, "a model")
})

test_that("detect mode fits once, reports and keeps the data", {
  calls <- new.env(); calls$n <- 0L; calls$data <- list()
  d <- toy_data(); d$Yield[5] <- 99
  res <- with_outlier_screening(d, list(outliers = "detect"), fake_fit(calls))
  expect_equal(calls$n, 1L)
  expect_equal(res$outliers$n_detected, 1L)
  expect_equal(res$outliers$n_removed, 0L)
  expect_equal(nrow(res$outliers$removed), 0L)
  expect_equal(res$outliers$summary$Outliers_detected, c(1L, 0L))
  expect_match(res$description, "1 flagged")
})

test_that("remove mode refits once on data without the flagged responses", {
  calls <- new.env(); calls$n <- 0L; calls$data <- list()
  d <- toy_data(); d$Yield[c(5, 20)] <- 99
  res <- with_outlier_screening(d, list(outliers = "remove"), fake_fit(calls))

  expect_equal(calls$n, 2L)
  refit_data <- calls$data[[2]]
  expect_true(all(is.na(refit_data$Yield[c(5, 20)])))
  expect_equal(sum(is.na(refit_data$Yield)), 2L)

  o <- res$outliers
  expect_equal(o$n_detected, 2L)
  expect_equal(o$n_removed, 2L)
  expect_equal(o$n_remaining, 0L)
  expect_equal(o$summary$Removed, c(1L, 1L))
  expect_equal(o$initial_fit_stats$LogLik, -1)
  expect_equal(res$log, c("Initial fit:", "  pass 1",
                          "Outlier screen: 2 observation(s) with |standardised residual| > 4 removed.",
                          "Refit without them:", "  pass 2"))
  expect_equal(nrow(res$model_code$removed), 2L)
  expect_match(res$description, "2 outlier\\(s\\) .* removed before refitting")
})

test_that("remove mode with nothing to remove does not refit", {
  calls <- new.env(); calls$n <- 0L; calls$data <- list()
  res <- with_outlier_screening(toy_data(), list(outliers = "remove"), fake_fit(calls))
  expect_equal(calls$n, 1L)
  expect_equal(res$outliers$n_detected, 0L)
  expect_equal(res$outliers$n_removed, 0L)
})

test_that("a lower threshold is passed through to the screen", {
  calls <- new.env(); calls$n <- 0L; calls$data <- list()
  res <- with_outlier_screening(toy_data(),
                                list(outliers = "detect", outlier_threshold = 2.5),
                                fake_fit(calls))
  expect_equal(res$outliers$threshold, 2.5)
})

# ---- retired MET structures ---------------------------------------------------

test_that("only the joint and diagonal MET structures are offered", {
  expect_setequal(unname(MET_STRUCTURES), c("facv", "diag"))
  html <- paste(as.character(htmltools::renderTags(met_ui("m"))$html), collapse = "\n")
  expect_false(grepl("Separable us(2)", html, fixed = TRUE))
  expect_false(grepl("Separate fa()", html, fixed = TRUE))
})

test_that("check_met_structure refuses the withdrawn structures by name", {
  expect_equal(check_met_structure("facv"), "facv")
  expect_equal(check_met_structure(NULL), "facv")
  expect_error(check_met_structure("separable"), "Separable us\\(2\\) x FA.*removed")
  expect_error(check_met_structure("fa"), "Separate fa\\(\\) per effect.*removed")
  expect_error(check_met_structure("nonsense"), "Unknown MET structure")
})

test_that("the MET fallback ladder no longer steps through withdrawn structures", {
  specs <- met_specifications("facv", 3L, TRUE, TRUE, c("RepF", "BlockF"),
                              row_process = "ar2")
  used <- vapply(specs, `[[`, character(1), "structure")
  expect_true(all(used %in% c("facv", "diag")))
  expect_equal(specs[[1]]$reason, "Requested model")
})

test_that("met_formulae rejects the withdrawn structures", {
  expect_error(met_formulae("RepF", c("N1", "N2"), 40, 4, structure = "separable"),
               "Unknown MET structure")
  expect_error(met_formulae("RepF", c("N1", "N2"), 40, 4, structure = "fa"),
               "Unknown MET structure")
})

# ---- ASReml script ------------------------------------------------------------

fake_result <- function(multi_env = TRUE, trend = NULL, removed = NULL,
                        kinship = NULL, spatial = TRUE) {
  env <- if (multi_env) envs else "Trial"
  trend <- normalise_field_trend(trend, env)
  f <- if (multi_env) {
    met_formulae("RepF", c("N1", "N2"), 40, 3, "facv", 1L, spatial, FALSE)
  } else {
    single_formulae("RepF", c("N1", "N2"), 40, "us", spatial, FALSE)
  }
  fixed <- paste(c(if (multi_env) "Yield ~ Env" else "Yield ~ 1",
                   field_trend_fixed_terms(trend, env, multi_env)), collapse = " + ")
  code <- model_code_record(fixed, f, c("N1", "N2"),
                            list(maxit = 50, workspace = "1gb"),
                            multi_env, spatial, kinship, trend)
  code$removed <- removed
  list(description = "a test model", model_code = code)
}

test_that("asreml_script writes a parseable script with the fitted formulae", {
  res <- fake_result(trend = list(Env01 = "both"))
  script <- asreml_script(res, "trial.csv", met_map(), axis = "columns", sep = ";")
  expect_silent(parse(text = script))
  txt <- paste(script, collapse = "\n")
  expect_match(txt, 'read.csv("trial.csv", header = TRUE, sep = ";"', fixed = TRUE)
  expect_match(txt, 'add_neighbours(d, "columns")', fixed = TRUE)
  expect_match(txt, "multi_env = TRUE", fixed = TRUE)
  expect_match(txt, 'fixed = Yield ~ Env + at(Env, "Env01"):lrow + at(Env, "Env01"):lcol',
               fixed = TRUE)
  expect_match(txt, "facv(EffectEnv, 1)", fixed = TRUE)
  expect_match(txt, "d$lrow <-", fixed = TRUE)
  expect_match(txt, 'equate.levels = c("Geno", "N1", "N2")', fixed = TRUE)
  expect_match(txt, "maxit = 50", fixed = TRUE)
  expect_false(grepl("removed <-", txt, fixed = TRUE))
})

test_that("asreml_script records the removed outliers", {
  removed <- data.frame(Environment = c("Env01", "Env03"), Row_index = c(4L, 9L),
                        Column_index = c(2L, 1L))
  script <- asreml_script(fake_result(removed = removed))
  expect_silent(parse(text = script))
  txt <- paste(script, collapse = "\n")
  expect_match(txt, "2 observation(s) removed as outliers", fixed = TRUE)
  expect_match(txt, 'Env = c("Env01", "Env03")', fixed = TRUE)
  expect_match(txt, "Row_i = c(4, 9)", fixed = TRUE)
})

test_that("asreml_script handles a single trial from a multi-site file", {
  script <- asreml_script(fake_result(multi_env = FALSE, trend = "lrow"),
                          "all_sites.csv", single_map(),
                          site = list(column = "Site", value = "North"))
  expect_silent(parse(text = script))
  txt <- paste(script, collapse = "\n")
  expect_match(txt, 'raw[["Site"]])) == "North"', fixed = TRUE)
  expect_false(grepl("multi_env", txt, fixed = TRUE))
  expect_false(grepl("EffectEnv <-", txt, fixed = TRUE))
  expect_match(txt, "fixed = Yield ~ 1 + lrow", fixed = TRUE)
})

test_that("asreml_script leaves editable placeholders when details are unknown", {
  script <- asreml_script(fake_result(kinship = list(label = "pedigree relationship")))
  expect_silent(parse(text = script))
  txt <- paste(script, collapse = "\n")
  expect_match(txt, "<path to your data file>", fixed = TRUE)
  expect_match(txt, "<environment>", fixed = TRUE)
  expect_match(txt, ".kinship <- rel$ginv", fixed = TRUE)
  expect_match(txt, "pedigree relationship", fixed = TRUE)
})

test_that("asreml_script writes to a file when asked", {
  f <- withr::local_tempfile(fileext = ".R")
  out <- asreml_script(fake_result(), file = f)
  expect_equal(readLines(f), out)
})

test_that("asreml_script refuses a result from before 3.9.0", {
  expect_error(asreml_script(list(description = "old")), "no model code")
})

# ---- interface ------------------------------------------------------------------

test_that("both workspaces offer the field-trend and outlier controls", {
  for (f in list(single_ui, met_ui)) {
    html <- paste(as.character(htmltools::renderTags(f("w"))$html), collapse = "\n")
    expect_match(html, "Global field trend", fixed = TRUE)
    expect_match(html, "w-outliers", fixed = TRUE)
    expect_match(html, "w-outlier_threshold", fixed = TRUE)
    expect_match(html, "w-script", fixed = TRUE)
    expect_match(html, "w-outlier_table", fixed = TRUE)
  }
})

test_that("the MET workspace builds a per-site field-trend request from its inputs", {
  csv <- withr::local_tempfile(fileext = ".csv")
  utils::write.csv(sample_met_trial(), csv, row.names = FALSE)
  shiny::testServer(met_server, {
    session$setInputs(file = list(datapath = csv, name = "met.csv"), header = TRUE,
                      separator = ",", env_col = "Environment",
                      yield_col = "Yield_t_ha", geno_col = "Genotype",
                      row_col = "Row", column_col = "Column", rep_col = "Rep",
                      block_col = "Block", covariate_col = "")
    expect_equal(env_levels(), c("Env01", "Env02", "Env03", "Env04"))

    session$setInputs(trend_on = FALSE)
    expect_null(trend_spec())

    session$setInputs(trend_on = TRUE, trend_sites = c("Env01", "Env03"),
                      trend_site_1 = "lrow", trend_site_3 = "both")
    expect_equal(trend_spec(), list(Env01 = "lrow", Env03 = "both"))
    tr <- normalise_field_trend(trend_spec(), env_levels())
    expect_equal(field_trend_fixed_terms(tr, env_levels()),
                 c('at(Env, c("Env01", "Env03")):lrow', 'at(Env, "Env03"):lcol'))

    # A site with no term chosen yet defaults to both terms.
    session$setInputs(trend_sites = c("Env02"))
    expect_equal(trend_spec(), list(Env02 = "both"))

    session$setInputs(trend_sites = character(0))
    expect_null(trend_spec())
  })
})
