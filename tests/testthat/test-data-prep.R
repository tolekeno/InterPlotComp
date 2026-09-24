test_that("prepare_trial_data reshapes a worked example into the analysis layout", {
  d <- prepare_trial_data(sample_single_trial(), single_map())

  expect_s3_class(d, "data.frame")
  expect_true(all(c("Yield", "Geno", "Env", "Row", "Column", "Row_i", "Col_i",
                    "Padded") %in% names(d)))
  expect_s3_class(d$Geno, "factor")
  expect_s3_class(d$Env, "factor")
  expect_equal(levels(d$Env), "Trial")          # single trials get one pseudo-env
  expect_false(any(d$Padded))
  expect_false(is.null(attr(d, "field_summary")))
})

test_that("prepare_trial_data re-indexes coordinates to a contiguous 1..n grid", {
  raw <- data.frame(
    Y = stats::rnorm(20, 8), G = rep(sprintf("G%d", 1:5), 4),
    R = rep(c(10, 20, 30, 40), each = 5),   # non-contiguous labels
    C = rep(1:5, 4)
  )
  d <- prepare_trial_data(raw, list(yield = "Y", geno = "G", row = "R", column = "C"))
  expect_equal(sort(unique(d$Row_i)), 1:4)
  expect_equal(sort(unique(d$Col_i)), 1:5)
})

test_that("prepare_trial_data rejects incomplete column mappings", {
  raw <- sample_single_trial()
  expect_error(
    prepare_trial_data(raw, list(geno = "Genotype", row = "Row", column = "Column")),
    "Response"
  )
  expect_error(
    prepare_trial_data(raw, single_map(yield = "No_Such_Column")),
    "not in the file"
  )
})

test_that("prepare_trial_data rejects data that cannot support a competition model", {
  # A non-numeric response.
  raw <- data.frame(Y = letters[1:20], G = rep(sprintf("G%d", 1:5), 4),
                    R = rep(1:4, each = 5), C = rep(1:5, 4))
  expect_error(prepare_trial_data(raw, list(yield = "Y", geno = "G", row = "R",
                                            column = "C")),
               "no numeric values")

  # Too few observed plots.
  raw2 <- data.frame(Y = c(stats::rnorm(5), rep(NA, 15)),
                     G = rep(sprintf("G%d", 1:5), 4),
                     R = rep(1:4, each = 5), C = rep(1:5, 4))
  expect_error(prepare_trial_data(raw2, list(yield = "Y", geno = "G", row = "R",
                                             column = "C")),
               "At least 10 observed plots")

  # A single genotype is a trial, not a panel.
  raw3 <- data.frame(Y = stats::rnorm(20, 8), G = "OnlyOne",
                     R = rep(1:4, each = 5), C = rep(1:5, 4))
  expect_error(prepare_trial_data(raw3, list(yield = "Y", geno = "G", row = "R",
                                             column = "C")),
               "genotype panel")
})

test_that("prepare_trial_data rejects two plots at one field position", {
  raw <- data.frame(Y = stats::rnorm(20, 8), G = rep(sprintf("G%d", 1:5), 4),
                    R = rep(1L, 20), C = rep(1L, 20))
  expect_error(
    prepare_trial_data(raw, list(yield = "Y", geno = "G", row = "R", column = "C")),
    "share a field position"
  )
})

test_that("prepare_trial_data requires a genotype for every observed plot", {
  raw <- sample_single_trial()
  raw$Genotype[which(!is.na(raw$Yield_t_ha))[1]] <- NA
  expect_error(prepare_trial_data(raw, single_map()), "no genotype")
})

test_that("the multi-environment workspace insists on more than one environment", {
  raw <- sample_single_trial()
  raw$Environment <- "OnlySite"
  expect_error(prepare_trial_data(raw, met_map(), multi_env = TRUE),
               "at least two environments")
})

test_that("a covariate is centred and its mean retained", {
  d <- prepare_trial_data(sample_single_trial(),
                          single_map(covariate = "Plant_height_cm"))
  expect_true(all(c("Covariate", "Covariate_c") %in% names(d)))
  expect_equal(mean(d$Covariate_c, na.rm = TRUE), 0, tolerance = 1e-8)
  expect_equal(attr(d, "covariate_mean"), mean(d$Covariate, na.rm = TRUE))
  expect_equal(attr(d, "covariate_name"), "Plant_height_cm")
})

test_that("a mostly-absent covariate is refused rather than silently used", {
  raw <- sample_single_trial()
  raw$Plant_height_cm[seq_len(floor(nrow(raw) * 0.6))] <- NA
  expect_error(prepare_trial_data(raw, single_map(covariate = "Plant_height_cm")),
               "missing for")
})

test_that("complete_field_grid pads each environment to a full rectangle", {
  d <- prepare_trial_data(sample_single_trial(), single_map())
  padded <- complete_field_grid(d)

  expect_gt(nrow(padded), nrow(d))
  expect_equal(nrow(padded), max(padded$Row_i) * max(padded$Col_i))
  expect_equal(sum(padded$Padded), attr(padded, "n_padded"))

  # Padded records define the residual layout only: no response, no genotype.
  expect_true(all(is.na(padded$Yield[padded$Padded])))
  expect_true(all(is.na(padded$Geno[padded$Padded])))
  # Real records survive untouched.
  expect_equal(sum(!padded$Padded), nrow(d))
})

test_that("complete_field_grid pads a MET environment by environment", {
  d <- prepare_trial_data(sample_met_trial(), met_map(), multi_env = TRUE)
  padded <- complete_field_grid(d)
  per_env <- tapply(seq_len(nrow(padded)), padded$Env, function(i) {
    z <- padded[i, ]
    nrow(z) == max(z$Row_i) * max(z$Col_i)
  })
  expect_true(all(per_env))
  expect_equal(levels(padded$Env), levels(d$Env))
})

test_that("add_neighbours attaches the genotype growing in each adjacent plot", {
  nb <- prepared_single()
  expect_equal(nb$k, 2L)
  expect_equal(nb$names, c("N1", "N2"))
  expect_true(all(nb$names %in% names(nb$data)))
  expect_equal(levels(nb$data$N1), levels(nb$data$Geno))

  # Verify one neighbour by hand against the field key.
  d <- nb$data
  key <- paste(as.integer(d$Env), d$Row_i, d$Col_i, sep = "/")
  expect_equal(
    as.character(d$N1),
    as.character(d$Geno[match(paste(as.integer(d$Env), d$Row_i - 1L, d$Col_i,
                                    sep = "/"), key)])
  )
})

test_that("add_neighbours leaves border plots with fewer neighbours", {
  nb <- prepared_single()
  d <- nb$data
  border <- d$Row_i == 1L
  expect_true(all(is.na(d$N1[border])))
  expect_true(all(d$Neighbour_count <= nb$k))
  expect_true(any(d$Neighbour_count < nb$k))
})

test_that("the competition axis selects the neighbour offsets", {
  expect_equal(prepared_single(axis = "rows")$k, 2L)
  expect_equal(prepared_single(axis = "columns")$k, 2L)
  expect_equal(prepared_single(axis = "four")$k, 4L)
  expect_error(add_neighbours(prepare_trial_data(sample_single_trial(),
                                                 single_map()), "diagonal"),
               "Unknown competition direction")
})

test_that("neighbour covariates sum over exactly the modelled neighbours", {
  d <- prepare_trial_data(sample_single_trial(),
                          single_map(covariate = "Plant_height_cm"))
  nb <- add_neighbours(complete_field_grid(d), "rows")
  expect_true(all(c("Covariate_nb", "Covariate_own") %in% names(nb$data)))
  # An absent neighbour contributes zero, i.e. "an average neighbour".
  expect_false(anyNA(nb$data$Covariate_nb))
  expect_false(anyNA(nb$data$Covariate_own))
})

test_that("design factors are nested and degenerate ones dropped", {
  d <- prepare_trial_data(sample_single_trial(), single_map())
  expect_true("RepF" %in% names(d))
  expect_setequal(available_design_terms(d), intersect(c("RepF", "BlockF"), names(d)))

  # A block factor with one level per plot carries no information.
  raw <- sample_single_trial()
  raw$Block <- seq_len(nrow(raw))
  d2 <- prepare_trial_data(raw, single_map())
  expect_false("BlockF" %in% names(d2))
})

test_that("MET design factors are nested within environment", {
  d <- prepare_trial_data(sample_met_trial(), met_map(), multi_env = TRUE)
  # Rep "1" in two environments must not collapse into one level.
  expect_gt(nlevels(d$RepF), length(unique(sample_met_trial()$Rep)))
  expect_true(all(grepl(":", levels(d$RepF), fixed = TRUE)))
})

test_that("field_summary reports one row per environment with honest counts", {
  d <- prepare_trial_data(sample_met_trial(), met_map(), multi_env = TRUE)
  fs <- attr(d, "field_summary")
  expect_equal(nrow(fs), nlevels(d$Env))
  expect_setequal(fs$Environment, levels(d$Env))
  expect_equal(sum(fs$Plots), nrow(d))
  expect_equal(sum(fs$Observed), sum(!is.na(d$Yield)))
  expect_true(all(fs$Rows > 0 & fs$Columns > 0))
})

test_that("competition diagnostics and warnings describe the layout", {
  nb <- prepared_single()
  x <- competition_diagnostics(nb$data, nb$names)
  expect_equal(x$k, 2L)
  expect_gt(x$n_observed, 0)
  expect_true(x$full_neighbour_pct >= 0 && x$full_neighbour_pct <= 100)
  expect_type(competition_warnings(x), "character")

  # A layout that genuinely deserves a warning produces one.
  bad <- list(full_neighbour_pct = 10, mean_distinct_neighbours = 1.1,
              n_genotypes = 5, k = 2)
  expect_length(competition_warnings(bad), 3L)
})
