test_that("genetic_term_pattern matches a term with and without vm()", {
  # Regression guard: a diag model fitted with a relationship matrix names its
  # parameters vm(Geno, .kinship), and anchoring on the bare name silently
  # found no variance at all.
  p <- genetic_term_pattern("Geno")
  expect_true(grepl(p, "Geno"))
  expect_true(grepl(p, "Geno!var"))
  expect_true(grepl(p, "vm(Geno, .kinship)"))
  expect_true(grepl(p, "vm(Geno, .kinship)!var"))

  expect_false(grepl(p, "Env:Geno"))
  expect_false(grepl(p, "GenoSomethingElse"))
  expect_false(grepl(genetic_term_pattern("N1"), "vm(Geno, .kinship)"))
  expect_true(grepl(genetic_term_pattern("N1"), "vm(N1, .kinship)"))
})

test_that("partition_genetic_covariance splits the joint matrix correctly", {
  # One environment: each block is 1 x 1 and the algebra is checkable by hand.
  G <- matrix(c(0.36, -0.10, -0.10, 0.09), 2, 2)
  k <- 2
  p <- partition_genetic_covariance(G, k, labels = "Trial")

  expect_equal(p$direct[1, 1], 0.36)
  expect_equal(p$competition[1, 1], 0.09)
  expect_equal(p$direct_competition[1, 1], -0.10)
  # Pure stand: G_D + k^2 G_C + k (G_DC + G_DC')
  expect_equal(p$pure[1, 1], 0.36 + 4 * 0.09 + 2 * (-0.10 - 0.10))
  expect_equal(p$k, k)
  expect_equal(rownames(p$direct), "Trial")
})

test_that("partition_genetic_covariance handles several environments", {
  e <- 3L
  Gd <- diag(c(0.3, 0.4, 0.5))
  Gc <- diag(c(0.05, 0.06, 0.07))
  Gdc <- diag(c(-0.02, -0.03, -0.04))
  G <- rbind(cbind(Gd, Gdc), cbind(t(Gdc), Gc))

  p <- partition_genetic_covariance(G, k = 2, labels = sprintf("E%d", 1:e))
  expect_equal(dim(p$direct), c(e, e))
  expect_equal(unname(diag(p$direct)), c(0.3, 0.4, 0.5))
  expect_equal(unname(diag(p$competition)), c(0.05, 0.06, 0.07))
  expect_equal(unname(diag(p$pure)), c(0.3, 0.4, 0.5) + 4 * c(0.05, 0.06, 0.07) +
                 2 * 2 * c(-0.02, -0.03, -0.04))
  expect_equal(unname(diag(p$direct_cor)), rep(1, e))
})

test_that("partition_genetic_covariance insists on an even dimension", {
  expect_error(partition_genetic_covariance(diag(3), k = 2), "even dimension")
})

test_that("the pure-stand matrix is returned positive semi-definite", {
  # A strong negative covariance can push the derived matrix slightly negative
  # through rounding; it must still be usable as a covariance.
  G <- matrix(c(0.36, -0.18, -0.18, 0.09), 2, 2)
  p <- partition_genetic_covariance(G, k = 2)
  expect_gte(min(eigen(p$pure, symmetric = TRUE, only.values = TRUE)$values), -1e-8)
})

test_that("reliability is bounded and guards a degenerate variance", {
  expect_equal(reliability(c(0.1, 0.2), 0.4), c(0.75, 0.5))
  expect_equal(reliability(1.0, 0.4), 0)      # clamped at zero, not negative
  expect_equal(reliability(0.0, 0.4), 1)
  expect_true(all(is.na(reliability(c(0.1, 0.2), 0))))
  expect_true(all(is.na(reliability(0.1, NA_real_))))
})

test_that("cullis_h2 averages the PEV and stays inside [0, 1]", {
  expect_equal(cullis_h2(c(0.1, 0.3), 0.4), 1 - 0.2 / 0.4)
  expect_equal(cullis_h2(c(1, 2), 0.4), 0)
  expect_true(is.na(cullis_h2(numeric(0), 0.4)))
  expect_true(is.na(cullis_h2(c(0.1, 0.2), -1)))
  expect_equal(cullis_h2(c(0.1, NA, 0.3), 0.4), 1 - 0.2 / 0.4)
})

test_that("significance_stars follows the conventional thresholds", {
  p <- c(0.0001, 0.005, 0.03, 0.08, 0.5, NA)
  expect_equal(significance_stars(p),
               c("***", "**", "*", ".", "n.s.", ""))
})

test_that("describe_residual names the fitted spatial process", {
  expect_equal(describe_residual("id", "id"), "independent residuals")
  expect_match(describe_residual("ar1", "ar1"), "AR1 x AR1")
  expect_equal(spatial_residual_text("ar1", "ar1"), "ar1(Column):ar1(Row)")
  expect_equal(spatial_residual_text("ar2", "ar1"), "ar1(Column):ar2(Row)")
})

test_that("the single-trial specification ladder is nested and free of repeats", {
  specs <- single_specifications("us", spatial = TRUE, nugget = TRUE,
                                 design_terms = c("RepF", "BlockF"),
                                 allow_fallback = TRUE)
  expect_gt(length(specs), 1L)
  expect_equal(specs[[1]]$reason, "Requested model")
  expect_equal(specs[[1]]$structure, "us")

  keys <- vapply(specs, function(s) paste(s$structure, s$spatial, s$nugget,
                                          paste(s$design_terms, collapse = "+"),
                                          s$row_process, s$col_process),
                 character(1))
  expect_false(anyDuplicated(keys) > 0)

  # The spatial residual is the last assumption given up.
  expect_true(specs[[length(specs)]]$spatial == FALSE)
  # Every step after the first carries a stated reason.
  expect_false(any(vapply(specs, function(s) is.null(s$reason), logical(1))))
})

test_that("the ladder collapses to a single step when fallback is off", {
  specs <- single_specifications("us", TRUE, TRUE, "RepF", allow_fallback = FALSE)
  expect_length(specs, 1L)
})

test_that("a second-order residual process is simplified before anything else", {
  specs <- single_specifications("us", spatial = TRUE, nugget = TRUE,
                                 design_terms = "RepF", allow_fallback = TRUE,
                                 row_process = "ar2", col_process = "ar1")
  expect_equal(specs[[1]]$row_process, "ar2")
  expect_equal(specs[[2]]$row_process, "ar1")
  expect_match(specs[[2]]$reason, "Simplified the residual process")
  # Once simplified, no later step may reinstate the richer process.
  expect_true(all(vapply(specs[-1], function(s) s$row_process, character(1)) == "ar1"))
})

test_that("single_formulae writes the expected model text", {
  f <- single_formulae(c("RepF", "BlockF"), c("N1", "N2"), n_geno = 60,
                       structure = "us", spatial = TRUE, nugget = TRUE)
  txt <- formula_text(f$random)
  expect_match(txt, "RepF")
  expect_match(txt, "str(", fixed = TRUE)
  expect_match(txt, "us(2):id(60)", fixed = TRUE)
  expect_match(txt, "and(N2)", fixed = TRUE)
  expect_match(txt, "idv(units)", fixed = TRUE)
  expect_equal(formula_text(f$residual), "~ar1(Column):ar1(Row)")
})

test_that("single_formulae wraps every genetic factor when a relationship is used", {
  # The trap this guards: leaving id(n) in the str() variance formula is
  # accepted by ASReml and silently returns the independent-genotype answer.
  f <- single_formulae("RepF", c("N1", "N2"), n_geno = 60, structure = "us",
                       spatial = TRUE, nugget = FALSE, kinship = TRUE)
  txt <- formula_text(f$random)
  expect_match(txt, "vm(Geno,.kinship)", fixed = TRUE)
  expect_match(txt, "vm(N1,.kinship)", fixed = TRUE)
  expect_match(txt, "and(vm(N2,.kinship))", fixed = TRUE)
  expect_match(txt, "us(2):vm(Geno,.kinship)", fixed = TRUE)
  expect_false(grepl("id(60)", txt, fixed = TRUE))
})

test_that("a diag model drops the str() block entirely", {
  f <- single_formulae("RepF", c("N1", "N2"), 60, structure = "diag",
                       spatial = FALSE, nugget = FALSE)
  txt <- formula_text(f$random)
  expect_false(grepl("str(", txt, fixed = TRUE))
  expect_match(txt, "Geno")
  expect_match(txt, "and(N2)", fixed = TRUE)
  expect_equal(formula_text(f$residual), "~idv(units)")
})

test_that("a no-competition model carries only the direct effects", {
  f <- single_formulae("RepF", c("N1", "N2"), 60, "us", TRUE, FALSE,
                       competition = FALSE)
  txt <- formula_text(f$random)
  expect_false(grepl("N1", txt, fixed = TRUE))
  expect_match(txt, "Geno")
})

test_that("met_formulae builds the factor-analytic structures", {
  f <- met_formulae("RepF", c("N1", "N2"), n_geno = 40, n_env = 4,
                    structure = "facv", rank = 2, spatial = TRUE, nugget = TRUE)
  txt <- formula_text(f$random)
  expect_match(txt, "facv(EffectEnv,2)", fixed = TRUE)
  expect_match(txt, "Env:Geno", fixed = TRUE)
  expect_match(formula_text(f$residual),
               "dsum(~ar1(Column):ar1(Row)|Env)", fixed = TRUE)
})

test_that("met_formulae keeps an fa() term outside str()", {
  # Verified against ASReml 4.2: fa() inside str() fails on a direct-product
  # size mismatch, because ASReml adds its own latent-factor levels.
  f <- met_formulae("RepF", c("N1", "N2"), 40, 4, structure = "fa", rank = 1,
                    spatial = TRUE, nugget = FALSE)
  txt <- formula_text(f$random)
  expect_false(grepl("str(", txt, fixed = TRUE))
  expect_match(txt, "fa(Env,1):Geno", fixed = TRUE)
})

test_that("met_formulae rejects an unknown covariance structure", {
  expect_error(met_formulae("RepF", "N1", 40, 4, structure = "nonsense"),
               "Unknown MET structure")
})

test_that("max_fa_rank never exceeds what the environments can support", {
  expect_lte(max_fa_rank(2), 2L)
  expect_lte(max_fa_rank(10), 10L)
  expect_gte(max_fa_rank(4), 1L)
})

test_that("covariate_fixed_terms follows the own-plot adjustment switch", {
  d <- prepare_trial_data(sample_single_trial(),
                          single_map(covariate = "Plant_height_cm"))
  nb <- add_neighbours(complete_field_grid(d), "rows")

  terms_own <- covariate_fixed_terms(nb$data, adjust_own = TRUE)
  expect_true(any(grepl("Covariate_own", terms_own)))
  expect_true(any(grepl("Covariate_nb", terms_own)))

  terms_nb <- covariate_fixed_terms(nb$data, adjust_own = FALSE)
  expect_false(any(grepl("Covariate_own", terms_nb)))

  # No prepared covariate means no covariate terms at all.
  plain <- add_neighbours(complete_field_grid(
    prepare_trial_data(sample_single_trial(), single_map())), "rows")
  expect_length(covariate_fixed_terms(plain$data, TRUE), 0L)
})

test_that("interpret_component labels components in breeder-facing language", {
  expect_equal(interpret_component("Geno"), "Direct genetic variance")
  expect_equal(interpret_component("N1"), "Competitive genetic variance")
  expect_equal(
    interpret_component(c("Geno+N1+and(N2)!us(2)_1:1",
                          "Geno+N1+and(N2)!us(2)_2:2",
                          "Geno+N1+and(N2)!us(2)_2:1")),
    c("Direct genetic variance", "Competitive genetic variance",
      "Direct-competition covariance")
  )
  expect_match(interpret_component("Column:Row!R"), "Spatial residual variance")
  expect_match(interpret_component("Column:Row!Row!cor"), "correlation")
  expect_match(interpret_component("RepF"), "[Rr]eplicate")

  # An unrecognised parameter is passed through rather than mislabelled.
  expect_equal(interpret_component("something_unknown"), "something_unknown")
})

test_that("interpret_component names environments in a MET block", {
  out <- interpret_component("Env:Geno+Env:N1!EffectEnv_D2!var",
                             env_levels = c("Env01", "Env02"))
  expect_match(out, "Direct specific variance")
  expect_match(out, "Env02", fixed = TRUE)
})

test_that("pretty_fixed_term expands the covariate columns", {
  expect_equal(pretty_fixed_term("Covariate_nb"), "Covariate, neighbouring plots")
  expect_equal(pretty_fixed_term("Covariate_own"), "Covariate, own plot")
  expect_equal(pretty_fixed_term("Env_Env01"), "Environment Env01")
  expect_equal(pretty_fixed_term("(Intercept)"), "(Intercept)")
})
