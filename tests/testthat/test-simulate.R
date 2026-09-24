test_that("sample_single_trial is reproducible and correctly shaped", {
  a <- sample_single_trial()
  b <- sample_single_trial()
  expect_identical(a, b)           # fixed seed: the worked example must not drift

  expect_named(a, c("Row", "Column", "Rep", "Block", "Genotype",
                    "Plant_height_cm", "Yield_t_ha"))
  expect_equal(length(unique(a$Genotype)), 60L)
  expect_equal(max(a$Row), 15L)
  expect_equal(max(a$Column), 12L)
})

test_that("sample_single_trial carries the imperfections it advertises", {
  d <- sample_single_trial()
  # Three failed plots: identity known, response lost.
  expect_equal(sum(is.na(d$Yield_t_ha)), 3L)
  # One position physically absent from the field.
  expect_equal(nrow(d), 15L * 12L - 1L)
  expect_equal(sum(d$Row == 8 & d$Column == 6), 0L)
})

test_that("sample_met_trial spans four environments of different sizes", {
  d <- sample_met_trial()
  expect_identical(d, sample_met_trial())
  expect_setequal(unique(d$Environment), c("Env01", "Env02", "Env03", "Env04"))

  grids <- tapply(seq_len(nrow(d)), d$Environment,
                  function(i) paste(max(d$Row[i]), max(d$Column[i])))
  expect_equal(length(unique(unlist(grids))), 4L)   # genuinely different shapes
  expect_equal(sum(is.na(d$Yield_t_ha)), 6L)
})

test_that("sample_met_trial gives environments overlapping but unequal panels", {
  d <- sample_met_trial()
  panels <- split(unique(d[c("Environment", "Genotype")])$Genotype,
                  unique(d[c("Environment", "Genotype")])$Environment)
  expect_gt(length(Reduce(intersect, panels)), 30L)   # a shared core
  expect_false(length(unique(lengths(panels))) == 1L) # but not identical panels
})

test_that("the worked examples flow through the whole preparation pipeline", {
  nb <- prepared_single()
  expect_gt(nrow(nb$data), 0L)

  d <- prepare_trial_data(sample_met_trial(), met_map(), multi_env = TRUE)
  mnb <- add_neighbours(complete_field_grid(d), "rows")
  expect_equal(mnb$k, 2L)
  expect_equal(nlevels(mnb$data$Env), 4L)
})

test_that("sample_pedigree covers every entry of both worked examples", {
  ped <- sample_pedigree()
  expect_named(ped, c("Genotype", "Male_parent", "Female_parent"))
  expect_false(anyDuplicated(ped$Genotype) > 0)

  entries <- unique(c(sample_single_trial()$Genotype, sample_met_trial()$Genotype))
  expect_true(all(entries %in% ped$Genotype))

  # Founders have no parents; everyone else has both.
  founders <- is.na(ped$Male_parent)
  expect_equal(sum(founders), 10L)
  expect_true(all(is.na(ped$Female_parent[founders])))
  expect_false(anyNA(ped$Male_parent[!founders]))
  expect_false(anyNA(ped$Female_parent[!founders]))
})

test_that("sample_pedigree accepts an explicit entry list", {
  ped <- sample_pedigree(c("A", "B", "C"))
  expect_true(all(c("A", "B", "C") %in% ped$Genotype))
  expect_equal(nrow(ped), 13L)   # 10 founders plus the three entries
})

test_that("the simulator refuses a panel larger than the field", {
  expect_error(simulate_trial(n_rows = 2, n_cols = 2, n_geno = 50),
               "Too many genotypes")
})

test_that("simulated effects follow the declared covariance structure", {
  set.seed(1)
  eff <- simulate_genetic_effects(sprintf("G%04d", 1:20000))
  expect_equal(stats::var(eff$direct), SIM_TRUTH$direct_var, tolerance = 0.05)
  expect_equal(stats::var(eff$competition), SIM_TRUTH$competition_var,
               tolerance = 0.05)
  expect_equal(stats::cor(eff$direct, eff$competition),
               SIM_TRUTH$direct_comp_cor, tolerance = 0.05)
})

test_that("the simulated spatial surface has the requested variance", {
  set.seed(2)
  s <- simulate_ar1_surface(60, 60, var = 0.25, rho_row = 0.6, rho_col = 0.3)
  expect_equal(dim(s), c(60L, 60L))
  expect_equal(stats::var(as.numeric(s)), 0.25, tolerance = 0.15)
})
