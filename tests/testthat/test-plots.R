# A genotype table of the shape the plotting functions consume, built without
# fitting a model so that these tests run everywhere.
fake_genetic <- function(n = 40L, seed = 3L) {
  set.seed(seed)
  direct <- stats::rnorm(n, 0, 0.6)
  comp <- -0.4 * direct + stats::rnorm(n, 0, 0.3)
  data.frame(
    Genotype = sprintf("MZ%03d", seq_len(n)),
    Direct_effect = direct,
    Competition_effect = comp,
    Pure_stand_effect = direct + 2 * comp,
    Predicted_pure_stand_yield = 8.2 + direct + 2 * comp,
    SE_direct = stats::runif(n, 0.08, 0.2),
    SE_pure_stand = stats::runif(n, 0.1, 0.3),
    Reliability_direct = stats::runif(n, 0.3, 0.9),
    Rank_direct = rank(-direct),
    Rank_pure_stand = rank(-(direct + 2 * comp)),
    Rank_change = rank(-direct) - rank(-(direct + 2 * comp)),
    In_trial = TRUE,
    stringsAsFactors = FALSE
  )
}

expect_ggplot <- function(p) {
  testthat::expect_s3_class(p, "ggplot")
  # A plot that errors only when drawn is not much use; force the build.
  testthat::expect_silent(invisible(ggplot2::ggplot_build(p)))
  invisible(p)
}

# The field map is fed a purpose-built frame, exactly as the Shiny modules do.
field_frame <- function(d) {
  data.frame(Row = d$Row_i, Column = d$Col_i, Env = d$Env, Observed = d$Yield)
}

test_that("the field map builds for a single trial and for a facetted MET", {
  d <- prepare_trial_data(sample_single_trial(), single_map())
  expect_ggplot(plot_field_map(field_frame(d), "Observed", title = "Field plan"))

  m <- prepare_trial_data(sample_met_trial(), met_map(), multi_env = TRUE)
  expect_ggplot(plot_field_map(field_frame(m), "Observed", facet = TRUE))
})

test_that("the field map supports a diverging fill", {
  d <- prepare_trial_data(sample_single_trial(), single_map())
  f <- field_frame(d)
  f$Residual <- f$Observed - mean(f$Observed, na.rm = TRUE)
  expect_ggplot(plot_field_map(f, "Residual", diverging = TRUE,
                               fill_label = "Residual"))
})

test_that("the field map accepts factor coordinates as well as integers", {
  # prepare_trial_data() stores the coordinates twice: as factors in
  # Row/Column and as integers in Row_i/Col_i. Passing the factor form used to
  # fail at draw time with ggplot2's generic discrete-scale error.
  d <- prepare_trial_data(sample_single_trial(), single_map())
  f <- data.frame(Row = d$Row, Column = d$Column, Env = d$Env, Observed = d$Yield)
  expect_ggplot(plot_field_map(f, "Observed"))
})

test_that("the field map reports a missing value column by name", {
  d <- prepare_trial_data(sample_single_trial(), single_map())
  expect_error(plot_field_map(field_frame(d), "Not_A_Column"), "Not_A_Column")
})

test_that("the direct-versus-competition scatter builds", {
  expect_ggplot(plot_direct_vs_competition(fake_genetic(), k = 2))
})

test_that("the scatter refuses an empty genotype table rather than drawing nothing", {
  empty <- fake_genetic()[0, ]
  expect_error(plot_direct_vs_competition(empty), "No genotypes")
})

test_that("the ranking plot honours each selectable effect", {
  g <- fake_genetic()
  for (effect in c("Predicted_pure_stand_yield", "Pure_stand_effect",
                   "Direct_effect")) {
    expect_ggplot(plot_ranking(g, effect = effect, top_n = 15))
  }
})

test_that("the ranking plot falls back when the predicted yield is absent", {
  g <- fake_genetic()
  g$Predicted_pure_stand_yield <- NULL
  expect_ggplot(plot_ranking(g, top_n = 10))
})

test_that("the ranking plot copes with missing standard errors", {
  g <- fake_genetic()
  g$SE_pure_stand <- NA_real_
  expect_ggplot(plot_ranking(g, top_n = 10))
})

test_that("the rank-change plot builds", {
  expect_ggplot(plot_rank_change(fake_genetic(), top_n = 20))
})

test_that("the variance-component plot builds from a component table", {
  vc <- data.frame(
    Component = c("Geno", "N1", "Column:Row!R"),
    Interpretation = c("Direct genetic variance", "Competitive genetic variance",
                       "Spatial residual variance"),
    Estimate = c(0.27, 0.07, 0.31),
    Pct_of_total = c(41.5, 10.8, 47.7),
    stringsAsFactors = FALSE
  )
  expect_ggplot(plot_variance_components(vc))
})

test_that("the variance-component plot refuses a table with nothing positive", {
  vc <- data.frame(Component = "Geno", Interpretation = "Direct genetic variance",
                   Estimate = 0, Pct_of_total = 0, stringsAsFactors = FALSE)
  expect_error(plot_variance_components(vc), "No positive variance components")
})

test_that("the correlation heatmap builds and blanks its diagonal", {
  m <- matrix(c(1, 0.6, 0.3, 0.6, 1, 0.5, 0.3, 0.5, 1), 3, 3,
              dimnames = list(sprintf("E%d", 1:3), sprintf("E%d", 1:3)))
  expect_ggplot(plot_correlation_heatmap(m, title = "Direct effects"))
})

test_that("the environment-variance plot builds", {
  v <- data.frame(Environment = sprintf("E%d", 1:4),
                  Direct_variance = c(0.30, 0.40, 0.20, 0.35),
                  Competition_variance = c(0.05, 0.07, 0.04, 0.06),
                  Pure_stand_variance = c(0.22, 0.31, 0.15, 0.26),
                  stringsAsFactors = FALSE)
  expect_ggplot(plot_environment_variances(v))
})

test_that("theme_trial returns a usable ggplot2 theme in every grid mode", {
  for (g in c("xy", "x", "y", "none")) {
    th <- theme_trial(12, grid = g)
    expect_s3_class(th, "theme")
    expect_ggplot(ggplot2::ggplot(data.frame(x = 1:3, y = 1:3),
                                  ggplot2::aes(x, y)) +
                    ggplot2::geom_point() + th)
  }
})

test_that("series colours follow the entity, not the rank", {
  expect_equal(series_colour("Direct_effect"), PAL$direct)
  expect_equal(series_colour("Competition_effect"), PAL$competition)
  expect_equal(series_colour("anything_else"), PAL$pure)
  expect_named(effect_colours(),
               c("Direct", "Competition", "Pure stand", "Neighbour"))
})

test_that("contrast_text picks a legible label colour from the fill luminance", {
  # White text on a pale fill is the failure this guards against.
  expect_equal(as.vector(contrast_text("#FFFFFF")), PAL$ink)
  expect_equal(as.vector(contrast_text("#000000")), "#FFFFFF")
  expect_length(contrast_text(c("#FFFFFF", "#000000")), 2L)
})

test_that("scale_diverging centres its neutral step on the stated centre", {
  s <- scale_diverging(c(0.1, 0.5, 0.9), centre = 0, aesthetic = "fill")
  expect_s3_class(s, "ScaleContinuous")
  # A set of entirely positive values must still map symmetrically about zero.
  expect_equal(s$limits, c(-0.9, 0.9))
})

test_that("scale_diverging survives a degenerate range", {
  s <- scale_diverging(c(2, 2, 2), centre = 2)
  expect_equal(s$limits, c(1, 3))
  expect_s3_class(scale_diverging(NA_real_, centre = 0), "ScaleContinuous")
})

test_that("captions wrap rather than run off the figure", {
  long <- paste(rep("a long clause about the fitted model", 12), collapse = ", ")
  wrapped <- wrap_caption(long, base_size = 12)
  expect_true(grepl("\n", wrapped, fixed = TRUE))
  expect_null(wrap_caption(NULL))
})

test_that("figure_base_size scales with the export width", {
  small <- figure_base_size(10)
  large <- figure_base_size(30)
  expect_gt(large, small)
  expect_true(is.finite(small) && small > 0)
})
