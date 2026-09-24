test_that("is_blank treats NULL, NA and empty strings alike", {
  expect_true(is_blank(NULL))
  expect_true(is_blank(NA))
  expect_true(is_blank(""))
  expect_true(is_blank(character(0)))
  expect_false(is_blank("Yield"))
  expect_false(is_blank(0))
})

test_that("%||% falls back on NULL and on zero length", {
  expect_equal(NULL %||% 3, 3)
  expect_equal(character(0) %||% "x", "x")
  expect_equal(5 %||% 3, 5)
  expect_equal(FALSE %||% TRUE, FALSE)
})

test_that("clean_names makes syntactic, unique names", {
  out <- clean_names(c(" Yield t/ha ", "Yield t/ha", "Rep"))
  expect_true(all(out == make.names(out)))
  expect_false(anyDuplicated(out) > 0)
  expect_equal(out[3], "Rep")
})

test_that("guess_column honours pattern priority and falls back", {
  cols <- c("Plot", "Genotype", "Entry", "Yield")
  expect_equal(guess_column(cols, c("^geno", "^entry")), "Genotype")
  expect_equal(guess_column(cols, c("^entry", "^geno")), "Entry")
  expect_equal(guess_column(cols, "^nothing$", default = ""), "")
})

test_that("ordered_unique sorts numerically when the labels are numeric", {
  # The bug this guards: alphabetical ordering puts "10" before "2", which
  # would mis-index the field grid.
  expect_equal(ordered_unique(c("10", "2", "1")), c("1", "2", "10"))
  expect_equal(ordered_unique(c("b", "a", "b")), c("a", "b"))
})

test_that("coordinate_levels fills internal gaps on an integer axis", {
  # A wholly absent field row must keep its level, or AR1 treats the plots
  # either side of it as adjacent.
  expect_equal(coordinate_levels(c(1, 2, 4, 5)), as.character(1:5))
  expect_equal(coordinate_levels(c(3, 1, 2)), as.character(1:3))
})

test_that("coordinate_levels refuses to expand implausibly sparse axes", {
  expect_equal(coordinate_levels(c(1, 5000)), c("1", "5000"))
  expect_equal(coordinate_levels(c("R1", "R2")), c("R1", "R2"))
  expect_equal(coordinate_levels(c(1.5, 2.5)), c("1.5", "2.5"))
})

test_that("escape_regex neutralises metacharacters", {
  x <- "a.b(c)[d]+e"
  expect_equal(grep(escape_regex(x), x), 1L)
  expect_length(grep(escape_regex("a.c"), "abc"), 0L)
})

test_that("safe_pct guards a zero or missing denominator", {
  expect_equal(safe_pct(c(1, 3), 4), c(25, 75))
  expect_true(all(is.na(safe_pct(c(1, 2), 0))))
  expect_true(all(is.na(safe_pct(c(1, 2), NA_real_))))
})

test_that("safe_cov2cor survives a non-positive diagonal", {
  # cov2cor() aborts here; at a variance-component boundary that would take
  # out the whole results panel.
  m <- matrix(c(0, 0, 0, 1), 2, 2)
  out <- expect_silent(safe_cov2cor(m))
  expect_true(is.na(out[1, 1]))
  expect_equal(out[2, 2], 1)
  expect_false(anyNA(out[2, 2]))
  # stats::cov2cor() cannot be used here: it warns and returns NaN.
  expect_warning(stats::cov2cor(m))
})

test_that("safe_cov2cor reproduces cov2cor on a well-behaved matrix", {
  m <- matrix(c(4, 1, 1, 9), 2, 2, dimnames = list(c("a", "b"), c("a", "b")))
  expect_equal(safe_cov2cor(m), stats::cov2cor(m))
})

test_that("safe_cov2cor clamps rounding overshoot into [-1, 1]", {
  m <- matrix(c(1, 1 + 1e-12, 1 + 1e-12, 1), 2, 2)
  expect_lte(max(abs(safe_cov2cor(m)), na.rm = TRUE), 1)
})

test_that("nearest_pd floors negative eigenvalues and keeps symmetry", {
  m <- matrix(c(1, 2, 2, 1), 2, 2)          # eigenvalues 3 and -1
  out <- nearest_pd(m)
  expect_gte(min(eigen(out, symmetric = TRUE)$values), 0)
  expect_equal(out, t(out))
})

test_that("nearest_pd leaves an already positive-definite matrix untouched", {
  m <- matrix(c(4, 1, 1, 9), 2, 2)
  expect_equal(nearest_pd(m), m)
})

test_that("matrix_to_long and matrix_to_wide round-trip a labelled matrix", {
  m <- matrix(1:4, 2, 2, dimnames = list(c("E1", "E2"), c("E1", "E2")))
  long <- matrix_to_long(m, value_name = "Correlation")
  expect_equal(nrow(long), 4L)
  expect_named(long, c("Row", "Column", "Correlation"))
  expect_equal(long$Correlation, as.numeric(m))

  wide <- matrix_to_wide(m, label = "Environment")
  expect_equal(names(wide)[1], "Environment")
  expect_equal(wide$Environment, c("E1", "E2"))
})

test_that("matrix_to_long can tag rows with an effect label", {
  m <- matrix(1, 1, 1, dimnames = list("a", "a"))
  expect_equal(matrix_to_long(m, label = "Direct")$Effect, "Direct")
})

test_that("pretty_term never leaks an internal factor name", {
  internal <- c("RepF", "BlockF", "Geno", "N1", "units", "EffectEnv", "EnvDummy")
  out <- pretty_term(internal)
  expect_false(any(out %in% internal))
  expect_equal(pretty_term("Yield"), "Yield")   # unmapped names pass through
})

test_that("fmt renders NA as an en dash rather than the string NA", {
  expect_equal(fmt(NA_real_), "\u2013")
  expect_equal(fmt(1.23456, digits = 2), "1.23")
})

test_that("solution_column and std_error_column tolerate ASReml's casing", {
  expect_equal(solution_column(data.frame(solution = 1, std.error = 1)), "solution")
  expect_equal(solution_column(data.frame(Solution = 1)), "Solution")
  expect_equal(std_error_column(data.frame(solution = 1, std.error = 1)), "std.error")
  expect_true(is.na(std_error_column(data.frame(solution = 1))))
  expect_error(solution_column(data.frame(x = 1)), "Solution column")
})

test_that("theoretical_quantiles returns an ordered, symmetric grid", {
  q <- theoretical_quantiles(11)
  expect_length(q, 11L)
  expect_false(is.unsorted(q))
  expect_equal(q, -rev(q))
})
