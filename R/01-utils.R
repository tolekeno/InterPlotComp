# ---------------------------------------------------------------------------
# Small, dependency-free helpers shared by every module.
# ---------------------------------------------------------------------------

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x

#' Treat "", NA and NULL alike as "not supplied".
is_blank <- function(x) is.null(x) || length(x) == 0L || is.na(x[1]) || !nzchar(x[1])

#' Syntactically valid, de-duplicated column names.
clean_names <- function(x) make.names(trimws(x), unique = TRUE)

#' Guess a column name from a set of regular expressions, in priority order.
#'
#' @param candidates available column names
#' @param patterns regular expressions tried in order; the first hit wins
#' @param default value returned when nothing matches
guess_column <- function(candidates, patterns, default = "") {
  for (p in patterns) {
    hit <- grep(p, candidates, ignore.case = TRUE, value = TRUE)
    if (length(hit)) return(hit[1])
  }
  default
}

#' Locate the "solution" column of an ASReml coefficient table.
#'
#' ASReml-R has used both `solution` and `Solution` across releases.
solution_column <- function(x) {
  hit <- grep("^solution$", names(x), ignore.case = TRUE, value = TRUE)
  if (!length(hit)) hit <- grep("solution", names(x), ignore.case = TRUE, value = TRUE)
  if (!length(hit)) {
    stop("Could not find the Solution column in the ASReml coefficient table.", call. = FALSE)
  }
  hit[1]
}

#' Locate the standard-error column of an ASReml coefficient table (may be absent).
std_error_column <- function(x) {
  hit <- grep("^std[._ ]?error$", names(x), ignore.case = TRUE, value = TRUE)
  if (!length(hit)) hit <- grep("std", names(x), ignore.case = TRUE, value = TRUE)
  if (!length(hit)) NA_character_ else hit[1]
}

#' Escape a string for literal use inside a regular expression.
escape_regex <- function(x) {
  gsub("([][{}().|^$*+?\\\\-])", "\\\\\\1", x)
}

#' Order unique values numerically when the labels are numbers, else alphabetically.
ordered_unique <- function(x) {
  x_chr <- as.character(x)
  x_num <- suppressWarnings(as.numeric(x_chr))
  if (!anyNA(x_num)) unique(x_chr[order(x_num)]) else sort(unique(x_chr))
}

#' Coordinate levels for a field axis, filling internal gaps.
#'
#' A field axis labelled with integers is expanded to its full span so that an
#' entirely absent row or column still occupies a level. Without this, AR1
#' would treat plots either side of a missing row as physically adjacent, which
#' silently biases both the spatial and the competition estimates. Expansion is
#' skipped for sparse or non-integer labels, where the span is not meaningful.
#'
#' @param x observed coordinate values for one trial
#' @param max_expansion refuse to expand beyond this many levels
coordinate_levels <- function(x, max_expansion = 2000L) {
  x_chr <- as.character(x)
  x_num <- suppressWarnings(as.numeric(x_chr))
  if (anyNA(x_num)) return(sort(unique(x_chr)))

  observed <- sort(unique(x_num))
  is_integer_grid <- all(abs(observed - round(observed)) < 1e-8)
  span <- max(observed) - min(observed) + 1
  if (is_integer_grid && span <= min(max_expansion, max(5L * length(observed), 20L))) {
    return(as.character(seq.int(min(observed), max(observed))))
  }
  as.character(observed)
}

#' Format a number for compact on-screen display.
fmt <- function(x, digits = 3) {
  ifelse(is.na(x), "–", formatC(x, format = "f", digits = digits, big.mark = ","))
}

#' Percentage of a total, guarding against a zero or missing denominator.
safe_pct <- function(x, total) {
  if (!is.finite(total) || total <= 0) return(rep(NA_real_, length(x)))
  100 * x / total
}

#' Correlation matrix from a covariance matrix, tolerating non-positive diagonals.
#'
#' `stats::cov2cor()` errors on a zero or negative diagonal, which occurs
#' routinely at a variance-component boundary. This version returns NA for the
#' affected cells instead of aborting the whole results panel.
safe_cov2cor <- function(x) {
  x <- as.matrix(x)
  d <- diag(x)
  out <- x / sqrt(outer(d, d))
  out[!is.finite(out)] <- NA_real_
  diag(out) <- ifelse(d > 0, 1, NA_real_)
  out[!is.na(out) & out >  1] <-  1
  out[!is.na(out) & out < -1] <- -1
  dimnames(out) <- dimnames(x)
  out
}

#' Nearest positive-definite correction of a symmetric matrix.
#'
#' Derived covariance matrices (for example the pure-stand matrix
#' G_D + k^2 G_C + k(G_DC + G_DC')) are algebraically valid but can acquire
#' tiny negative eigenvalues from rounding. Eigenvalues are floored rather than
#' the matrix rebuilt, so the result stays as close to the estimate as possible.
nearest_pd <- function(x, tol = 1e-10) {
  x <- as.matrix(x)
  x <- (x + t(x)) / 2
  if (anyNA(x)) return(x)
  ev <- eigen(x, symmetric = TRUE)
  if (min(ev$values) >= tol) return(x)
  floored <- pmax(ev$values, tol * max(1, max(ev$values)))
  out <- ev$vectors %*% diag(floored, length(floored)) %*% t(ev$vectors)
  dimnames(out) <- dimnames(x)
  (out + t(out)) / 2
}

#' Convert a square matrix to a long data frame, for tables and ggplot heatmaps.
matrix_to_long <- function(x, value_name = "Value", label = NULL) {
  x <- as.matrix(x)
  out <- expand.grid(
    Row = rownames(x), Column = colnames(x),
    KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE
  )
  out[[value_name]] <- as.numeric(x)
  if (!is.null(label)) out <- data.frame(Effect = label, out, stringsAsFactors = FALSE)
  out
}

#' Convert a square matrix to a wide data frame with an explicit label column.
matrix_to_wide <- function(x, label = "Environment") {
  out <- data.frame(rownames(x), as.data.frame(x, check.names = FALSE),
                    check.names = FALSE, row.names = NULL)
  names(out)[1] <- label
  out
}

#' Timestamped download file name.
stamped <- function(base, ext) {
  sprintf("%s_%s.%s", base, format(Sys.time(), "%Y%m%d_%H%M"), ext)
}

#' Standard-normal-scale inverse for QQ plots without pulling in extra packages.
theoretical_quantiles <- function(n) stats::qnorm(stats::ppoints(n))

#' Translate internal model-term names into wording a breeder will recognise.
#'
#' Internal names (RepF, BlockF, N1, EffectEnv) are convenient in formulae but
#' must never reach a figure caption, a results table or an error message.
pretty_term <- function(x) {
  map <- c(
    RepF = "replicate", BlockF = "block", EnvRep = "replicate",
    Geno = "genotype (direct)", N1 = "neighbour genotype (competitive)",
    `Env:Geno` = "environment x genotype (direct)",
    `Env:N1` = "environment x neighbour (competitive)",
    units = "plot (nugget)", EffectEnv = "environment effect",
    EnvDummy = "environment"
  )
  out <- unname(map[x])
  ifelse(is.na(out), x, out)
}
