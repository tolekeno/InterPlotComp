# ---------------------------------------------------------------------------
# Worked example data
#
# The previous version shipped a `sample_interplot_trial.csv` that was not in
# the repository, so both example-download buttons failed at runtime. Examples
# are now simulated on demand from a fixed seed: there is no missing-file
# failure mode, the data reproduce exactly, and the true parameters are known
# so users can check the model recovers them.
#
# The simulation deliberately includes the features that break naive code:
# an incomplete block design, AR1 spatial trend, a few failed plots, an
# internal hole in the field grid, unequal replication and (for the MET) trials
# of different sizes with different genotype sets.
# ---------------------------------------------------------------------------

#' True parameter values used by the simulator, shown in the app's help text.
#' @noRd
SIM_TRUTH <- list(
  direct_var      = 0.36,
  competition_var = 0.09,
  direct_comp_cor = -0.55,
  block_var       = 0.05,
  spatial_var     = 0.22,
  ar_row          = 0.60,
  ar_col          = 0.35,
  nugget_var      = 0.06,
  grand_mean      = 8.2
)

#' Draw correlated direct and competitive effects for a genotype panel.
#' @noRd
simulate_genetic_effects <- function(genotypes, truth = SIM_TRUTH) {
  n <- length(genotypes)
  s <- matrix(c(truth$direct_var,
                truth$direct_comp_cor * sqrt(truth$direct_var * truth$competition_var),
                truth$direct_comp_cor * sqrt(truth$direct_var * truth$competition_var),
                truth$competition_var), 2, 2)
  z <- matrix(stats::rnorm(n * 2), n, 2) %*% chol(s)
  list(direct = stats::setNames(z[, 1], genotypes),
       competition = stats::setNames(z[, 2], genotypes))
}

#' AR1 x AR1 spatial surface on a row x column grid.
#' @noRd
simulate_ar1_surface <- function(n_rows, n_cols, var, rho_row, rho_col) {
  ar1 <- function(n, rho) rho^abs(outer(seq_len(n), seq_len(n), "-"))
  s <- sqrt(var) * (chol(ar1(n_rows, rho_row)))
  t_col <- chol(ar1(n_cols, rho_col))
  matrix(stats::rnorm(n_rows * n_cols), n_rows, n_cols) |>
    (\(z) t(s) %*% z %*% t_col)()
}

#' Simulate one single-row-plot trial with inter-plot competition.
#'
#' @param n_rows,n_cols field dimensions (rows are the competition direction)
#' @param n_geno number of genotypes; replication is `n_rows * n_cols / n_geno`
#' @param seed random seed
#' @param truth list of true variance parameters
#' @noRd
simulate_trial <- function(n_rows = 15, n_cols = 12, n_geno = 60, seed = 2026,
                           truth = SIM_TRUTH, env_effect = 0,
                           genotypes = NULL, effects = NULL) {
  set.seed(seed)
  if (is.null(genotypes)) genotypes <- sprintf("MZ%03d", seq_len(n_geno))
  n_geno <- length(genotypes)
  n_plot <- n_rows * n_cols
  n_rep  <- n_plot %/% n_geno
  if (n_rep < 1L) stop("Too many genotypes for the field size.", call. = FALSE)

  if (is.null(effects)) effects <- simulate_genetic_effects(genotypes, truth)

  # Resolvable design: replicates run along the columns, incomplete blocks are
  # contiguous strips of plots within a replicate.
  d <- expand.grid(Row = seq_len(n_rows), Column = seq_len(n_cols),
                   KEEP.OUT.ATTRS = FALSE)
  d <- d[order(d$Column, d$Row), ]
  d$Rep <- pmin(((seq_len(n_plot) - 1L) %/% n_geno) + 1L, n_rep)
  d$Block <- ((seq_len(n_plot) - 1L) %/% max(3L, n_geno %/% 4L)) + 1L
  d$Genotype <- unlist(lapply(seq_len(n_rep), function(i) sample(genotypes)))[seq_len(n_plot)]
  # Any plots beyond a whole number of replicates are filled at random.
  if (anyNA(d$Genotype)) d$Genotype[is.na(d$Genotype)] <-
    sample(genotypes, sum(is.na(d$Genotype)), replace = TRUE)

  key <- paste(d$Row, d$Column, sep = "/")
  neighbour_effect <- rep(0, n_plot)
  for (o in list(c(-1, 0), c(1, 0))) {
    g <- d$Genotype[match(paste(d$Row + o[1], d$Column + o[2], sep = "/"), key)]
    neighbour_effect <- neighbour_effect + ifelse(is.na(g), 0, effects$competition[g])
  }

  surface <- simulate_ar1_surface(n_rows, n_cols, truth$spatial_var,
                                  truth$ar_row, truth$ar_col)
  block_effect <- stats::setNames(
    stats::rnorm(length(unique(d$Block)), 0, sqrt(truth$block_var)),
    sort(unique(d$Block))
  )

  d$Yield <- truth$grand_mean + env_effect +
    effects$direct[d$Genotype] + neighbour_effect +
    block_effect[as.character(d$Block)] +
    surface[cbind(d$Row, d$Column)] +
    stats::rnorm(n_plot, 0, sqrt(truth$nugget_var))

  # A correlated proxy trait, so the worked examples can demonstrate the
  # competition adjustment. Plant height is generated from the same genotype
  # competitive effects that drive the interference, which is exactly the
  # situation the adjustment is meant for: a taller neighbour shades its
  # neighbour, and the trait therefore explains part of the competition.
  d$Plant_height_cm <- round(
    200 + 60 * effects$competition[d$Genotype] * -1 +
      12 * effects$direct[d$Genotype] +
      stats::rnorm(n_plot, 0, 6), 1)

  d$Yield <- round(d$Yield, 3)
  d <- d[, c("Row", "Column", "Rep", "Block", "Genotype", "Plant_height_cm", "Yield")]
  rownames(d) <- NULL
  attr(d, "effects") <- effects
  d
}

#' Worked example: a single trial with inter-plot competition
#'
#' Simulates one 15 x 12 single-row-plot trial of 60 genotypes from the known
#' parameters in `SIM_TRUTH`, so that a fitted model can be checked against the
#' values that generated the data. Includes the imperfections that break naive
#' code: an incomplete block design, AR1 spatial trend, three failed plots and
#' one position physically absent from the field.
#'
#' @return A data frame with columns `Row`, `Column`, `Rep`, `Block`,
#'   `Genotype` and `Yield_t_ha`.
#' @export
#' @examples
#' d <- sample_single_trial()
#' str(d)
sample_single_trial <- function() {
  d <- simulate_trial(n_rows = 15, n_cols = 12, n_geno = 60, seed = 2026)
  set.seed(11)
  # Three failed plots: identity known, response lost.
  d$Yield[sample(nrow(d), 3)] <- NA
  # One position physically absent from the field (a service alley).
  d <- d[!(d$Row == 8 & d$Column == 6), , drop = FALSE]
  names(d)[names(d) == "Yield"] <- "Yield_t_ha"
  rownames(d) <- NULL
  d
}

#' Worked example: a multi-environment trial series
#'
#' Four environments of different sizes, sharing a core genotype set with
#' environment-specific additions, moderate crossover genotype-by-environment
#' interaction, unequal replication, six failed plots and one internal grid
#' hole.
#'
#' @return A data frame with columns `Environment`, `Row`, `Column`, `Rep`,
#'   `Block`, `Genotype` and `Yield_t_ha`.
#' @export
#' @examples
#' d <- sample_met_trial()
#' table(d$Environment)
sample_met_trial <- function() {
  core <- sprintf("MZ%03d", 1:40)
  extra <- sprintf("MZ%03d", 41:52)
  set.seed(4242)

  # Neutral environment labels: a worked example should not imply that these
  # are real site results.
  spec <- list(
    list(env = "Env04", rows = 12, cols = 10, geno = c(core, extra[1:8]),   shift =  0.00, seed = 101),
    list(env = "Env03", rows = 10, cols = 10, geno = core,                  shift =  0.85, seed = 102),
    list(env = "Env02", rows = 14, cols =  8, geno = c(core, extra[9:12]),  shift = -0.60, seed = 103),
    list(env = "Env01", rows = 12, cols =  9, geno = core,                  shift =  0.30, seed = 104)
  )

  # A shared genetic core with environment-specific deviation produces genuine
  # but incomplete correlation between environments, which is what the
  # factor-analytic model is meant to describe.
  base <- simulate_genetic_effects(union(core, extra))
  out <- lapply(spec, function(s) {
    set.seed(s$seed)
    w <- 0.75
    eff <- list(
      direct = w * base$direct[s$geno] +
        sqrt(1 - w^2) * stats::rnorm(length(s$geno), 0, sqrt(SIM_TRUTH$direct_var)),
      competition = w * base$competition[s$geno] +
        sqrt(1 - w^2) * stats::rnorm(length(s$geno), 0, sqrt(SIM_TRUTH$competition_var))
    )
    names(eff$direct) <- names(eff$competition) <- s$geno
    z <- simulate_trial(s$rows, s$cols, seed = s$seed, env_effect = s$shift,
                        genotypes = s$geno, effects = eff)
    data.frame(Environment = s$env, z, stringsAsFactors = FALSE)
  })
  d <- do.call(rbind, out)

  set.seed(9)
  d$Yield[sample(nrow(d), 6)] <- NA
  d <- d[!(d$Environment == "Env02" & d$Row == 5 & d$Column == 4), , drop = FALSE]
  names(d)[names(d) == "Yield"] <- "Yield_t_ha"
  rownames(d) <- NULL
  d
}

#' Worked example: a pedigree for the example trials
#'
#' A conventional breeding structure: ten unrelated founders crossed as five
#' males by five females, so the trial entries form overlapping full-sib and
#' half-sib families. That is exactly the structure a relationship matrix
#' exploits, and it lets the worked example demonstrate the feature end to end.
#'
#' @param genotypes Character vector of entry identifiers to assign parents to.
#'   Defaults to the genotypes of both worked example trials.
#' @return A data frame with columns `Genotype`, `Male_parent` and
#'   `Female_parent`, founders having `NA` parents.
#' @export
#' @examples
#' ped <- sample_pedigree()
#' head(ped)
sample_pedigree <- function(genotypes = NULL) {
  if (is.null(genotypes)) {
    genotypes <- sort(unique(c(sample_single_trial()$Genotype,
                               sample_met_trial()$Genotype)))
  }
  males   <- sprintf("FOUNDER_M%d", 1:5)
  females <- sprintf("FOUNDER_F%d", 1:5)
  set.seed(31)
  rbind(
    data.frame(Genotype = c(males, females),
               Male_parent = NA_character_, Female_parent = NA_character_,
               stringsAsFactors = FALSE),
    data.frame(Genotype = genotypes,
               Male_parent = sample(males, length(genotypes), replace = TRUE),
               Female_parent = sample(females, length(genotypes), replace = TRUE),
               stringsAsFactors = FALSE)
  )
}
