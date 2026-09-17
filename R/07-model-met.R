# ---------------------------------------------------------------------------
# Multi-environment inter-plot competition model
#
#   y_ieg = mu + E_e + [design within e] + d_eg + sum_{j in N(i)} c_e,g(j) + s_ie
#
# Direct and competitive effects are environment-specific, and the 2E of them
# share one joint covariance matrix:
#
#   random = ~ str(~ Env:Geno + Env:N1 + and(Env:N2),
#                  ~ facv(EffectEnv, r):id(nGeno))
#   residual = ~ dsum(~ ar1(Column):ar1(Row) | Env)
#
# True `Env:Geno` interaction terms are used rather than a pre-combined
# environment-by-genotype factor. A pre-combined factor drops the unobserved
# cells of a sparse genotype x environment table, which then makes the genetic
# term smaller than its variance structure and produces a conformability error.
#
# `dsum()` gives each environment its own AR1 x AR1 section, so environments
# may have different field dimensions.
#
# STRUCTURES
#   facv       joint FA(r) over all 2E effects. Most general: direct and
#              competitive effects may have different G x E patterns.
#              Parameters: 2E(r + 1).
#   separable  us(2) between direct and competitive effects, crossed with an
#              FA(r) environment covariance. Assumes one shared environment
#              correlation pattern, but costs only 3 + E(r + 1) parameters, so
#              it often fits where the joint model is singular.
#   diag       environment-specific variances, no between-environment
#              correlation. The nested null model for G x E.
# ---------------------------------------------------------------------------

MET_STRUCTURES <- c(
  "Joint factor-analytic over direct + competitive effects" = "facv",
  "Separable us(2) \u00d7 factor-analytic environments"      = "separable",
  "Separate fa() per effect \u2014 no direct-competition covariance" = "fa",
  "Diagonal \u2014 no genetic correlation between environments" = "diag"
)

#' Add the synthetic factors that give the `str()` variance model its dimensions.
#'
#' `facv()` requires a *factor* argument, not an integer, so a factor with the
#' right number of levels must exist in the data. Only the level count and
#' labels matter: the design effects themselves come from `Env:Geno` and
#' `Env:N1 + and(Env:N2)`. Levels are assigned by recycling across records,
#' which is arbitrary by design and never enters the fitted model. Labels are
#' chosen so that every ASReml variance parameter can be matched by name.
#' @noRd
add_met_dummy_factors <- function(d) {
  env_levels <- levels(d$Env)
  e <- length(env_levels)
  effect_levels <- c(sprintf("D%d", seq_len(e)), sprintf("C%d", seq_len(e)))
  d$EffectEnv <- factor(rep(effect_levels, length.out = nrow(d)), levels = effect_levels)
  d$EnvDummy  <- factor(rep(env_levels, length.out = nrow(d)), levels = env_levels)
  attr(d, "effect_levels") <- effect_levels
  d
}

#' Highest sensible factor-analytic rank for E environments.
#'
#' An FA(r) covariance over n dimensions is identified only while the number of
#' free parameters does not exceed n(n + 1)/2.
#' @noRd
max_fa_rank <- function(n_env, structure = "facv") {
  n <- if (structure %in% c("separable", "fa")) n_env else 2L * n_env
  r <- 1L
  while ((r + 1L) * n - (r + 1L) * r / 2 <= n * (n + 1) / 2 && r < 4L) r <- r + 1L
  max(1L, min(r, n_env - 1L, 3L))
}

#' Build the MET random and residual formulae.
#' @noRd
met_formulae <- function(design_terms, neighbour_names, n_geno, n_env,
                         structure = "facv", rank = 1L, spatial = TRUE,
                         nugget = TRUE, competition = TRUE, kinship = FALSE,
                         row_process = "ar1", col_process = "ar1") {
  # See single_formulae(): with a relationship matrix the genotype dimension of
  # the str() variance formula must be vm(Geno, .kinship), not id(n).
  gterm <- function(f) if (kinship) sprintf("vm(%s, .kinship)", f) else f
  gdim  <- if (kinship) "vm(Geno, .kinship)" else sprintf("id(%d)", n_geno)

  # The fa() structure is not a str() block: ASReml augments an fa() term with
  # its own latent-factor levels, so the term cannot sit inside a str() whose
  # size is fixed by the listed effects. Verified: fa(EffectEnv, 1) inside
  # str() fails with "Size of direct product (468) does not conform with total
  # size of included terms (416)", the gap being rank x nGeno.
  if (identical(structure, "fa")) {
    fa_term <- function(f) sprintf("fa(Env, %d):%s", rank, gterm(f))
    genetic <- if (competition) {
      paste(c(fa_term("Geno"), fa_term(neighbour_names[1]),
              sprintf("and(%s)", vapply(neighbour_names[-1], fa_term, character(1)))),
            collapse = " + ")
    } else {
      fa_term("Geno")
    }
    random <- c(design_terms, genetic)
    if (spatial && nugget) random <- c(random, "idv(units)")
    return(list(
      random = stats::as.formula(paste("~", paste(random, collapse = " + ")),
                                 env = globalenv()),
      residual = stats::as.formula(
        if (spatial) {
          sprintf("~ dsum(~ %s | Env)",
                  spatial_residual_text(row_process, col_process))
        } else "~ dsum(~ idv(units) | Env)",
        env = globalenv())
    ))
  }

  if (competition) {
    neighbour_sum <- paste(
      c(sprintf("Env:%s", gterm("N1")),
        sprintf("and(Env:%s)", gterm(neighbour_names[-1]))),
      collapse = " + "
    )
    model_part <- sprintf("~ Env:%s + %s", gterm("Geno"), neighbour_sum)
    cov_part <- switch(
      structure,
      facv      = sprintf("facv(EffectEnv, %d):%s", rank, gdim),
      separable = sprintf("us(2):facv(EnvDummy, %d):%s", rank, gdim),
      diag      = sprintf("diag(EffectEnv):%s", gdim),
      stop("Unknown MET structure: ", structure, call. = FALSE)
    )
    genetic <- sprintf("str(%s, ~ %s)", model_part, cov_part)
  } else {
    # The no-competition baseline must mirror the genetic structure actually
    # fitted, reduced to the direct effects only, or the likelihood-ratio test
    # would compare two models that differ in more than the competition term.
    cov_part <- if (structure == "diag") {
      sprintf("diag(EnvDummy):%s", gdim)
    } else {
      sprintf("facv(EnvDummy, %d):%s", rank, gdim)
    }
    genetic <- sprintf("str(~ Env:%s, ~ %s)", gterm("Geno"), cov_part)
  }

  random <- c(design_terms, genetic)
  if (spatial && nugget) random <- c(random, "idv(units)")

  list(
    random = stats::as.formula(paste("~", paste(random, collapse = " + ")),
                               env = globalenv()),
    residual = stats::as.formula(
      if (spatial) {
        sprintf("~ dsum(~ %s | Env)",
                spatial_residual_text(row_process, col_process))
      } else "~ dsum(~ idv(units) | Env)",
      env = globalenv())
  )
}

#' Ordered nested specifications for the MET fallback ladder.
#'
#' Each step gives up the most expensive assumption still standing: first the
#' factor-analytic rank, then the nugget, then the joint covariance in favour
#' of the separable form, then between-environment correlation altogether, then
#' the design variances, and only last the spatial residual.
#' @noRd
met_specifications <- function(structure, rank, spatial, nugget, design_terms,
                               allow_fallback = TRUE,
                               row_process = "ar1", col_process = "ar1") {
  specs <- list()
  seen <- character(0)
  # See single_specifications(): later steps must inherit a simplified
  # residual process so the ladder stays a sequence of nested models.
  current_row <- row_process
  current_col <- col_process

  add <- function(structure, rank, spatial, nugget, design_terms, reason,
                  rp = current_row, cp = current_col) {
    key <- paste(structure, rank, spatial, nugget,
                 paste(design_terms, collapse = "+"), rp, cp)
    if (key %in% seen) return()
    seen <<- c(seen, key)
    specs[[length(specs) + 1L]] <<- list(
      structure = structure, rank = rank, spatial = spatial, nugget = nugget,
      design_terms = design_terms, reason = reason,
      row_process = rp, col_process = cp)
  }

  add(structure, rank, spatial, nugget, design_terms, "Requested model")
  if (!allow_fallback) return(specs)

  if (spatial && (row_process != "ar1" || col_process != "ar1")) {
    add(structure, rank, spatial, nugget, design_terms,
        "Simplified the residual process to AR1 x AR1", rp = "ar1", cp = "ar1")
    current_row <- "ar1"
    current_col <- "ar1"
  }

  for (r in rev(seq_len(max(1L, rank - 1L)))) {
    add(structure, r, spatial, nugget, design_terms,
        sprintf("Reduced the factor-analytic rank to %d", r))
  }
  if (nugget) {
    add(structure, 1L, spatial, FALSE, design_terms,
        "Dropped the independent nugget variance")
  }
  if (structure != "separable") {
    add("separable", min(rank, 2L), spatial, FALSE, design_terms,
        "Separable us(2) x FA covariance (fewer parameters)")
    add("separable", 1L, spatial, FALSE, design_terms,
        "Separable us(2) x FA(1) covariance")
  }
  if (structure != "fa") {
    add("fa", 1L, spatial, FALSE, design_terms,
        "Separate FA(1) per effect, no direct-competition covariance")
  }
  add("diag", 1L, spatial, FALSE, design_terms,
      "Diagonal genetic covariance: no between-environment correlation")
  if (length(design_terms) > 1L) {
    add("diag", 1L, spatial, FALSE, design_terms[1],
        "Diagonal covariance, replicate term only")
  }
  if (length(design_terms)) {
    add("diag", 1L, spatial, FALSE, character(0),
        "Diagonal covariance with no replicate or block variances")
  }
  if (spatial) {
    add("diag", 1L, FALSE, FALSE, design_terms,
        "Independent residuals within environment instead of a spatial process")
  }
  specs
}

#' Fit the multi-environment competition model
#'
#' Fits environment-specific direct and competitive genotype effects sharing one
#' joint covariance across environments, with an AR1 x AR1 residual per
#' environment. Requires 'ASReml-R' and a valid licence.
#'
#' @param d Prepared MET data from [prepare_trial_data()] with `multi_env =
#'   TRUE`, grid-completed by [complete_field_grid()] for a spatial model, and
#'   carrying the neighbour factors added by [add_neighbours()].
#' @param neighbour_names Names of the neighbour factors, from
#'   [add_neighbours()].
#' @param opts Named list of fitting options: `structure` (one of
#'   `MET_STRUCTURES`), `rank`, `spatial`, `nugget`, `auto_simplify`,
#'   `exact_se`, `compare_baseline`, `maxit`, `workspace`, `cinv_limit` and an
#'   optional `relationship` from [build_relationship()].
#' @param progress Optional `function(i, n, reason)` called as the
#'   simplification ladder advances.
#' @return A list holding the fitted model, the genetic values by environment,
#'   the direct, competitive and pure-stand covariance and correlation
#'   matrices, variance summaries, the fitting log and residuals.
#' @export
fit_met_model <- function(d, neighbour_names, opts, progress = NULL) {
  load_asreml()

  relationship <- opts$relationship
  use_kinship <- !is.null(relationship)
  # Must be a frame-local named exactly as the formula text spells it; see
  # R/04b-relationship.R for why.
  .kinship <- if (use_kinship) relationship$ginv else NULL
  coverage <- NULL
  if (use_kinship) {
    aligned <- align_relationship(d, neighbour_names, relationship)
    d <- aligned$data
    coverage <- aligned$coverage
  }

  d <- add_met_dummy_factors(d)
  effect_levels <- attr(d, "effect_levels")
  env_levels <- levels(d$Env)
  genotypes <- levels(d$Geno)
  n_env <- length(env_levels)
  n_geno <- length(genotypes)
  k <- length(neighbour_names)
  design_terms <- available_design_terms(d)

  if (isTRUE(opts$spatial)) {
    d <- d[order(d$Env, d$Col_i, d$Row_i), , drop = FALSE]
    rownames(d) <- NULL
  }

  n_coef <- n_model_coefficients(d, design_terms, n_geno, k_blocks = 2L * n_env)
  want_cinv <- isTRUE(opts$exact_se) && n_coef <= (opts$cinv_limit %||% 5000L)
  old_options <- asreml::asreml.options(Cinv = want_cinv)
  on.exit(do.call(asreml::asreml.options, old_options), add = TRUE)

  fit_one <- function(spec, competition = TRUE) {
    f <- met_formulae(spec$design_terms, neighbour_names, n_geno, n_env,
                      spec$structure, spec$rank, spec$spatial, spec$nugget,
                      competition, kinship = use_kinship,
                      row_process = spec$row_process %||% "ar1",
                      col_process = spec$col_process %||% "ar1")
    args <- list(
      fixed = stats::as.formula("Yield ~ Env", env = globalenv()),
      random = f$random, residual = f$residual,
      na.action = asreml::na.method(y = "include", x = "include"),
      data = d, maxit = as.integer(opts$maxit), workspace = opts$workspace,
      trace = FALSE, keep.order = TRUE
    )
    if (competition) args$equate.levels <- unname(c("Geno", neighbour_names))
    do.call(asreml::asreml, args)
  }

  specs <- met_specifications(opts$structure, as.integer(opts$rank),
                              isTRUE(opts$spatial), isTRUE(opts$nugget),
                              design_terms, isTRUE(opts$auto_simplify),
                              row_process = opts$row_process %||% "ar1",
                              col_process = opts$col_process %||% "ar1")
  run <- run_fit_ladder(specs, fit_one, isTRUE(opts$auto_simplify), progress)
  fit <- run$fit
  spec <- run$spec

  # For the fa structure the covariance parameters are named after the model
  # terms themselves, so the extractor needs the exact term strings.
  fa_terms <- if (identical(spec$structure, "fa")) {
    gt <- function(f) if (use_kinship) sprintf("vm(%s, .kinship)", f) else f
    c(direct = sprintf("fa(Env, %d):%s", spec$rank, gt("Geno")),
      competition = sprintf("fa(Env, %d):%s", spec$rank, gt(neighbour_names[1])))
  } else NULL

  G <- genetic_covariance_met(fit, spec$structure, effect_levels,
                              env_levels, spec$rank, fa_terms = fa_terms)
  parts <- partition_genetic_covariance(G$matrix, k, labels = env_levels)

  s <- summary(fit, coef = TRUE)
  values <- extract_met_effects(fit, s, env_levels, genotypes, k, parts, want_cinv,
                               kinship = use_kinship,
                               in_trial = levels(droplevels(d$Geno[!is.na(d$Yield)])),
                               fa_rank = if (identical(spec$structure, "fa")) spec$rank else NULL)

  comparison <- NULL
  if (isTRUE(opts$compare_baseline)) {
    baseline <- tryCatch(fit_one(spec, competition = FALSE), error = function(e) NULL)
    if (!is.null(baseline)) {
      comparison <- list(
        table = rbind(fit_statistics(fit, "With competition G x E"),
                      fit_statistics(baseline, "Direct G x E only")),
        lrt = likelihood_ratio_test(fit, baseline,
                                    "With competition G x E", "Direct G x E only"))
    }
  }

  list(
    fit = fit, summary = s, data = d, spec = spec,
    structure_note = G$note,
    environments = env_levels, genotypes = genotypes,
    k = k, neighbour_names = neighbour_names,
    matrices = parts,
    values = values,
    variance = met_variance_table(parts),
    varcomp = variance_component_table(fit),
    fa_summary = if (spec$structure %in% c("facv", "separable", "fa")) {
      fa_variance_explained(fit, spec, effect_levels, env_levels, fa_terms)
    } else NULL,
    correlations_assumed = identical(spec$structure, "diag"),
    # TRUE when the direct-competition covariance is a structural zero rather
    # than an estimate, so the interface can say so instead of reporting 0.
    dc_covariance_fixed = identical(spec$structure, "fa"),
    exact_se = attr(values, "exact_se") %||% FALSE,
    heritability = attr(values, "heritability"),
    comparison = comparison,
    fit_stats = fit_statistics(fit, "Fitted MET competition model"),
    log = run$log, warnings = run$warnings,
    fallback_used = !identical(spec$reason, "Requested model"),
    converged = isTRUE(fit$converge),
    description = describe_met_model(spec, k, n_env, relationship),
    relationship = relationship,
    coverage = coverage,
    residuals = residual_frame(fit, d)
  )
}

#' Extract environment-specific direct and competitive solutions.
#'
#' ASReml labels these coefficients `Env_<environment>:Geno_<genotype>` and
#' `Env_<environment>:N1_<genotype>`. Expected labels are constructed directly,
#' with a reversed-order fallback, which is far more reliable than the pattern
#' guessing the previous version needed and copes correctly with sparse
#' genotype x environment tables, where some cells are simply absent.
#' @noRd
extract_met_effects <- function(fit, s, env_levels, genotypes, k, parts,
                                want_cinv, kinship = FALSE, in_trial = NULL,
                                fa_rank = NULL) {
  cr <- as.data.frame(s$coef.random)
  sol <- solution_column(cr)
  rn <- rownames(cr)

  grid <- expand.grid(Genotype = genotypes, Environment = env_levels,
                      KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)

  # With a relationship matrix ASReml names the term "vm(Geno, .kinship)".
  term_text <- function(f) if (kinship) sprintf("vm(%s, .kinship)", f) else f
  labels_for <- function(term) {
    tt <- term_text(term)
    # An fa() term is labelled "fa(Env, r)_<environment>:<term>_<genotype>",
    # alongside "..._Comp<k>:..." rows holding the latent factor scores, which
    # are not environment effects and must not be picked up here.
    if (!is.null(fa_rank)) {
      fa_lab <- sprintf("fa(Env, %d)_%s:%s_%s", fa_rank, grid$Environment,
                        tt, grid$Genotype)
      if (mean(fa_lab %in% rn) >= 0.5) return(fa_lab)
    }
    forward <- sprintf("Env_%s:%s_%s", grid$Environment, tt, grid$Genotype)
    if (mean(forward %in% rn) >= 0.5) return(forward)
    reversed <- sprintf("%s_%s:Env_%s", tt, grid$Genotype, grid$Environment)
    if (mean(reversed %in% rn) >= 0.5) return(reversed)
    forward
  }
  direct_labels <- labels_for("Geno")
  comp_labels   <- labels_for("N1")

  if (!any(direct_labels %in% rn)) {
    stop("The Env:Geno solutions could not be located in the ASReml ",
         "coefficient table. Open 'Solutions' to inspect the raw output.",
         call. = FALSE)
  }

  out <- grid
  out$Direct_effect <- cr[[sol]][match(direct_labels, rn)]
  out$Competition_effect <- cr[[sol]][match(comp_labels, rn)]

  pev <- if (want_cinv && all(comp_labels %in% rn) && all(direct_labels %in% rn)) {
    tryCatch(pev_direct_competition(fit, direct_labels, comp_labels, k),
             error = function(e) NULL)
  } else NULL

  var_direct <- stats::setNames(diag(parts$direct), env_levels)
  var_pure   <- stats::setNames(diag(parts$pure), env_levels)

  if (!is.null(pev)) {
    out$SE_direct <- sqrt(pmax(pev$pev_direct, 0))
    out$SE_competition <- sqrt(pmax(pev$pev_competition, 0))
    out$SE_pure_stand <- sqrt(pmax(pev$pev_pure, 0))
    out$Reliability_direct <- mapply(function(p, e) reliability(p, var_direct[[e]]),
                                     pev$pev_direct, out$Environment)
    out$Reliability_pure_stand <- mapply(function(p, e) reliability(p, var_pure[[e]]),
                                         pev$pev_pure, out$Environment)
    h2 <- vapply(env_levels, function(e) {
      take <- out$Environment == e
      cullis_h2(pev$pev_direct[take], var_direct[[e]])
    }, numeric(1))
    exact <- TRUE
  } else {
    se_col <- std_error_column(cr)
    if (!is.na(se_col)) {
      out$SE_direct <- cr[[se_col]][match(direct_labels, rn)]
      out$SE_competition <- cr[[se_col]][match(comp_labels, rn)]
    }
    out$SE_pure_stand <- NA_real_
    h2 <- stats::setNames(rep(NA_real_, length(env_levels)), env_levels)
    exact <- FALSE
  }

  means <- met_environment_means(fit, env_levels)
  out$Pure_stand_effect <- out$Direct_effect + k * out$Competition_effect
  out$Environment_mean <- means$Environment_mean[match(out$Environment, means$Environment)]
  out$Predicted_pure_stand_yield <- out$Environment_mean + out$Pure_stand_effect
  out$Competitor_type <- classify_competitor(out$Competition_effect)
  out$Status <- ifelse(is.na(out$Direct_effect) | is.na(out$Competition_effect),
                       "Not estimable", "Estimable")
  if (!is.null(in_trial)) {
    out$Tested <- ifelse(out$Genotype %in% in_trial, "In trial", "Relative only")
  }
  out$Rank_pure_stand <- stats::ave(
    -out$Pure_stand_effect, out$Environment,
    FUN = function(x) rank(x, ties.method = "min", na.last = "keep"))

  out <- out[order(out$Environment, out$Rank_pure_stand, na.last = TRUE), , drop = FALSE]
  rownames(out) <- NULL
  attr(out, "exact_se") <- exact
  attr(out, "heritability") <- h2
  out
}

#' Fitted environment means, used to put pure-stand effects on the yield scale.
#' @noRd
met_environment_means <- function(fit, env_levels) {
  p <- tryCatch(stats::predict(fit, classify = "Env", trace = FALSE)$pvals,
                error = function(e) NULL)
  if (is.null(p)) {
    return(data.frame(Environment = env_levels, Environment_mean = NA_real_,
                      Environment_mean_se = NA_real_, stringsAsFactors = FALSE))
  }
  p <- as.data.frame(p)
  env_col <- grep("^env$", names(p), ignore.case = TRUE, value = TRUE)[1]
  val_col <- grep("predicted", names(p), ignore.case = TRUE, value = TRUE)[1]
  se_col  <- grep("std", names(p), ignore.case = TRUE, value = TRUE)[1]
  if (is.na(env_col) || is.na(val_col)) {
    return(data.frame(Environment = env_levels, Environment_mean = NA_real_,
                      Environment_mean_se = NA_real_, stringsAsFactors = FALSE))
  }
  data.frame(
    Environment = as.character(p[[env_col]]),
    Environment_mean = as.numeric(p[[val_col]]),
    Environment_mean_se = if (!is.na(se_col)) as.numeric(p[[se_col]]) else NA_real_,
    stringsAsFactors = FALSE
  )
}

#' Per-environment genetic variances derived from the joint matrix.
#' @noRd
met_variance_table <- function(parts) {
  data.frame(
    Environment = rownames(parts$direct),
    Direct_variance = diag(parts$direct),
    Competition_variance = diag(parts$competition),
    Direct_competition_covariance = diag(parts$direct_competition),
    Direct_competition_correlation = diag(parts$direct_competition) /
      sqrt(diag(parts$direct) * diag(parts$competition)),
    Pure_stand_variance = diag(parts$pure),
    stringsAsFactors = FALSE, row.names = NULL
  )
}

#' Percentage of genetic variance explained by the factor-analytic factors.
#'
#' The standard summary of an FA multi-environment model (Smith, Cullis &
#' Thompson 2001). An environment whose variance is poorly explained by the
#' common factors behaves idiosyncratically and should not be pooled with the
#' others when making selection decisions.
#' @noRd
fa_variance_explained <- function(fit, spec, effect_levels, env_levels,
                                  fa_terms = NULL) {
  p <- parameter_table(fit)

  if (identical(spec$structure, "fa")) {
    # Two independent fa() terms, each with its own E environment loadings.
    grab_fa <- function(term, level, suffix) {
      hit <- which(endsWith(p$Parameter, paste0(term, "!", level, "!", suffix)))
      if (length(hit)) p$Estimate[hit[1]] else NA_real_
    }
    out <- do.call(rbind, lapply(c("direct", "competition"), function(which_effect) {
      term <- fa_terms[[which_effect]]
      psi <- vapply(env_levels, grab_fa, numeric(1), term = term, suffix = "var")
      load_mat <- vapply(seq_len(spec$rank), function(r) {
        vapply(env_levels, grab_fa, numeric(1), term = term,
               suffix = paste0("fa", r))
      }, numeric(length(env_levels)))
      load_mat <- matrix(load_mat, nrow = length(env_levels), ncol = spec$rank)
      if (anyNA(psi) || anyNA(load_mat)) return(NULL)
      common <- rowSums(load_mat^2)
      d <- data.frame(
        Effect = if (which_effect == "direct") "Direct" else "Competition",
        Environment = env_levels,
        Specific_variance = psi,
        Total_variance = common + psi,
        Variance_explained_pct = 100 * common / (common + psi),
        stringsAsFactors = FALSE, row.names = NULL)
      for (r in seq_len(spec$rank)) d[[paste0("Loading_", r)]] <- load_mat[, r]
      d
    }))
    return(out)
  }

  factor_name <- if (spec$structure == "separable") "EnvDummy" else "EffectEnv"
  levels_used <- if (spec$structure == "separable") env_levels else effect_levels

  grab <- function(level, suffix) {
    key <- paste0("!", factor_name, "_", level, "!", suffix)
    hit <- which(endsWith(p$Parameter, key))
    if (length(hit)) p$Estimate[hit[1]] else NA_real_
  }
  psi <- vapply(levels_used, grab, numeric(1), suffix = "var")
  load_mat <- vapply(seq_len(spec$rank), function(r) {
    vapply(levels_used, grab, numeric(1), suffix = paste0("fa", r))
  }, numeric(length(levels_used)))
  load_mat <- matrix(load_mat, nrow = length(levels_used), ncol = spec$rank)
  if (anyNA(psi) || anyNA(load_mat)) return(NULL)

  common <- rowSums(load_mat^2)
  total <- common + psi
  out <- data.frame(
    Effect = if (spec$structure == "separable") "Environment (shared)"
    else rep(c("Direct", "Competition"), each = length(env_levels)),
    Environment = if (spec$structure == "separable") env_levels
    else rep(env_levels, times = 2),
    Specific_variance = psi,
    Total_variance = total,
    Variance_explained_pct = 100 * common / total,
    stringsAsFactors = FALSE, row.names = NULL
  )
  for (r in seq_len(spec$rank)) out[[paste0("Loading_", r)]] <- load_mat[, r]
  out
}

#' One-sentence description of the fitted MET model.
#' @noRd
describe_met_model <- function(spec, k, n_env, relationship = NULL) {
  genetic <- switch(
    spec$structure,
    facv = sprintf("joint FA(%d) covariance over %d direct and %d competitive environment effects",
                   spec$rank, n_env, n_env),
    fa = sprintf(paste("separate FA(%d) covariances for the direct and",
                       "competitive effects, with no direct-competition",
                       "covariance"), spec$rank),
    separable = sprintf("separable us(2) x FA(%d) genetic covariance", spec$rank),
    diag = "diagonal genetic covariance with no between-environment correlation"
  )
  paste0(
    genetic,
    if (length(spec$design_terms)) {
      paste0(", random ", paste(pretty_term(spec$design_terms), collapse = " + "))
    } else ", no replicate or block variances",
    if (spec$spatial) {
      paste0(", environment-specific ",
             describe_residual(spec$row_process %||% "ar1",
                               spec$col_process %||% "ar1"))
    } else ", independent residuals within environment",
    if (spec$nugget) " with a common nugget" else "",
    if (!is.null(relationship)) paste0(", ", relationship$label) else
      ", independent genotypes",
    sprintf("; %d competing neighbours", k)
  )
}
