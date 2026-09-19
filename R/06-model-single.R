# ---------------------------------------------------------------------------
# Single-trial inter-plot competition model
#
# Model (Besag & Kempton 1986; Stringer, Cullis & Thompson 2011):
#
#   y_i = mu + [design] + d_g(i) + sum_{j in N(i)} c_g(j) + s_i + e_i
#
#   d_g  direct effect     - how genotype g performs in its own plot
#   c_g  competitive effect - how genotype g alters a neighbour's plot
#   s_i  AR1 x AR1 spatial residual
#
# In ASReml-R the two genetic effect sets are tied into one variance structure:
#
#   random   = ~ str(~ Geno + N1 + and(N2), ~ us(2):id(nGeno))
#   residual = ~ ar1(Column):ar1(Row)
#
# `and()` adds N2's design matrix onto N1's rather than creating new effects,
# so both neighbours of a plot draw on the same competitive effect vector.
# `equate.levels` forces Geno, N1 and N2 to share one genotype level set, so
# effect 7 in the direct block and effect 7 in the competitive block are the
# same genotype. `us(2)` then estimates Var(d), Var(c) and Cov(d,c) directly.
#
# WHY us(2) REPLACED facv(DirectCompetition, 1)
# The previous version fitted a one-factor analytic covariance and needed a
# synthetic two-level `DirectCompetition` factor, assigned by alternating rows
# of the data, purely to give facv() a factor argument. With only two
# dimensions, FA(1) has two loadings plus two specific variances - four
# parameters for a 2 x 2 matrix that has only three. It is therefore not more
# parsimonious than unstructured, and it is *over*-parameterised, which is the
# main reason the old app kept hitting singular Average Information matrices.
# `us(2)` is the canonical ASReml idiom here: three parameters, directly
# interpretable, no synthetic factor, and no fragile positional parsing of
# loadings. `corgh(2)` is offered as a better-conditioned reparameterisation
# for boundary cases, and `diag(2)` as the nested independence model.
# ---------------------------------------------------------------------------

SINGLE_STRUCTURES <- c(
  "Unstructured us(2) \u2014 recommended"            = "us",
  "Correlation + heterogeneous variance corgh(2)"    = "corgh",
  "Independent direct and competitive effects"       = "diag"
)

#' Build the random and residual formulae for one specification.
#'
#' @param design_terms character vector of design factors to include
#' @param neighbour_names N1..Nk
#' @param n_geno number of genotype levels
#' @param structure "us", "corgh" or "diag"
#' @param spatial fit AR1 x AR1?
#' @param nugget add an independent plot-level variance alongside AR1?
#' @param competition include the competitive effects at all?
#' @noRd
single_formulae <- function(design_terms, neighbour_names, n_geno,
                            structure = "us", spatial = TRUE, nugget = TRUE,
                            competition = TRUE, kinship = FALSE,
                            row_process = "ar1", col_process = "ar1") {
  # With a relationship matrix every genetic factor is wrapped in vm(), and the
  # genotype dimension of the str() variance formula becomes vm(Geno, .kinship)
  # instead of id(n). Leaving id(n) there is accepted by ASReml but silently
  # returns the independent-genotype answer.
  gterm <- function(f) if (kinship) sprintf("vm(%s, .kinship)", f) else f
  gdim  <- if (kinship) "vm(Geno, .kinship)" else sprintf("id(%d)", n_geno)

  if (competition) {
    # N1 carries the effects; the remaining neighbours are folded onto it.
    neighbour_sum <- paste(
      c(gterm(neighbour_names[1]),
        sprintf("and(%s)", gterm(neighbour_names[-1]))),
      collapse = " + "
    )
    genetic <- if (structure == "diag") {
      paste(gterm("Geno"), "+", neighbour_sum)
    } else {
      sprintf("str(~ %s + %s, ~ %s(2):%s)", gterm("Geno"), neighbour_sum,
              structure, gdim)
    }
  } else {
    genetic <- gterm("Geno")
  }

  random <- c(design_terms, genetic)
  if (spatial && nugget) random <- c(random, "idv(units)")

  list(
    random = stats::as.formula(paste("~", paste(random, collapse = " + ")),
                               env = globalenv()),
    residual = if (spatial) {
      stats::as.formula(paste("~", spatial_residual_text(row_process, col_process)),
                        env = globalenv())
    } else {
      stats::as.formula("~ idv(units)", env = globalenv())
    }
  )
}

#' Ordered list of nested specifications for the fallback ladder.
#'
#' The ordering removes the least defensible assumption first: the
#' direct-competition covariance is the hardest parameter to estimate, the
#' nugget is often weakly identified alongside AR1, and the spatial residual is
#' given up last because dropping it biases everything else.
#' @noRd
single_specifications <- function(structure, spatial, nugget, design_terms,
                                  allow_fallback = TRUE,
                                  row_process = "ar1", col_process = "ar1") {
  specs <- list()
  seen <- character(0)
  # The residual process in force for the remainder of the ladder. Once it
  # has been simplified, every later step must inherit the simpler process,
  # or the ladder stops being a sequence of nested models.
  current_row <- row_process
  current_col <- col_process

  add <- function(structure, spatial, nugget, design_terms, reason,
                  rp = current_row, cp = current_col) {
    key <- paste(structure, spatial, nugget,
                 paste(design_terms, collapse = "+"), rp, cp)
    if (key %in% seen) return()
    seen <<- c(seen, key)
    specs[[length(specs) + 1L]] <<- list(
      structure = structure, spatial = spatial, nugget = nugget,
      design_terms = design_terms, reason = reason,
      row_process = rp, col_process = cp
    )
  }

  add(structure, spatial, nugget, design_terms, "Requested model")
  if (!allow_fallback) return(specs)

  # A second-order residual process is the first assumption to give up: it
  # is the most recently added and the least costly to lose.
  if (spatial && (row_process != "ar1" || col_process != "ar1")) {
    add(structure, spatial, nugget, design_terms,
        "Simplified the residual process to AR1 x AR1",
        rp = "ar1", cp = "ar1")
    current_row <- "ar1"
    current_col <- "ar1"
  }

  if (structure == "us") {
    add("corgh", spatial, nugget, design_terms,
        "Reparameterised the genetic covariance as corgh(2)")
  }
  if (nugget) {
    add(structure, spatial, FALSE, design_terms,
        "Dropped the independent nugget variance")
    add("corgh", spatial, FALSE, design_terms,
        "corgh(2) genetic covariance without the nugget")
  }
  add("diag", spatial, FALSE, design_terms,
      "Constrained the direct-competition covariance to zero")
  if (length(design_terms) > 1L) {
    add("diag", spatial, FALSE, design_terms[1],
        "Independent genetic effects, keeping only the replicate term")
  }
  if (length(design_terms)) {
    add("diag", spatial, FALSE, character(0),
        "Independent genetic effects with no replicate or block variances")
  }
  if (spatial) {
    add("diag", FALSE, FALSE, design_terms,
        "Replaced the spatial residual with an independent residual")
  }
  specs
}

#' Fit the single-trial competition model.
#'
#' @param d prepared trial data (already grid-completed if spatial)
#' @param neighbour_names N1..Nk
#' @param opts list of user options
#' @param progress optional function(i, n, reason) for the busy indicator
#' @return a rich result list consumed by the UI
#' @export
fit_single_model <- function(d, neighbour_names, opts, progress = NULL) {
  load_asreml()

  relationship <- opts$relationship
  use_kinship <- !is.null(relationship)
  # ASReml resolves the second argument of vm() from the calling frame, so the
  # object must be an ordinary local named exactly as the formula text spells
  # it. `fit_one()` below closes over this binding.
  .kinship <- if (use_kinship) relationship$ginv else NULL

  if (use_kinship) {
    aligned <- align_relationship(d, neighbour_names, relationship)
    d <- aligned$data
    coverage <- aligned$coverage
  } else {
    coverage <- NULL
  }

  design_terms <- available_design_terms(d)
  n_geno <- nlevels(d$Geno)
  k <- length(neighbour_names)

  # ar1(Column):ar1(Row) requires the data sorted with Row varying fastest
  # within Column. Neighbour lookup is key-based, so ordering is safe here.
  if (isTRUE(opts$spatial)) {
    d <- d[order(d$Col_i, d$Row_i), , drop = FALSE]
    rownames(d) <- NULL
  }

  # Request the inverse coefficient matrix only when it is affordable: it is
  # dense and scales with the square of the number of model coefficients.
  n_coef <- n_model_coefficients(d, design_terms, n_geno)
  want_cinv <- isTRUE(opts$exact_se) && n_coef <= (opts$cinv_limit %||% 5000L)
  old_options <- asreml::asreml.options(Cinv = want_cinv)
  on.exit(do.call(asreml::asreml.options, old_options), add = TRUE)

  # A covariate enters as fixed covariates. Both the
  # competition model and its no-competition baseline carry them, so the
  # likelihood-ratio test still compares two models with identical fixed
  # effects and tests only the genetic competitive effects.
  covariate_terms <- covariate_fixed_terms(d, opts$adjust_own_covariate)
  fixed_text <- paste(c("Yield ~ 1", covariate_terms), collapse = " + ")

  fit_one <- function(spec, competition = TRUE) {
    f <- single_formulae(spec$design_terms, neighbour_names, n_geno,
                         spec$structure, spec$spatial, spec$nugget, competition,
                         kinship = use_kinship,
                         row_process = spec$row_process %||% "ar1",
                         col_process = spec$col_process %||% "ar1")
    args <- list(
      fixed = stats::as.formula(fixed_text, env = globalenv()),
      random = f$random, residual = f$residual,
      na.action = asreml::na.method(y = "include", x = "include"),
      data = d, maxit = as.integer(opts$maxit), workspace = opts$workspace,
      trace = FALSE, keep.order = TRUE
    )
    # equate.levels is meaningful only when neighbour factors are in the model.
    if (competition) args$equate.levels <- unname(c("Geno", neighbour_names))
    do.call(asreml::asreml, args)
  }

  specs <- single_specifications(opts$structure, isTRUE(opts$spatial),
                                 isTRUE(opts$nugget), design_terms,
                                 isTRUE(opts$auto_simplify),
                                 row_process = opts$row_process %||% "ar1",
                                 col_process = opts$col_process %||% "ar1")
  run <- run_fit_ladder(specs, fit_one, isTRUE(opts$auto_simplify), progress)
  fit <- run$fit
  spec <- run$spec

  # Keep restarting from the current estimates until ASReml reports
  # convergence: a model that merely ran out of iterations is not safe to quote.
  extra <- iterate_to_convergence(fit, opts$max_rounds %||% 15L,
                                  progress = function(r) {
                                    if (is.function(progress)) {
                                      progress(length(specs), length(specs),
                                               sprintf("Continuing to convergence (round %d)", r))
                                    }
                                  })
  fit <- extra$fit

  # ---- genetic covariance ------------------------------------------------
  G <- genetic_covariance_2x2(fit, spec$structure)
  # For a single trial each partitioned block is 1 x 1, labelled by the trial.
  parts <- partition_genetic_covariance(G$matrix, k, labels = levels(d$Env)[1])
  var_direct <- parts$direct[1, 1]
  var_comp   <- parts$competition[1, 1]
  cov_dc     <- parts$direct_competition[1, 1]
  var_pure   <- parts$pure[1, 1]
  cor_dc     <- if (var_direct > 0 && var_comp > 0) {
    max(-1, min(1, cov_dc / sqrt(var_direct * var_comp)))
  } else NA_real_

  # ---- genotype solutions -------------------------------------------------
  s <- summary(fit, coef = TRUE)
  genetic <- extract_single_effects(fit, s, levels(d$Geno), k,
                                    var_direct, var_pure,
                                    kinship = use_kinship,
                                    in_trial = levels(droplevels(
                                      d$Geno[!is.na(d$Yield)])))

  # ---- baseline comparison ------------------------------------------------
  # A competition model is only worth reporting if competition improves the
  # fit. The reduced model keeps the identical fixed and residual structure and
  # differs only by the competitive effects, so a REML LRT is valid.
  comparison <- NULL
  if (isTRUE(opts$compare_baseline)) {
    baseline <- tryCatch(fit_one(spec, competition = FALSE), error = function(e) NULL)
    if (!is.null(baseline)) {
      comparison <- list(
        table = rbind(
          fit_statistics(fit, "With competition"),
          fit_statistics(baseline, "Direct effects only")
        ),
        lrt = likelihood_ratio_test(fit, baseline,
                                    "With competition", "Direct effects only")
      )
    }
  }

  list(
    fit = fit,
    summary = s,
    data = d,
    spec = spec,
    structure_note = G$note,
    neighbour_names = neighbour_names,
    k = k,
    genetic = genetic,
    variance = single_variance_table(fit, parts, spec, k),
    varcomp = variance_component_table(fit),
    components = list(direct = var_direct, competition = var_comp,
                      covariance = cov_dc, correlation = cor_dc,
                      pure = var_pure),
    heritability = attr(genetic, "heritability"),
    relationship = relationship,
    coverage = coverage,
    exact_se = attr(genetic, "exact_se"),
    comparison = comparison,
    fit_stats = fit_statistics(fit, "Fitted competition model"),
    fixed_effects = fixed_effects_table(s),
    wald = wald_table(fit),
    convergence_rounds = extra$rounds,
    covariate_terms = covariate_terms,
    covariate_name = attr(d, "covariate_name"),
    log = run$log,
    warnings = run$warnings,
    fallback_used = !identical(spec$reason, "Requested model"),
    converged = isTRUE(fit$converge),
    description = describe_single_model(spec, k, relationship, covariate_terms),
    residuals = residual_frame(fit, d)
  )
}

#' Pull direct and competitive solutions and assemble the genotype table.
#' @noRd
extract_single_effects <- function(fit, s, genotypes, k, var_direct, var_pure,
                                   kinship = FALSE, in_trial = NULL) {
  cr <- as.data.frame(s$coef.random)
  sol <- solution_column(cr)
  se_col <- std_error_column(cr)

  # ASReml names vm() coefficients after the full term text, for example
  # "vm(Geno, .kinship)_MZ001".
  prefix <- function(f) if (kinship) sprintf("vm(%s, .kinship)_", f) else paste0(f, "_")
  direct_labels <- paste0(prefix("Geno"), genotypes)
  comp_labels   <- paste0(prefix("N1"), genotypes)
  present <- direct_labels %in% rownames(cr)
  if (!any(present)) {
    stop("The direct genotype solutions could not be located in the ASReml ",
         "coefficient table. Open 'Solutions' to inspect the raw output.",
         call. = FALSE)
  }
  has_comp <- all(comp_labels %in% rownames(cr))

  out <- data.frame(
    Genotype = genotypes,
    Direct_effect = cr[[sol]][match(direct_labels, rownames(cr))],
    stringsAsFactors = FALSE
  )
  out$Competition_effect <- if (has_comp) {
    cr[[sol]][match(comp_labels, rownames(cr))]
  } else NA_real_

  # Exact prediction error variances via the inverse coefficient matrix.
  pev <- if (has_comp) {
    tryCatch(pev_direct_competition(fit, direct_labels, comp_labels, k),
             error = function(e) NULL)
  } else NULL

  if (!is.null(pev)) {
    out$SE_direct <- sqrt(pmax(pev$pev_direct, 0))
    out$SE_competition <- sqrt(pmax(pev$pev_competition, 0))
    out$SE_pure_stand <- sqrt(pmax(pev$pev_pure, 0))
    out$Reliability_direct <- reliability(pev$pev_direct, var_direct)
    out$Reliability_pure_stand <- reliability(pev$pev_pure, var_pure)
    h2 <- c(direct = cullis_h2(pev$pev_direct, var_direct),
            pure   = cullis_h2(pev$pev_pure, var_pure))
    exact <- TRUE
  } else {
    if (!is.na(se_col)) {
      out$SE_direct <- cr[[se_col]][match(direct_labels, rownames(cr))]
      out$SE_competition <- if (has_comp) {
        cr[[se_col]][match(comp_labels, rownames(cr))]
      } else NA_real_
      out$Reliability_direct <- reliability(out$SE_direct^2, var_direct)
    }
    # No pure-stand SE is reported without the exact covariance. Combining the
    # two standard errors as though independent would be badly wrong, because
    # the direct and competitive solutions for a genotype are strongly
    # negatively correlated.
    out$SE_pure_stand <- NA_real_
    out$Reliability_pure_stand <- NA_real_
    h2 <- c(direct = cullis_h2(out$SE_direct^2, var_direct), pure = NA_real_)
    exact <- FALSE
  }

  intercept <- extract_intercept(fit, s)
  out$Pure_stand_effect <- out$Direct_effect + k * out$Competition_effect
  out$Predicted_pure_stand_yield <- intercept + out$Pure_stand_effect
  # Ranked on predicted pure-stand performance, the quantity a breeder selects
  # on, rather than on the effect alone. The two orderings coincide for a
  # single trial, where the intercept is common, but the yield scale is what
  # the ranking should be seen to be built from.
  rank_basis <- if (all(is.na(out$Predicted_pure_stand_yield))) {
    out$Pure_stand_effect
  } else {
    out$Predicted_pure_stand_yield
  }
  out$Rank_direct <- rank(-out$Direct_effect, ties.method = "min", na.last = "keep")
  out$Rank_pure_stand <- rank(-rank_basis, ties.method = "min", na.last = "keep")
  out$Rank_change <- out$Rank_direct - out$Rank_pure_stand
  out$Competitor_type <- classify_competitor(out$Competition_effect)
  if (!is.null(in_trial)) {
    # vm() predicts every individual in the relationship matrix, including
    # ancestors with no plot. Their values are genuine predictions from
    # relatives, but a breeder must be able to tell them apart.
    out$Tested <- ifelse(out$Genotype %in% in_trial, "In trial", "Relative only")
  }

  out <- out[order(out$Rank_pure_stand, na.last = TRUE), , drop = FALSE]
  rownames(out) <- NULL
  attr(out, "heritability") <- h2
  attr(out, "exact_se") <- exact
  attr(out, "intercept") <- intercept
  out
}

#' Label a genotype by the sign and size of its competitive effect.
#'
#' Thresholded at one standard deviation so the labels stay meaningful rather
#' than splitting the panel at zero.
#' @noRd
classify_competitor <- function(x) {
  if (all(is.na(x))) return(rep(NA_character_, length(x)))
  sd_x <- stats::sd(x, na.rm = TRUE)
  # Short labels: the full meaning is given once beneath the table, and long
  # strings force every row of the results table to wrap.
  ifelse(is.na(x), NA_character_,
         ifelse(x >  sd_x, "Benign",
                ifelse(x < -sd_x, "Aggressive", "Neutral")))
}

#' Fitted intercept, needed to put pure-stand effects back on the yield scale.
#' @noRd
extract_intercept <- function(fit, s) {
  cf <- s$coef.fixed
  if (!is.null(cf)) {
    cf <- as.data.frame(cf)
    sol <- solution_column(cf)
    hit <- grep("intercept|(^|[!_:])mu($|[!_:])", rownames(cf), ignore.case = TRUE)
    if (!length(hit) && nrow(cf) == 1L) hit <- 1L
    if (length(hit)) return(as.numeric(cf[[sol]][hit[1]]))
  }
  fx <- fit$coefficients$fixed
  if (!is.null(fx) && length(fx)) {
    hit <- grep("intercept|(^|[!_:])mu($|[!_:])", names(fx) %||% "", ignore.case = TRUE)
    if (!length(hit) && length(fx) == 1L) hit <- 1L
    if (length(hit)) return(as.numeric(fx[hit[1]]))
  }
  NA_real_
}

#' Interpreted variance summary for the single trial.
#' @noRd
single_variance_table <- function(fit, parts, spec, k) {
  p <- parameter_table(fit)
  is_var <- p$Type %in% c("V", "P", "G")

  pick <- function(pattern, exclude = NULL) {
    hit <- is_var & grepl(pattern, p$Parameter, ignore.case = TRUE)
    if (!is.null(exclude)) hit <- hit & !grepl(exclude, p$Parameter, ignore.case = TRUE)
    if (any(hit)) p$Estimate[which(hit)[1]] else NA_real_
  }

  # With no `v` on either axis the residual scale is ASReml's sigma2 rather
  # than a named structure parameter.
  spatial_var <- if (spec$spatial) as.numeric(fit$sigma2 %||% NA_real_) else NA_real_
  nugget_var <- if (spec$nugget) pick("units", "Geno|N1") else 0
  residual_var <- if (spec$spatial) spatial_var else pick("units", "Geno|N1")
  total_error <- if (spec$spatial) sum(c(spatial_var, nugget_var), na.rm = TRUE) else residual_var

  var_d <- parts$direct[1, 1]
  var_c <- parts$competition[1, 1]
  cov_dc <- parts$direct_competition[1, 1]
  var_ps <- parts$pure[1, 1]
  cor_dc <- if (var_d > 0 && var_c > 0) cov_dc / sqrt(var_d * var_c) else NA_real_

  data.frame(
    Component = c(
      "Direct genetic variance",
      "Competitive genetic variance",
      "Direct-competition covariance",
      "Direct-competition correlation",
      "Pure-stand genetic variance",
      if (spec$spatial) {
        sprintf("Spatial (%s x %s) variance",
                toupper(spec$col_process %||% "ar1"),
                toupper(spec$row_process %||% "ar1"))
      } else "Residual variance",
      "Independent nugget variance",
      "Total plot-level error variance"
    ),
    Estimate = c(var_d, var_c, cov_dc, cor_dc, var_ps,
                 residual_var, nugget_var, total_error),
    Interpretation = c(
      "Genetic variation in a genotype's own plot yield",
      "Genetic variation in the effect a genotype has on its neighbours",
      "Covariance between a genotype's own yield and its effect on neighbours",
      "Negative values indicate that high-yielding genotypes suppress neighbours",
      sprintf("Var(D + %d C): genetic variance expressed in a pure stand", k),
      if (spec$spatial) {
        paste("Diagonal variance of the separable field trend:",
              describe_residual(spec$row_process %||% "ar1",
                                spec$col_process %||% "ar1"))
      } else "Independent plot-to-plot error",
      if (spec$nugget) "Plot-level variation independent of the spatial trend"
      else "Not fitted",
      if (spec$spatial) "Spatial variance + nugget" else "Residual variance"
    ),
    stringsAsFactors = FALSE
  )
}

#' One-sentence description of what was actually fitted.
#' @noRd
describe_single_model <- function(spec, k, relationship = NULL,
                                 covariate_terms = character(0)) {
  genetic <- switch(
    spec$structure,
    us    = "unstructured us(2) direct-competition covariance",
    corgh = "corgh(2) direct-competition covariance",
    diag  = "independent direct and competitive effects"
  )
  paste0(
    genetic,
    if (length(spec$design_terms)) {
      paste0(", random ", paste(pretty_term(spec$design_terms), collapse = " + "))
    } else ", no replicate or block variances",
    if (spec$spatial) {
      paste0(", ", describe_residual(spec$row_process %||% "ar1",
                                     spec$col_process %||% "ar1"))
    } else ", independent residual",
    if (spec$nugget) " with nugget" else "",
    if (!is.null(relationship)) paste0(", ", relationship$label) else
      ", independent genotypes",
    if (length(covariate_terms)) {
      paste0(", adjusted for ",
             if ("Covariate_own" %in% covariate_terms) {
               "the neighbouring and own-plot values of the covariate"
             } else "the neighbouring plots' covariate")
    } else "",
    sprintf("; %d competing neighbours", k)
  )
}

#' Residuals and fitted values joined to the field layout, for diagnostics.
#' @noRd
residual_frame <- function(fit, d) {
  r <- as.numeric(stats::residuals(fit))
  f <- as.numeric(stats::fitted(fit))
  n <- nrow(d)
  if (length(r) != n || length(f) != n) return(NULL)
  out <- data.frame(
    Env = d$Env, Row = d$Row_i, Column = d$Col_i,
    Genotype = as.character(d$Geno), Observed = d$Yield,
    Fitted = f, Residual = r, Padded = d$Padded,
    stringsAsFactors = FALSE
  )
  out$Std_residual <- out$Residual / stats::sd(out$Residual[!out$Padded], na.rm = TRUE)
  out[!out$Padded & !is.na(out$Observed), , drop = FALSE]
}

#' Sample variogram of the residuals, when ASReml can supply one.
#'
#' The AR1 x AR1 sample variogram is the standard diagnostic for whether the
#' separable spatial model has captured the field trend (Gilmour, Cullis &
#' Verbyla 1997): a well-fitted surface gives a variogram that rises to a
#' plateau without ridges or trends along either axis.
#' @noRd
residual_variogram <- function(fit) {
  tryCatch({
    v <- as.data.frame(asreml::varioGram(fit))
    names(v)[names(v) == "gamma"] <- "Semivariance"
    v
  }, error = function(e) NULL)
}
