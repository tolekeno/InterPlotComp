# ---------------------------------------------------------------------------
# ASReml-R interface shared by the single-trial and MET workspaces
#
# LICENSING
# ASReml-R is commercial software licensed by VSNi. This application is only a
# front end: it loads whatever ASReml-R is installed in the R library of the
# process running the app, and relies entirely on the licence already activated
# for that machine and user. It never reads, writes, stores, embeds, transmits
# or renews a licence key, and it has no fallback engine.
# ---------------------------------------------------------------------------

# Error text that indicates an over-parameterised but nested-simplifiable model
# rather than a genuine user error. Only these trigger the fallback ladder.
RECOVERABLE_PATTERNS <- paste(
  "Average Information", "AI matrix", "singularit", "singular",
  "not positive definite", "positive definite", "fixed at a boundary",
  "did not converge", "Gvalues", "unable to invert",
  # ASReml reports an over-parameterised variance model that diverges as
  # "too many exceptions" or an aborted estimation. Both are simplifiable.
  "too many exceptions", "estimation was aborted", "abort",
  "exceeded the maximum", "workspace",
  sep = "|"
)

# ---------------------------------------------------------------------------
# Spatial residual processes
# ---------------------------------------------------------------------------

# Correlation processes offered for each field axis.
#
# WHY MORE THAN AR1
# Inter-plot competition does not only move genetic signal between plots, it
# leaves a signature in the residuals along the competition direction: a plot
# that gives up yield to its neighbour is negatively correlated with it at
# lag 1, while lag 2 is positive. AR1 imposes a geometric decay that keeps one
# sign, so it cannot represent that pattern and the unmodelled part is pushed
# into the competitive effects. A second-order process can: AR2 estimates two
# free correlations, and SAR2 is its symmetric-autoregressive counterpart,
# which is often better behaved on short field axes. Both are standard choices
# for trials with interference (Besag & Kempton 1986; Gleeson & Cullis 1987).
RESIDUAL_PROCESSES <- c(
  "AR1 \u2014 first-order autoregressive (default)"    = "ar1",
  "AR2 \u2014 second-order, allows negative lag 1"      = "ar2",
  "SAR \u2014 symmetric autoregressive"                 = "sar",
  "SAR2 \u2014 second-order symmetric autoregressive"   = "sar2",
  "Independent \u2014 no correlation on this axis"      = "id"
)

#' Residual formula text for a separable two-dimensional field process.
#'
#' The scale parameter is carried by the column term, so exactly one variance
#' is estimated however the two axes are specified.
#'
#' @param row_process,col_process names from `RESIDUAL_PROCESSES`
#' @noRd
spatial_residual_text <- function(row_process = "ar1", col_process = "ar1") {
  sprintf("%sv(Column):%s(Row)", col_process, row_process)
}

#' Plain-English description of a separable residual specification.
#' @noRd
describe_residual <- function(row_process, col_process) {
  label <- function(p) toupper(p)
  if (identical(row_process, "id") && identical(col_process, "id")) {
    return("independent residuals")
  }
  sprintf("%s x %s spatial residual (columns x rows)",
          label(col_process), label(row_process))
}

#' Is ASReml-R installed in this R library?
#' @noRd
asreml_installed <- function() has_pkg("asreml")

#' Installed ASReml-R version, or NA.
#' @noRd
asreml_version <- function() {
  if (!asreml_installed()) return(NA_character_)
  tryCatch(as.character(utils::packageVersion("asreml")), error = function(e) NA_character_)
}

#' Attach ASReml-R, converting a licence failure into an actionable message.
#'
#' ASReml's special model functions (`str`, `and`, `us`, `corgh`, `facv`, `id`,
#' `ar1v`, `dsum`) are resolved when the model formulae are evaluated, so the
#' package must be attached rather than merely namespace-loaded.
#' @noRd
load_asreml <- function() {
  if (!asreml_installed()) {
    stop(
      "ASReml-R is not installed in the R library used by this application.\n",
      "Install it from the package supplied by VSNi, then restart the app in ",
      "the same R installation. Checked libraries:\n  ",
      paste(.libPaths(), collapse = "\n  "),
      call. = FALSE
    )
  }
  ok <- tryCatch({
    suppressPackageStartupMessages(library("asreml", character.only = TRUE))
    TRUE
  }, error = function(e) conditionMessage(e))

  if (!isTRUE(ok)) {
    stop(
      "ASReml-R is installed but could not be loaded, which normally means the ",
      "licence is missing, expired, or not activated for the operating-system ",
      "user running this process.\n\nASReml reported:\n", ok,
      "\n\nActivate the licence in a plain R session with ",
      "asreml::asreml.license.activate(), then restart the application. ",
      "This application never handles licence keys itself.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' One-line description of the ASReml installation for the UI.
#' @noRd
asreml_status <- function() {
  if (!asreml_installed()) {
    return(list(ok = FALSE, level = "bad",
                title = "ASReml-R is not available",
                detail = paste0("No 'asreml' package was found in: ",
                                paste(.libPaths(), collapse = "; "),
                                ". Install and activate ASReml-R 4.x, then restart R.")))
  }
  list(ok = TRUE, level = "ok",
       title = paste0("ASReml-R ", asreml_version(), " is installed"),
       detail = paste("The licence is checked by ASReml when the first model is",
                      "fitted. This application does not read or store licence keys."))
}

# ---------------------------------------------------------------------------
# Variance parameters
# ---------------------------------------------------------------------------

#' Tidy `fit$vparameters` with the constraint codes attached.
#' @noRd
parameter_table <- function(fit) {
  v <- fit$vparameters
  if (is.null(v) || !length(v)) return(data.frame())
  type <- fit$vparameters.type
  if (is.null(type) || length(type) != length(v)) type <- rep(NA_character_, length(v))
  con <- fit$vparameters.con
  if (is.null(con) || length(con) != length(v)) con <- rep(NA_character_, length(v))
  data.frame(
    Parameter = names(v) %||% paste0("par", seq_along(v)),
    Estimate  = as.numeric(v),
    Type      = toupper(as.character(type)),
    Constraint = as.character(con),
    stringsAsFactors = FALSE
  )
}

#' Publication-ready variance-component table.
#'
#' Adds the proportion of total variance contributed by each component, which
#' is what breeders normally want to read off, and flags components sitting at
#' a boundary because those invalidate the usual standard errors.
#' @noRd
variance_component_table <- function(fit) {
  vc <- as.data.frame(summary(fit)$varcomp)
  vc$Component <- rownames(vc)
  rownames(vc) <- NULL
  names(vc)[names(vc) == "component"] <- "Estimate"
  names(vc)[names(vc) == "std.error"] <- "Std_error"
  names(vc)[names(vc) == "z.ratio"]   <- "Z_ratio"
  names(vc)[names(vc) == "bound"]     <- "Bound"
  names(vc)[names(vc) == "%ch"]       <- "Pct_change"

  is_variance <- !grepl("cor$|\\.cor|!R$", vc$Component)
  total <- sum(vc$Estimate[is_variance & vc$Estimate > 0], na.rm = TRUE)
  vc$Pct_of_total <- ifelse(is_variance, safe_pct(vc$Estimate, total), NA_real_)
  vc$At_boundary <- vc$Bound %in% c("B", "F", "S")

  # Internal factor names are replaced with breeder-facing wording; the raw
  # ASReml label is kept alongside so the output can still be traced back.
  vc$Term <- pretty_term(sub("!.*$", "", vc$Component))
  keep <- c("Component", "Term", "Estimate", "Std_error", "Z_ratio", "Pct_of_total",
            "Bound", "At_boundary", "Pct_change")
  vc[, intersect(keep, names(vc)), drop = FALSE]
}

#' Fail with a consistent, actionable message when extraction is impossible.
#' @noRd
stop_extract <- function(what) {
  stop("Could not reconstruct ", what, " from the ASReml variance parameters. ",
       "Open the 'Variance components' tab to see exactly what was estimated, ",
       "and report the parameter names if this persists.", call. = FALSE)
}

#' Genetic covariance of the direct and competitive effects in a single trial.
#'
#' ASReml names the parameters of a `str()` block after the model string, for
#' example `Geno+N1+and(N2)!us(2)_2:1`. Identification is therefore by name,
#' not by position: position depends on term order and on which design terms
#' survived the fallback ladder.
#'
#' @param fit fitted asreml object
#' @param structure "us", "corgh" or "diag"
#' @return list(matrix = 2 x 2 covariance, note = plain-English description)
#' @noRd
genetic_covariance_2x2 <- function(fit, structure) {
  p <- parameter_table(fit)
  if (!nrow(p)) stop("ASReml returned no variance parameters.", call. = FALSE)
  out <- matrix(NA_real_, 2L, 2L)

  if (structure == "us") {
    block <- p[grepl("!us(2)_", p$Parameter, fixed = TRUE), , drop = FALSE]
    idx <- sub(".*!us\\(2\\)_", "", block$Parameter)
    ij <- strsplit(idx, ":", fixed = TRUE)
    ok <- lengths(ij) == 2L
    if (sum(ok) < 3L) stop_extract("the unstructured genetic covariance")
    for (r in which(ok)) {
      i <- as.integer(ij[[r]][1]); j <- as.integer(ij[[r]][2])
      out[i, j] <- out[j, i] <- block$Estimate[r]
    }
    note <- "Unstructured us(2): Var(direct), Cov(direct, competition), Var(competition) estimated directly."

  } else if (structure == "corgh") {
    block <- p[grepl("!corgh(2)", p$Parameter, fixed = TRUE), , drop = FALSE]
    is_cor <- grepl("cor$", block$Parameter)
    v <- block$Estimate[!is_cor]
    r <- block$Estimate[is_cor]
    if (length(v) < 2L || !length(r)) stop_extract("the corgh(2) genetic covariance")
    out[1, 1] <- v[1]; out[2, 2] <- v[2]
    out[1, 2] <- out[2, 1] <- r[1] * sqrt(v[1] * v[2])
    note <- "corgh(2): heterogeneous variances with an estimated direct-competition correlation."

  } else {
    # The independence model is fitted as plain `Geno + N1 + and(N2)`, so the
    # two variances appear under their own term names.
    exclude <- "Rep|Block|units|Row|Column|!R$|residual"
    is_var <- p$Type %in% c("V", "P", "G") & !grepl(exclude, p$Parameter, ignore.case = TRUE)
    d <- p$Estimate[is_var & grepl("^Geno($|!)", p$Parameter)]
    c <- p$Estimate[is_var & grepl("^N1($|!)", p$Parameter)]
    if (!length(d) || !length(c)) stop_extract("the independent genetic variances")
    out[1, 1] <- d[1]; out[2, 2] <- c[1]; out[1, 2] <- out[2, 1] <- 0
    note <- "Independent effects: the direct-competition covariance is constrained to zero."
  }

  if (anyNA(out)) stop_extract("the genetic covariance matrix")
  list(matrix = (out + t(out)) / 2, note = note)
}

#' Joint 2E x 2E genetic covariance across environments for a MET.
#'
#' ASReml labels these parameters by the level names of the synthetic effect
#' factor, for example `Env:Geno+Env:N1+and(Env:N2)!EffectEnv_D2!fa1`. The
#' application controls those level names, so every parameter can be placed by
#' name and none of the fragile positional parsing of earlier versions is
#' needed.
#'
#' @param fit fitted asreml object
#' @param structure "facv", "separable" or "diag"
#' @param effect_levels level names of the 2E-level effect factor, direct first
#' @param env_levels level names of the E-level environment factor (separable)
#' @param rank factor-analytic rank
#' @noRd
genetic_covariance_met <- function(fit, structure, effect_levels,
                                   env_levels = NULL, rank = 1L) {
  p <- parameter_table(fit)
  if (!nrow(p)) stop("ASReml returned no variance parameters.", call. = FALSE)
  dim <- length(effect_levels)

  # Match "<anything>!<factor>_<level>!<suffix>" or "...!<factor>_<level>".
  value_for <- function(levels, factor_name, suffix = NULL) {
    key <- paste0("!", factor_name, "_", levels)
    if (!is.null(suffix)) key <- paste0(key, "!", suffix)
    idx <- vapply(key, function(k) {
      hit <- which(endsWith(p$Parameter, k))
      if (length(hit)) hit[1] else NA_integer_
    }, integer(1))
    stats::setNames(p$Estimate[idx], levels)
  }

  if (structure == "diag") {
    v <- value_for(effect_levels, "EffectEnv")
    if (anyNA(v)) stop_extract("the diagonal MET genetic variances")
    G <- diag(as.numeric(v), dim)
    note <- "Diagonal: environment-specific variances with no genetic correlation between environments."

  } else if (structure == "facv") {
    psi <- value_for(effect_levels, "EffectEnv", "var")
    L <- vapply(seq_len(rank), function(r) {
      as.numeric(value_for(effect_levels, "EffectEnv", paste0("fa", r)))
    }, numeric(dim))
    L <- matrix(L, nrow = dim, ncol = rank)
    if (anyNA(psi) || anyNA(L)) stop_extract("the factor-analytic MET parameters")
    G <- L %*% t(L) + diag(as.numeric(psi), dim)
    note <- sprintf(
      "Joint FA(%d) over %d direct and %d competitive environment effects: G = Lambda Lambda' + Psi.",
      rank, dim / 2, dim / 2)

  } else if (structure == "separable") {
    # us(2) between direct and competitive effects, FA(rank) between
    # environments, combined as a direct product. This assumes the direct and
    # competitive effects share one environment correlation pattern - a strong
    # assumption, but it costs 3 + E(rank + 1) parameters instead of
    # 2E(rank + 1), which is what makes it fit when the joint model cannot.
    us <- genetic_covariance_2x2(fit, "us")$matrix
    psi <- value_for(env_levels, "EnvDummy", "var")
    L <- vapply(seq_len(rank), function(r) {
      as.numeric(value_for(env_levels, "EnvDummy", paste0("fa", r)))
    }, numeric(length(env_levels)))
    L <- matrix(L, nrow = length(env_levels), ncol = rank)
    if (anyNA(psi) || anyNA(L)) stop_extract("the separable MET environment covariance")
    Genv <- L %*% t(L) + diag(as.numeric(psi), length(env_levels))
    G <- kronecker(us, Genv)
    note <- sprintf(
      "Separable us(2) (x) FA(%d): a 2 x 2 direct-competition covariance crossed with one environment covariance.",
      rank)

  } else {
    stop("Unknown MET structure: ", structure, call. = FALSE)
  }

  if (anyNA(G)) stop_extract("the joint MET genetic covariance matrix")
  list(matrix = (G + t(G)) / 2, note = note)
}

#' Split a joint 2E x 2E (or 2 x 2) genetic matrix into its interpretable parts.
#'
#' The joint matrix is ordered as the E direct blocks followed by the E
#' competition blocks. For k neighbours the pure-stand covariance is
#'   G_PS = G_D + k^2 G_C + k (G_DC + G_DC')
#' which is the variance of D + kC, the genetic value a genotype would express
#' when every neighbour is itself.
#' @noRd
partition_genetic_covariance <- function(G, k, labels = NULL) {
  n <- nrow(G) / 2
  if (n != round(n)) stop("The joint genetic matrix must have even dimension.", call. = FALSE)
  n <- as.integer(n)
  di <- seq_len(n)
  ci <- n + di

  Gd  <- G[di, di, drop = FALSE]
  Gc  <- G[ci, ci, drop = FALSE]
  Gdc <- G[di, ci, drop = FALSE]
  Gps <- Gd + k^2 * Gc + k * (Gdc + t(Gdc))

  if (!is.null(labels)) {
    dimnames(Gd) <- dimnames(Gc) <- dimnames(Gdc) <- dimnames(Gps) <-
      list(labels, labels)
  }

  list(
    direct = Gd, competition = Gc, direct_competition = Gdc,
    pure = nearest_pd(Gps),
    direct_cor = safe_cov2cor(Gd),
    competition_cor = safe_cov2cor(Gc),
    pure_cor = safe_cov2cor(nearest_pd(Gps)),
    k = k
  )
}

# ---------------------------------------------------------------------------
# Prediction error variances
# ---------------------------------------------------------------------------

#' Size of the mixed-model coefficient matrix, used to gate the Cinv request.
#' @noRd
n_model_coefficients <- function(d, terms, n_geno, k_blocks = 2L) {
  design <- sum(vapply(terms, function(x) nlevels(d[[x]]), integer(1)), na.rm = TRUE)
  design + k_blocks * n_geno + 10L
}

#' Exact prediction-error covariance for direct and competitive effects.
#'
#' `asreml.options(Cinv = TRUE)` returns the inverse mixed-model coefficient
#' matrix, so the prediction error variance of any linear combination of the
#' solutions is available exactly:
#'
#'   PEV(d_g + k c_g) = sigma^2 * (C_dd + k^2 C_cc + 2k C_dc)
#'
#' This is what makes a *correct* pure-stand standard error possible. Adding
#' SE(d) and k SE(c) in quadrature, as if they were independent, is wrong:
#' direct and competitive solutions for the same genotype are strongly
#' negatively correlated, and the naive value can be several times too large.
#'
#' @param fit fitted asreml object carrying a `Cinv` element
#' @param direct_labels,competition_labels coefficient names, same order
#' @param k number of competing neighbours
#' @return data frame of PEVs, or NULL when Cinv is unavailable
#' @noRd
pev_direct_competition <- function(fit, direct_labels, competition_labels, k) {
  C <- fit$Cinv
  if (is.null(C)) return(NULL)
  nm <- rownames(C)
  if (is.null(nm)) return(NULL)
  if (!all(direct_labels %in% nm) || !all(competition_labels %in% nm)) return(NULL)

  s2 <- fit$sigma2 %||% 1
  di <- match(direct_labels, nm)
  ci <- match(competition_labels, nm)

  # Only the three diagonals of the per-genotype 2x2 blocks are needed, so the
  # full matrix is never densified.
  cdd <- as.numeric(Matrix::diag(C)[di])
  ccc <- as.numeric(Matrix::diag(C)[ci])
  cdc <- vapply(seq_along(di), function(i) as.numeric(C[di[i], ci[i]]), numeric(1))

  data.frame(
    pev_direct      = s2 * cdd,
    pev_competition = s2 * ccc,
    cov_direct_competition = s2 * cdc,
    pev_pure        = s2 * (cdd + k^2 * ccc + 2 * k * cdc),
    stringsAsFactors = FALSE
  )
}

#' Reliability and accuracy of a BLUP, given its PEV and the genetic variance.
#'
#' Reliability r^2 = 1 - PEV / sigma_g^2 is the squared correlation between the
#' predicted and true genetic value; accuracy is its square root. Both are
#' standard selection-decision statistics and are far more interpretable to a
#' breeder than a raw standard error.
#' @noRd
reliability <- function(pev, genetic_variance) {
  if (!is.finite(genetic_variance) || genetic_variance <= 0) return(rep(NA_real_, length(pev)))
  pmin(pmax(1 - pev / genetic_variance, 0), 1)
}

#' Cullis generalised heritability from mean PEV.
#'
#' Cullis, Smith & Coombes (2006). Uses mean PEV as the approximation to half
#' the mean pairwise prediction error variance of differences, which is exact
#' for a balanced design and close otherwise.
#' @noRd
cullis_h2 <- function(pev, genetic_variance) {
  if (!is.finite(genetic_variance) || genetic_variance <= 0) return(NA_real_)
  pev <- pev[is.finite(pev)]
  if (!length(pev)) return(NA_real_)
  max(0, min(1, 1 - mean(pev) / genetic_variance))
}

# ---------------------------------------------------------------------------
# Fitting with a transparent simplification ladder
# ---------------------------------------------------------------------------

#' Goodness-of-fit statistics in a single row.
#' @noRd
fit_statistics <- function(fit, label = "") {
  s <- suppressWarnings(summary(fit))
  npar <- attr(s$aic, "parameters") %||% NA_integer_
  data.frame(
    Model = label,
    LogLik = as.numeric(fit$loglik),
    Parameters = as.integer(npar),
    AIC = as.numeric(s$aic),
    BIC = as.numeric(s$bic),
    Converged = isTRUE(fit$converge),
    stringsAsFactors = FALSE
  )
}

#' Likelihood-ratio test between two nested REML fits.
#'
#' Valid only when the fixed-effect model is identical, which the application
#' guarantees by construction. Variance parameters tested at a boundary make
#' the chi-square p-value conservative; this is stated in the output rather
#' than silently corrected.
#' @noRd
likelihood_ratio_test <- function(full, reduced, label_full, label_reduced) {
  df <- (attr(summary(full)$aic, "parameters") %||% NA) -
    (attr(summary(reduced)$aic, "parameters") %||% NA)
  stat <- 2 * (full$loglik - reduced$loglik)
  p <- if (is.finite(df) && df > 0 && is.finite(stat) && stat > 0) {
    stats::pchisq(stat, df, lower.tail = FALSE)
  } else NA_real_
  data.frame(
    Comparison = paste(label_full, "vs", label_reduced),
    LR_statistic = stat, df = df, p_value = p,
    stringsAsFactors = FALSE
  )
}

#' Run a ladder of nested model specifications until one succeeds.
#'
#' Each entry of `specs` must carry a `reason`. `fit_fun(spec)` builds and fits
#' the model. Only recoverable numerical failures advance the ladder: a genuine
#' data or syntax error aborts immediately with its original message, so users
#' are never shown a cascade of confusing downstream failures.
#'
#' @param specs list of specification lists
#' @param fit_fun function(spec) returning a fitted model
#' @param allow_fallback if FALSE, only the first specification is attempted
#' @return list(fit, spec, log, warnings)
#' @noRd
run_fit_ladder <- function(specs, fit_fun, allow_fallback = TRUE,
                           progress = NULL) {
  log <- character(0)
  captured <- character(0)

  for (i in seq_along(specs)) {
    spec <- specs[[i]]
    if (is.function(progress)) progress(i, length(specs), spec$reason)

    attempt <- withCallingHandlers(
      tryCatch(list(ok = TRUE, fit = fit_fun(spec)),
               error = function(e) list(ok = FALSE, message = conditionMessage(e))),
      warning = function(w) {
        captured <<- c(captured, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )

    if (isTRUE(attempt$ok)) {
      log <- c(log, sprintf("%d. %s \u2014 fitted successfully.", i, spec$reason))
      return(list(fit = attempt$fit, spec = spec, log = log,
                  warnings = unique(captured), attempts = i))
    }

    log <- c(log, sprintf("%d. %s \u2014 failed: %s", i, spec$reason,
                          gsub("[\r\n]+", " ", attempt$message)))

    recoverable <- grepl(RECOVERABLE_PATTERNS, attempt$message, ignore.case = TRUE)
    if (!allow_fallback || !recoverable) {
      stop(attempt$message, call. = FALSE)
    }
  }

  stop("Every nested model in the simplification ladder failed.\n\n",
       paste(log, collapse = "\n"),
       "\n\nSuggestions: turn off the spatial residual, remove an unnecessary ",
       "replicate or block column, or check that genotypes are replicated ",
       "enough to separate direct from competitive effects.", call. = FALSE)
}

#' Format the ladder log for on-screen display.
#' @noRd
format_attempt_log <- function(log) paste(log, collapse = "\n")

#' Readable model summary for the on-screen panel.
#'
#' `print(summary(fit))` dumps ASReml's stored `$call`, which includes the
#' entire body of `asreml()` - several hundred lines of internal source that
#' bury the results and even echo licence-handling internals. Only the parts a
#' user needs are printed here; the raw object remains available to the code.
#' @noRd
print_model_summary <- function(result) {
  line <- function() cat(strrep("-", 72), "
")

  cat("FITTED MODEL
"); line()
  cat(strwrap(result$description, 72), sep = "
")
  cat("
")
  if (!is.null(result$structure_note)) {
    cat(strwrap(result$structure_note, 72), sep = "
")
    cat("
")
  }

  cat("
MODEL FORMULAE
"); line()
  f <- result$fit$formulae
  for (nm in c("fixed", "random", "residual")) {
    if (!is.null(f[[nm]])) {
      txt <- paste(deparse(stats::formula(f[[nm]])), collapse = " ")
      txt <- gsub("[[:space:]]+", " ", txt)
      cat(sprintf("%-10s", paste0(nm, ":")),
          paste(strwrap(txt, 60, exdent = 0), collapse = "
           "), "
")
    }
  }

  cat("
FIT STATISTICS
"); line()
  st <- result$fit_stats
  cat(sprintf("%-26s %s
", "Converged", if (isTRUE(st$Converged)) "yes" else "NO"))
  cat(sprintf("%-26s %.4f
", "REML log-likelihood", st$LogLik))
  cat(sprintf("%-26s %d
", "Variance parameters", st$Parameters))
  cat(sprintf("%-26s %.3f
", "AIC", st$AIC))
  cat(sprintf("%-26s %.3f
", "BIC", st$BIC))
  cat(sprintf("%-26s %s
", "Residual degrees of freedom",
              format(result$fit$nedf %||% NA)))

  cat("
VARIANCE COMPONENTS
"); line()
  print(result$summary$varcomp, digits = 5)

  if (length(result$warnings)) {
    cat("
ASREML WARNINGS
"); line()
    cat(paste0("- ", unique(result$warnings)), sep = "
")
  }
  invisible(NULL)
}
