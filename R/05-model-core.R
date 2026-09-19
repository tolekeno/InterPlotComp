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
#' Neither axis carries a `v` suffix, so the scale is ASReml's own residual
#' variance, `sigma2`. The alternative, putting the variance on the column term
#' as `ar1v(Column)`, fits an identical model - verified to the fourth decimal
#' of the log-likelihood with the same number of parameters, for a single trial
#' and for a `dsum()` MET alike, where each section still gets its own scale.
#' It merely reports the residual variance as a structure parameter and pins
#' `sigma2` at 1, which is harder to read.
#'
#' @param row_process,col_process names from `RESIDUAL_PROCESSES`
#' @noRd
spatial_residual_text <- function(row_process = "ar1", col_process = "ar1") {
  sprintf("%s(Column):%s(Row)", col_process, row_process)
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
#' `ar1`, `dsum`) are resolved when the model formulae are evaluated, so the
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

#' Tidy variance parameters on the variance scale, with constraint codes.
#'
#' IMPORTANT: `fit$vparameters` holds *ratios to sigma2*, not variances,
#' whenever sigma2 is estimated rather than pinned at 1 - which is the case for
#' any residual written without a `v` suffix, such as `ar1(Column):ar1(Row)`.
#' Reading it directly would silently rescale every genetic variance, and with
#' it the pure-stand variance, the heritabilities and the reliabilities, by a
#' factor of sigma2. The absolute components come from `summary()$varcomp`,
#' which ASReml has already scaled correctly for each parameter type; only the
#' type and constraint codes are taken from the raw object.
#' @noRd
parameter_table <- function(fit) {
  v <- fit$vparameters
  if (is.null(v) || !length(v)) return(data.frame())
  type <- fit$vparameters.type
  if (is.null(type) || length(type) != length(v)) type <- rep(NA_character_, length(v))
  con <- fit$vparameters.con
  if (is.null(con) || length(con) != length(v)) con <- rep(NA_character_, length(v))

  nm <- names(v) %||% paste0("par", seq_along(v))
  est <- as.numeric(v)
  vc <- tryCatch(summary(fit)$varcomp, error = function(e) NULL)
  if (!is.null(vc) && "component" %in% names(vc)) {
    hit <- match(nm, rownames(vc))
    ok <- !is.na(hit)
    est[ok] <- as.numeric(vc$component[hit[ok]])
  }

  data.frame(
    Parameter = nm,
    Estimate  = est,
    Type      = toupper(as.character(type)),
    Constraint = as.character(con),
    stringsAsFactors = FALSE
  )
}

#' Breeder-facing name for one ASReml variance parameter.
#'
#' ASReml names a parameter after the model string that produced it, so a
#' perfectly ordinary direct genetic variance is reported as
#' `Geno+N1+and(N2)!us(2)_1:1`. That is the right label for tracing a number
#' back to the fit and the wrong one for reading a figure, so the raw name is
#' kept in `Component` and this supplies the name a breeder would use.
#'
#' Every rule below is matched against real ASReml output for each structure
#' the application can fit. Anything unrecognised is returned unchanged rather
#' than guessed at, so a new parameter shows up as itself instead of being
#' silently mislabelled.
#'
#' @param component character vector of raw parameter names
#' @param env_levels environment level names, in the order the multi-
#'   environment effect factor was built, used to name the `D1`/`C1` levels
#' @noRd
interpret_component <- function(component, env_levels = NULL) {
  env_label <- function(i) {
    if (!is.null(env_levels) && length(env_levels) >= i) env_levels[i]
    else sprintf("environment %d", i)
  }
  sentence <- function(x) paste0(toupper(substring(x, 1, 1)), substring(x, 2))

  one <- function(x) {
    # -- the direct-competition covariance block --------------------------
    if (grepl("!us[(]2[)]_", x)) {
      idx <- sub(".*!us[(]2[)]_", "", x)
      return(switch(idx,
                    "1:1" = "Direct genetic variance",
                    "2:2" = "Competitive genetic variance",
                    "2:1" = ,
                    "1:2" = "Direct-competition covariance",
                    x))
    }
    if (grepl("!corgh[(]2[)]", x)) {
      if (grepl("cor$", x)) return("Direct-competition correlation")
      return(switch(sub(".*!corgh[(]2[)]_", "", x),
                    "1" = "Direct genetic variance",
                    "2" = "Competitive genetic variance",
                    x))
    }
    # The independent structure is fitted as plain terms, so the two genetic
    # variances arrive under their own names.
    if (identical(x, "Geno")) return("Direct genetic variance")
    if (identical(x, "N1"))   return("Competitive genetic variance")

    # -- multi-environment factor-analytic block ---------------------------
    m <- regmatches(x, regexec("!EffectEnv_([DC])([0-9]+)!(fa[0-9]+|var)$", x,
                               perl = TRUE))[[1]]
    if (length(m) == 4L) {
      effect <- if (m[2] == "D") "Direct" else "Competitive"
      env <- env_label(as.integer(m[3]))
      return(if (identical(m[4], "var")) {
        sprintf("%s specific variance (%s)", effect, env)
      } else {
        sprintf("%s loading on factor %s (%s)", effect, sub("^fa", "", m[4]), env)
      })
    }
    # The separable structure indexes the same parameters by environment name.
    m <- regmatches(x, regexec("!EnvDummy_([^!]+)!(fa[0-9]+|var)$", x, perl = TRUE))[[1]]
    if (length(m) == 3L) {
      return(if (identical(m[3], "var")) {
        sprintf("Specific variance (%s)", m[2])
      } else {
        sprintf("Loading on factor %s (%s)", sub("^fa", "", m[3]), m[2])
      })
    }

    # -- residual block ----------------------------------------------------
    # A per-environment residual is named after its environment; a single
    # trial's is named after the two field axes.
    block <- sub("!.*$", "", x)
    qual <- if (grepl("^Env_", block)) sprintf(" (%s)", sub("^Env_", "", block)) else ""
    if (grepl("!R$", x)) return(paste0("Spatial residual variance", qual))
    m <- regmatches(x, regexec("!([^!]+)!cor$", x, perl = TRUE))[[1]]
    if (length(m) == 2L) return(sprintf("%s correlation%s", m[2], qual))

    # -- design terms and the nugget ---------------------------------------
    pretty <- pretty_term(block)
    if (!identical(pretty, block)) return(paste(sentence(pretty), "variance"))
    x
  }

  vapply(as.character(component), one, character(1), USE.NAMES = FALSE)
}

#' Publication-ready variance-component table.
#'
#' Adds the proportion of total variance contributed by each component, which
#' is what breeders normally want to read off, and flags components sitting at
#' a boundary because those invalidate the usual standard errors.
#' @noRd
variance_component_table <- function(fit, env_levels = NULL) {
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

  # Internal parameter names are replaced with breeder-facing wording; the raw
  # ASReml label is kept alongside so the output can still be traced back.
  vc$Interpretation <- interpret_component(vc$Component, env_levels)
  keep <- c("Component", "Interpretation", "Estimate", "Std_error", "Z_ratio",
            "Pct_of_total", "Bound", "At_boundary", "Pct_change")
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
#' @param structure "facv", "separable", "fa" or "diag"
#' @param effect_levels level names of the 2E-level effect factor, direct first
#' @param env_levels level names of the E-level environment factor
#' @param rank factor-analytic rank
#' @param fa_terms for structure "fa", the two model-term strings whose
#'   parameters carry the direct and competitive covariances
#' @noRd
genetic_covariance_met <- function(fit, structure, effect_levels,
                                   env_levels = NULL, rank = 1L,
                                   fa_terms = NULL) {
  p <- parameter_table(fit)
  if (!nrow(p)) stop("ASReml returned no variance parameters.", call. = FALSE)
  dim <- length(effect_levels)

  # Match "<anything>!<factor>_<level>!<suffix>", "...!<factor>_<level>", or
  # for an fa() term "<term>!<level>!<suffix>".
  value_for <- function(levels, factor_name, suffix = NULL, prefix = NULL) {
    key <- if (is.null(prefix)) {
      paste0("!", factor_name, "_", levels)
    } else {
      paste0(prefix, "!", levels)
    }
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

  } else if (structure == "fa") {
    # Separate fa() terms for the direct and competitive effects. Each supplies
    # its own E x E factor-analytic covariance; ASReml names the parameters
    # "<term>!<environment>!var" and "<term>!<environment>!fa<k>".
    #
    # The two terms are independent by construction, so the joint matrix is
    # block diagonal and the direct-competition covariance is structurally
    # zero. That is a property of this model, not an estimate: it cannot be
    # tested, and the pure-stand covariance reduces to G_D + k^2 G_C.
    if (is.null(fa_terms)) stop("fa_terms are required for the fa structure.",
                                call. = FALSE)
    block <- function(term) {
      psi <- value_for(env_levels, NULL, "var", prefix = term)
      L <- vapply(seq_len(rank), function(r) {
        as.numeric(value_for(env_levels, NULL, paste0("fa", r), prefix = term))
      }, numeric(length(env_levels)))
      L <- matrix(L, nrow = length(env_levels), ncol = rank)
      if (anyNA(psi) || anyNA(L)) stop_extract("the separate fa() parameters")
      L %*% t(L) + diag(as.numeric(psi), length(env_levels))
    }
    Gd <- block(fa_terms[["direct"]])
    Gc <- block(fa_terms[["competition"]])
    e <- length(env_levels)
    G <- matrix(0, 2L * e, 2L * e)
    G[seq_len(e), seq_len(e)] <- Gd
    G[e + seq_len(e), e + seq_len(e)] <- Gc
    note <- sprintf(paste(
      "Separate FA(%d) covariances for the direct and competitive effects.",
      "The direct-competition covariance is zero by construction, not estimated."),
      rank)

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

  # Cinv is already on the variance scale: sqrt(Cinv[i, i]) reproduces the same
  # standard error whether the residual is written ar1v(Column) with sigma2
  # pinned at 1, or ar1(Column) with sigma2 estimated. Multiplying by sigma2
  # here would be a silent no-op in the first case and simply wrong in the
  # second.
  di <- match(direct_labels, nm)
  ci <- match(competition_labels, nm)

  # Only the three diagonals of the per-genotype 2x2 blocks are needed, so the
  # full matrix is never densified.
  cdd <- as.numeric(Matrix::diag(C)[di])
  ccc <- as.numeric(Matrix::diag(C)[ci])
  cdc <- vapply(seq_along(di), function(i) as.numeric(C[di[i], ci[i]]), numeric(1))

  data.frame(
    pev_direct      = cdd,
    pev_competition = ccc,
    cov_direct_competition = cdc,
    pev_pure        = cdd + k^2 * ccc + 2 * k * cdc,
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

#' Heritability and accuracy summary for a single trial.
#'
#' The "Variance & heritability" panel previously showed variance components
#' only, leaving the heritabilities buried in the header strip. They are the
#' numbers a breeder acts on, so they get their own table, together with the
#' mean reliability and accuracy that determine how dependable a selection is.
#'
#' @param result the list returned by `fit_single_model()`
#' @noRd
single_heritability_table <- function(result) {
  g <- result$genetic
  h2 <- result$heritability
  narrow <- !is.null(result$relationship)
  basis <- if (narrow) "narrow-sense (additive)" else "entry-mean"

  mean_rel <- function(col) {
    if (!col %in% names(g)) return(NA_real_)
    mean(g[[col]], na.rm = TRUE)
  }

  # Mean reliability is algebraically the same quantity as Cullis
  # heritability, 1 - mean(PEV)/sigma_g^2, so reporting both would only look
  # like corroboration. What adds information is how the reliabilities are
  # spread: the accuracy and how many genotypes clear a usable threshold.
  n_above <- function(col, cut = 0.5) {
    if (!col %in% names(g)) return(NA_integer_)
    sum(g[[col]] >= cut, na.rm = TRUE)
  }
  n_geno <- nrow(g)

  out <- data.frame(
    Effect = c("Direct", "Pure stand"),
    Genetic_variance = c(result$components$direct, result$components$pure),
    Heritability = c(h2[["direct"]], h2[["pure"]]),
    stringsAsFactors = FALSE
  )
  out$Accuracy <- sqrt(pmax(out$Heritability, 0))
  out$Genotypes_reliability_over_0.5 <- c(n_above("Reliability_direct"),
                                          n_above("Reliability_pure_stand"))
  out$Genotypes_total <- n_geno
  out$Basis <- c(basis, basis)
  out$Interpretation <- c(
    "Repeatability of the genotype's own-plot performance",
    sprintf("Repeatability of the value expressed in a pure stand (D + %d C)",
            result$k)
  )
  out
}

#' Heritability and accuracy by environment for a MET.
#' @noRd
met_heritability_table <- function(result) {
  env <- result$environments
  v <- result$variance
  h2 <- result$heritability
  h2p <- result$heritability_pure
  vals <- result$values

  mean_by_env <- function(col) {
    if (!col %in% names(vals)) return(rep(NA_real_, length(env)))
    vapply(env, function(e) {
      mean(vals[[col]][vals$Environment == e], na.rm = TRUE)
    }, numeric(1))
  }

  n_above <- function(col, cut = 0.5) {
    if (!col %in% names(vals)) return(rep(NA_integer_, length(env)))
    vapply(env, function(e) {
      sum(vals[[col]][vals$Environment == e] >= cut, na.rm = TRUE)
    }, integer(1))
  }

  h2d <- as.numeric(h2[env])
  h2ps <- if (is.null(h2p)) rep(NA_real_, length(env)) else as.numeric(h2p[env])

  data.frame(
    Environment = env,
    Direct_variance = v$Direct_variance,
    Heritability_direct = h2d,
    Accuracy_direct = sqrt(pmax(h2d, 0)),
    Pure_stand_variance = v$Pure_stand_variance,
    Heritability_pure_stand = h2ps,
    Accuracy_pure_stand = sqrt(pmax(h2ps, 0)),
    Genotypes_reliability_over_0.5 = n_above("Reliability_pure_stand"),
    stringsAsFactors = FALSE, row.names = NULL
  )
}

#' Fixed-effect terms contributed by the covariate.
#'
#' The neighbour term is the adjustment itself: it asks how much of a plot's
#' yield is explained by how large its neighbours were. The focal plot's own
#' value is optional and off by default, because it absorbs genetic variation
#' in the covariate and so removes part of the direct effect being estimated.
#'
#' @param d prepared data carrying `Covariate_nb` when a covariate was supplied
#' @param adjust_own include the focal plot's own centred covariate value
#' @noRd
covariate_fixed_terms <- function(d, adjust_own = FALSE) {
  out <- character(0)
  if ("Covariate_nb" %in% names(d)) out <- c(out, "Covariate_nb")
  if (isTRUE(adjust_own) && "Covariate_own" %in% names(d)) out <- c(out, "Covariate_own")
  out
}

#' Tidy the fixed-effect solutions, for reporting the covariate slopes.
#' @noRd
fixed_effects_table <- function(summary_object) {
  cf <- summary_object$coef.fixed
  if (is.null(cf)) return(NULL)
  cf <- as.data.frame(cf)
  sol <- solution_column(cf)
  se <- std_error_column(cf)
  out <- data.frame(
    Term = pretty_fixed_term(rownames(cf)),
    Estimate = as.numeric(cf[[sol]]),
    stringsAsFactors = FALSE
  )
  if (!is.na(se)) {
    out$Std_error <- as.numeric(cf[[se]])
    out$Z_ratio <- out$Estimate / out$Std_error
  }
  out[!is.na(out$Estimate), , drop = FALSE]
}

#' Readable labels for the fixed-effect rows.
#' @noRd
pretty_fixed_term <- function(x) {
  x <- sub("^Covariate_nb$", "Covariate, neighbouring plots", x)
  x <- sub("^Covariate_own$", "Covariate, own plot", x)
  x <- sub("^Env_", "Environment ", x)
  x
}

#' Wald tests for the fixed effects.
#'
#' Answers the question a covariate raises: should it stay in the model? The
#' conditional F-test with denominator degrees of freedom is preferred over the
#' chi-square form, because the chi-square assumes the denominator degrees of
#' freedom are infinite and is anti-conservative in a trial-sized dataset. The
#' chi-square table is used only if the denominator calculation fails.
#'
#' @param fit fitted asreml object
#' @return data frame of terms with degrees of freedom, statistic and p-value,
#'   or NULL when no Wald table could be produced
#' @noRd
wald_table <- function(fit) {
  w <- tryCatch(asreml::wald(fit, denDF = "numeric", trace = FALSE),
                error = function(e) NULL, warning = function(w) NULL)

  if (!is.null(w) && is.list(w) && !is.null(w$Wald)) {
    tab <- as.data.frame(w$Wald)
    out <- data.frame(
      Term = pretty_fixed_term(rownames(tab)),
      Df = as.numeric(tab[["Df"]]),
      Denominator_df = round(as.numeric(tab[["denDF"]]), 1),
      F_statistic = as.numeric(tab[["F.inc"]]),
      P_value = as.numeric(tab[["Pr"]]),
      stringsAsFactors = FALSE
    )
    out$Test <- "Conditional F"
  } else {
    w <- tryCatch(asreml::wald(fit, trace = FALSE), error = function(e) NULL)
    if (is.null(w)) return(NULL)
    tab <- as.data.frame(unclass(w))
    stat <- grep("Wald", names(tab), value = TRUE)[1]
    pcol <- grep("^Pr", names(tab), value = TRUE)[1]
    if (is.na(stat) || is.na(pcol)) return(NULL)
    out <- data.frame(
      Term = pretty_fixed_term(rownames(tab)),
      Df = as.numeric(tab[["Df"]]),
      Denominator_df = NA_real_,
      F_statistic = as.numeric(tab[[stat]]),
      P_value = as.numeric(tab[[pcol]]),
      stringsAsFactors = FALSE
    )
    out$Test <- "Wald chi-square"
  }

  out <- out[!is.na(out$P_value) | !is.na(out$F_statistic), , drop = FALSE]
  out$Significance <- significance_stars(out$P_value)
  out$Retain <- ifelse(is.na(out$P_value), NA_character_,
                       ifelse(out$P_value < 0.05, "Yes", "No"))
  rownames(out) <- NULL
  out
}

#' Conventional significance marks for a p-value.
#' @noRd
significance_stars <- function(p) {
  ifelse(is.na(p), "",
         ifelse(p < 0.001, "***",
                ifelse(p < 0.01, "**",
                       ifelse(p < 0.05, "*",
                              ifelse(p < 0.1, ".", "n.s.")))))
}

#' Refit until ASReml reports convergence.
#'
#' A single `asreml()` call stops at `maxit` whether or not the variance
#' parameters have settled, and a model reported as "not converged" is not safe
#' to quote. `update()` restarts from the current estimates, so calling it
#' repeatedly continues the same fit rather than beginning again. The round
#' limit exists only so that a genuinely non-convergent model cannot spin
#' forever; reaching it is reported rather than hidden.
#'
#' @param fit a fitted asreml object
#' @param max_rounds most additional `update()` calls to attempt
#' @param progress optional function(round) for the busy indicator
#' @return list(fit, rounds, converged)
#' @noRd
iterate_to_convergence <- function(fit, max_rounds = 15L, progress = NULL) {
  rounds <- 0L
  while (!isTRUE(fit$converge) && rounds < max_rounds) {
    rounds <- rounds + 1L
    if (is.function(progress)) progress(rounds)
    nxt <- tryCatch(suppressWarnings(stats::update(fit)), error = function(e) NULL)
    if (is.null(nxt)) break
    fit <- nxt
  }
  list(fit = fit, rounds = rounds, converged = isTRUE(fit$converge))
}
