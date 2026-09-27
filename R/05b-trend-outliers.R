# ---------------------------------------------------------------------------
# Global field trend, phenotypic outlier screening, and the reproducible
# ASReml-R script - the model-building steps shared by the single-trial and
# multi-environment fits.
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
# Global field trend (Gilmour, Cullis & Verbyla 1997)
# ---------------------------------------------------------------------------

# A separable AR1 x AR1 process models local, stationary field variation. A
# smooth gradient running the length of a trial - a fertility or moisture
# slope - is not stationary, and left to the spatial process it inflates the
# autocorrelation and leaks into whatever else is correlated with position,
# the competitive effects included. Gilmour et al. (1997) remove it first with
# linear covariates for row and column position, fitted as fixed effects:
# `lrow` and `lcol`. Because the gradient is a property of each field, a MET
# fits it site by site, as `at(Env, <sites>):lrow`, so each selected site gets
# its own slope and the others get none.

FIELD_TREND_TERMS <- c("lrow", "lcol")

# Per-site choices offered in the interface.
FIELD_TREND_CHOICES <- c(
  "lrow + lcol" = "both",
  "lrow only"   = "lrow",
  "lcol only"   = "lcol"
)

#' Expand a field-trend choice to the terms it stands for.
#' @noRd
trend_choice_terms <- function(x) {
  x <- as.character(x)
  x <- x[!is.na(x) & nzchar(x)]
  out <- unlist(lapply(x, function(v) switch(v, both = FIELD_TREND_TERMS,
                                             none = character(0), v)))
  unique(as.character(out))
}

#' Validate a field-trend request and put it in one canonical form.
#'
#' Accepts, so that a script and the interface can both say what they mean
#' without ceremony:
#' * `NULL`, `FALSE` or an empty value - no trend adjustment;
#' * an unnamed character vector such as `c("lrow", "lcol")` or `"both"` -
#'   the same terms at every site;
#' * a named list or named character vector, one entry per site, each
#'   `"lrow"`, `"lcol"`, `"both"`, `c("lrow", "lcol")` or `"none"`;
#' * a data frame with an `Environment` column and logical `lrow` and `lcol`
#'   columns, which is the form this function returns.
#'
#' @param trend the request
#' @param env_levels environment levels of the prepared data
#' @return data frame with columns `Environment`, `lrow`, `lcol`, one row per
#'   site that carries at least one term, in the order of `env_levels`
#' @noRd
normalise_field_trend <- function(trend, env_levels) {
  empty <- data.frame(Environment = character(0), lrow = logical(0),
                      lcol = logical(0), stringsAsFactors = FALSE)
  if (is.null(trend) || !length(trend) || isFALSE(trend)) return(empty)

  env_levels <- as.character(env_levels)

  if (is.data.frame(trend)) {
    if (!"Environment" %in% names(trend)) {
      stop("A field-trend table needs an 'Environment' column.", call. = FALSE)
    }
    per_site <- stats::setNames(lapply(seq_len(nrow(trend)), function(i) {
      FIELD_TREND_TERMS[vapply(FIELD_TREND_TERMS, function(t) {
        isTRUE(as.logical(trend[[t]][i] %||% FALSE))
      }, logical(1))]
    }), as.character(trend$Environment))
  } else if (is.null(names(trend)) || !any(nzchar(names(trend)))) {
    terms <- trend_choice_terms(unlist(trend))
    per_site <- stats::setNames(rep(list(terms), length(env_levels)), env_levels)
  } else {
    per_site <- lapply(as.list(trend), trend_choice_terms)
  }

  bad_terms <- setdiff(unlist(per_site), FIELD_TREND_TERMS)
  if (length(bad_terms)) {
    stop("Unknown field-trend term(s): ", paste(unique(bad_terms), collapse = ", "),
         ". Use 'lrow', 'lcol' or 'both'.", call. = FALSE)
  }
  bad_sites <- setdiff(names(per_site), env_levels)
  if (length(bad_sites)) {
    stop("Field-trend adjustment requested for site(s) not in the data: ",
         paste(bad_sites, collapse = ", "), ". Sites in the data: ",
         paste(env_levels, collapse = ", "), ".", call. = FALSE)
  }

  sites <- env_levels[env_levels %in% names(per_site)]
  sites <- sites[lengths(per_site[sites]) > 0L]
  if (!length(sites)) return(empty)
  data.frame(
    Environment = sites,
    lrow = vapply(sites, function(s) "lrow" %in% per_site[[s]], logical(1)),
    lcol = vapply(sites, function(s) "lcol" %in% per_site[[s]], logical(1)),
    stringsAsFactors = FALSE, row.names = NULL
  )
}

#' Add the centred linear row and column covariates.
#'
#' Centred on the midpoint of each site's own grid, so the covariates of one
#' site never depend on the size of another and the intercept (or the
#' environment mean) stays the fitted value at the centre of the field. Padded
#' grid positions get a value like any other plot; they carry no response, so
#' they do not influence the slope.
#'
#' @param d prepared data with `Env`, `Row_i` and `Col_i`
#' @noRd
add_trend_covariates <- function(d) {
  mid <- function(x) (max(x) + 1) / 2
  d$lrow <- d$Row_i - stats::ave(d$Row_i, d$Env, FUN = mid)
  d$lcol <- d$Col_i - stats::ave(d$Col_i, d$Env, FUN = mid)
  d
}

#' R source for a vector of environment level names inside a formula.
#' @noRd
level_vector_text <- function(x) {
  q <- encodeString(as.character(x), quote = "\"")
  if (length(q) == 1L) q else sprintf("c(%s)", paste(q, collapse = ", "))
}

#' Fixed-model terms for the requested field trend.
#'
#' A single trial fits plain `lrow` and `lcol`. A MET fits each term only at
#' the sites that asked for it: `at(Env):lrow` when every site did, which
#' gives one slope per site, otherwise `at(Env, c("A", "B")):lrow`. A common
#' slope across sites is deliberately not offered - fields do not share a
#' gradient.
#'
#' @param trend output of `normalise_field_trend()`
#' @param env_levels environment levels of the data
#' @param multi_env is this a MET?
#' @noRd
field_trend_fixed_terms <- function(trend, env_levels, multi_env = TRUE) {
  if (is.null(trend) || !nrow(trend)) return(character(0))
  out <- character(0)
  for (term in FIELD_TREND_TERMS) {
    sites <- trend$Environment[trend[[term]]]
    if (!length(sites)) next
    out <- c(out, if (!multi_env) {
      term
    } else if (setequal(sites, env_levels)) {
      sprintf("at(Env):%s", term)
    } else {
      sprintf("at(Env, %s):%s", level_vector_text(sites), term)
    })
  }
  out
}

#' Plain-English description of the field-trend adjustment.
#' @noRd
describe_field_trend <- function(trend, multi_env = TRUE) {
  if (is.null(trend) || !nrow(trend)) return("")
  label <- function(r, c) {
    if (r && c) "linear row and column trends" else if (r) "linear row trend"
    else "linear column trend"
  }
  if (!multi_env) {
    return(paste0(label(trend$lrow[1], trend$lcol[1]), " (lrow/lcol)"))
  }
  key <- paste(trend$lrow, trend$lcol)
  groups <- split(trend$Environment, factor(key, levels = unique(key)))
  paste(vapply(names(groups), function(k) {
    rc <- as.logical(strsplit(k, " ", fixed = TRUE)[[1]])
    sprintf("%s at %s", label(rc[1], rc[2]), paste(groups[[k]], collapse = ", "))
  }, character(1)), collapse = "; ")
}

#' Field-trend specification as a reporting table.
#' @noRd
field_trend_table <- function(trend, env_levels, multi_env = TRUE) {
  if (is.null(trend) || !nrow(trend)) return(NULL)
  terms <- field_trend_fixed_terms(trend, env_levels, multi_env)
  data.frame(
    Environment = trend$Environment,
    Linear_row_lrow = ifelse(trend$lrow, "Yes", "No"),
    Linear_column_lcol = ifelse(trend$lcol, "Yes", "No"),
    Fixed_terms = vapply(seq_len(nrow(trend)), function(i) {
      want <- FIELD_TREND_TERMS[c(trend$lrow[i], trend$lcol[i])]
      paste(terms[vapply(terms, function(t) {
        any(endsWith(t, paste0(":", want)) | t %in% want)
      }, logical(1))], collapse = " + ")
    }, character(1)),
    stringsAsFactors = FALSE
  )
}

# ---------------------------------------------------------------------------
# Phenotypic outlier screening
# ---------------------------------------------------------------------------

OUTLIER_MODES <- c(
  "Off"                               = "none",
  "Detect and report only"            = "detect",
  "Detect, remove, then refit"        = "remove"
)

DEFAULT_OUTLIER_THRESHOLD <- 4

#' Validate the outlier-screening options.
#' @noRd
outlier_settings <- function(opts) {
  mode <- as.character(opts$outliers %||% "none")[1]
  if (isTRUE(opts$outliers)) mode <- "detect"
  if (isFALSE(opts$outliers)) mode <- "none"
  if (!mode %in% OUTLIER_MODES) {
    stop("Unknown outlier option '", mode, "'. Use one of: ",
         paste(OUTLIER_MODES, collapse = ", "), ".", call. = FALSE)
  }
  threshold <- suppressWarnings(as.numeric(opts$outlier_threshold %||%
                                             DEFAULT_OUTLIER_THRESHOLD)[1])
  if (!is.finite(threshold) || threshold < 2) {
    stop("The outlier threshold must be a number of at least 2 standard ",
         "deviations; 4 is the usual choice.", call. = FALSE)
  }
  list(mode = mode, threshold = threshold)
}

#' Standardised conditional residuals of a fitted model.
#'
#' Each residual is divided by its own standard error, which is what makes a
#' fixed cut-off such as 4 meaningful: a plot at the edge of a field, a plot
#' with a missing neighbour and a plot in a noisy environment all have
#' different residual variances, and dividing by one pooled standard deviation
#' would flag the noisy site wholesale while missing the real outliers in a
#' quiet one. ASReml supplies these as `residuals(type = "stdCond")`, but only
#' for a fit run with `aom = TRUE`; without it the call silently returns the
#' raw residuals. The fit is therefore continued once with `aom = TRUE`, from
#' its converged estimates, purely to obtain them.
#'
#' If that fails, residuals are scaled by their standard deviation within each
#' environment, and the method is recorded so the report says which was used.
#'
#' @param fit fitted asreml object
#' @param d the data the model was fitted to
#' @param envir environment holding objects the formula refers to (`.kinship`)
#' @return numeric vector aligned with `d`, with attribute `method`
#' @noRd
standardised_residuals <- function(fit, d, envir = parent.frame()) {
  n <- nrow(d)
  run <- new.env(parent = envir)
  run$fit <- fit
  aom_fit <- tryCatch(
    suppressWarnings(eval(quote(stats::update(fit, aom = TRUE)), run)),
    error = function(e) NULL)
  r <- if (!is.null(aom_fit)) {
    tryCatch(as.numeric(stats::residuals(aom_fit, type = "stdCond")),
             error = function(e) NULL)
  } else NULL

  observed <- !is.na(d$Yield)
  if (!is.null(r) && length(r) == n && any(is.finite(r[observed]))) {
    attr(r, "method") <- "standardised conditional residuals (ASReml stdCond)"
    return(r)
  }

  raw <- as.numeric(stats::residuals(fit))
  if (length(raw) != n) return(structure(rep(NA_real_, n), method = "unavailable"))
  scale <- stats::ave(raw, d$Env, FUN = function(x) stats::sd(x, na.rm = TRUE))
  structure(raw / scale, method = "residuals scaled by their standard deviation within environment")
}

#' Observations whose standardised residual exceeds the threshold.
#'
#' Keyed by environment and grid position, which identify a plot uniquely and
#' survive the re-ordering and grid padding that happen inside a fit, so the
#' same records can be removed from the caller's data.
#'
#' @param d fitted data
#' @param fitted_values,residuals,std aligned with `d`
#' @param threshold absolute standardised-residual cut-off
#' @noRd
outlier_table <- function(d, fitted_values, residuals, std, threshold) {
  keep <- !is.na(d$Yield) & !is.na(std) & abs(std) > threshold
  if ("Padded" %in% names(d)) keep <- keep & !d$Padded
  out <- data.frame(
    Environment = as.character(d$Env[keep]),
    Genotype = as.character(d$Geno[keep]),
    Field_row = as.character(d$Row_label[keep] %||% d$Row_i[keep]),
    Field_column = as.character(d$Column_label[keep] %||% d$Col_i[keep]),
    Row_index = d$Row_i[keep],
    Column_index = d$Col_i[keep],
    Observed = d$Yield[keep],
    Fitted = fitted_values[keep],
    Residual = residuals[keep],
    Std_residual = std[keep],
    stringsAsFactors = FALSE
  )
  out <- out[order(-abs(out$Std_residual)), , drop = FALSE]
  rownames(out) <- NULL
  out
}

#' Screen a fitted model for outliers.
#'
#' Returns `NULL` when screening is off, so a result carries an `outliers`
#' element only when the user asked for one.
#' @noRd
screen_fit <- function(fit, d, settings, envir = parent.frame()) {
  if (identical(settings$mode, "none")) return(NULL)
  std <- standardised_residuals(fit, d, envir)
  tab <- outlier_table(d, as.numeric(stats::fitted(fit)),
                       as.numeric(stats::residuals(fit)), std, settings$threshold)
  tab$Action <- rep(if (identical(settings$mode, "remove")) "Removed before refit"
                    else "Flagged, retained", nrow(tab))
  list(mode = settings$mode, threshold = settings$threshold,
       method = attr(std, "method"), std = as.numeric(std), table = tab)
}

#' Set the response of the flagged plots to missing.
#'
#' The plot stays in the data: its genotype is still growing there and still
#' competing with its neighbours, and its grid position is still needed by the
#' spatial process. Only its own measurement is withdrawn.
#' @noRd
drop_outlier_records <- function(d, flagged) {
  if (is.null(flagged) || !nrow(flagged)) return(d)
  key <- paste(as.character(d$Env), d$Row_i, d$Col_i, sep = "\r")
  hit <- key %in% paste(flagged$Environment, flagged$Row_index,
                        flagged$Column_index, sep = "\r")
  d$Yield[hit] <- NA_real_
  attr(d, "outliers_removed") <- sum(hit)
  d
}

#' Fit, screen for outliers and, if asked, remove them and refit.
#'
#' `fit_fun(data, opts)` must return a result carrying `outliers` from
#' `screen_fit()`. The refit starts again from the requested model, not from
#' the one the first pass settled on, so the same simplification ladder
#' applies to both and the report is comparable. It is a single pass: points
#' that exceed the threshold only once the first outliers are gone are
#' reported, not removed, because removing observations until none remain
#' eats into genuine tails of the distribution.
#'
#' @param d data as supplied by the caller
#' @param opts fitting options
#' @param fit_fun function(data, opts) returning a result list
#' @noRd
with_outlier_screening <- function(d, opts, fit_fun) {
  settings <- outlier_settings(opts)
  opts$outliers <- settings$mode
  opts$outlier_threshold <- settings$threshold

  first <- fit_fun(d, opts)
  env_levels <- levels(d$Env)
  if (identical(settings$mode, "none")) return(first)

  found <- first$outliers
  if (!identical(settings$mode, "remove") || !nrow(found$table)) {
    first$outliers <- finish_outliers(found, found$table[0, , drop = FALSE],
                                      env_levels, removed = FALSE)
    first$description <- paste0(first$description, outlier_phrase(first$outliers))
    return(first)
  }

  cleaned <- drop_outlier_records(d, found$table)
  second <- fit_fun(cleaned, opts)

  remaining <- second$outliers$table
  remaining$Action <- rep("Exceeds threshold after refit, retained", nrow(remaining))
  second$outliers <- finish_outliers(found, remaining, env_levels, removed = TRUE)
  second$outliers$initial_fit_stats <- first$fit_stats
  if (!is.null(second$model_code)) second$model_code$removed <- found$table
  second$log <- c(
    "Initial fit:", paste0("  ", first$log),
    sprintf("Outlier screen: %d observation(s) with |standardised residual| > %g removed.",
            nrow(found$table), settings$threshold),
    "Refit without them:", paste0("  ", second$log))
  second$description <- paste0(second$description, outlier_phrase(second$outliers))
  second
}

#' Assemble the outlier element of a result.
#' @noRd
finish_outliers <- function(found, remaining, env_levels, removed) {
  tab <- rbind(found$table, remaining)
  rownames(tab) <- NULL
  list(
    mode = found$mode, threshold = found$threshold, method = found$method,
    table = tab,
    removed = if (removed) found$table else found$table[0, , drop = FALSE],
    n_detected = nrow(found$table),
    n_removed = if (removed) nrow(found$table) else 0L,
    n_remaining = nrow(remaining),
    summary = outlier_summary(tab, env_levels)
  )
}

#' Outliers per site, for the summary table.
#' @noRd
outlier_summary <- function(tab, env_levels) {
  env_levels <- as.character(env_levels)
  count <- function(action = NULL) {
    x <- if (is.null(action)) tab else tab[tab$Action %in% action, , drop = FALSE]
    vapply(env_levels, function(e) sum(x$Environment == e), integer(1))
  }
  first_pass <- c("Removed before refit", "Flagged, retained")
  max_abs <- vapply(env_levels, function(e) {
    v <- abs(tab$Std_residual[tab$Environment == e])
    if (length(v)) max(v) else NA_real_
  }, numeric(1))
  data.frame(
    Environment = env_levels,
    Outliers_detected = count(first_pass),
    Removed = count("Removed before refit"),
    Flagged_after_refit = count("Exceeds threshold after refit, retained"),
    Largest_abs_std_residual = max_abs,
    stringsAsFactors = FALSE, row.names = NULL
  )
}

#' Clause appended to the model description.
#' @noRd
outlier_phrase <- function(o) {
  if (is.null(o)) return("")
  if (o$n_removed > 0) {
    sprintf("; %d outlier(s) with |standardised residual| > %g removed before refitting",
            o$n_removed, o$threshold)
  } else {
    sprintf("; outlier screen at |standardised residual| > %g: %d flagged",
            o$threshold, o$n_detected)
  }
}

# ---------------------------------------------------------------------------
# Reproducible ASReml-R script
# ---------------------------------------------------------------------------

#' Deparse a model formula to one tidy line.
#' @noRd
formula_line <- function(f) {
  gsub("[[:space:]]+", " ", paste(deparse(f, width.cutoff = 500L), collapse = " "))
}

#' Everything needed to write out the fitted model as code.
#'
#' Captured inside the fit, where the final specification and the exact
#' formula text are known, so the script reproduces what was fitted rather
#' than what was requested.
#' @param kinship the relationship object, or NULL
#' @noRd
model_code_record <- function(fixed_text, formulae, neighbour_names, opts,
                              multi_env, spatial, kinship, trend) {
  list(
    fixed = fixed_text,
    random = formula_line(formulae$random),
    residual = formula_line(formulae$residual),
    equate = unname(c("Geno", neighbour_names)),
    neighbour_names = neighbour_names,
    multi_env = multi_env, spatial = spatial, kinship = !is.null(kinship),
    kinship_label = if (!is.null(kinship)) kinship$label else NULL,
    maxit = as.integer(opts$maxit %||% 60L),
    workspace = opts$workspace %||% "2gb",
    trend = trend,
    # Filled in by with_outlier_screening(), which alone knows what the first
    # pass removed.
    removed = NULL
  )
}

#' Write the fitted model as a stand-alone ASReml-R script
#'
#' Produces an R script that rebuilds the analysis data with the package's
#' own preparation functions, applies the same field-trend covariates and the
#' same outlier removal, and calls `asreml()` with the formulae that were
#' actually fitted - the model the simplification ladder settled on, not
#' merely the one requested. Running it reproduces the fit outside the
#' interface.
#'
#' @param result A result from [fit_single_model()] or [fit_met_model()].
#' @param data_file Path of the data file, written into the script. Leave
#'   `NULL` for a placeholder to edit.
#' @param map The column mapping passed to [prepare_trial_data()], written
#'   into the script. Leave `NULL` for a placeholder.
#' @param axis The competition direction passed to [add_neighbours()].
#' @param sep,header Field separator of the data file, and whether its first
#'   line holds the column names.
#' @param site For a single trial taken from a multi-site file, a list with
#'   the site `column` and the `value` analysed; the script filters to it.
#' @param file Optional path to write the script to.
#' @return The script as a character vector of lines, invisibly when `file`
#'   is given.
#' @export
asreml_script <- function(result, data_file = NULL, map = NULL, axis = "rows",
                          sep = ",", header = TRUE, site = NULL, file = NULL) {
  code <- result$model_code
  if (is.null(code)) {
    stop("This result carries no model code; refit it with InterPlotComp 3.9.0 ",
         "or later.", call. = FALSE)
  }
  q <- function(x) encodeString(as.character(x), quote = "\"")
  map_text <- if (is.null(map)) {
    sprintf(paste0("map <- list(yield = \"<response>\", geno = \"<genotype>\", ",
                   "row = \"<row>\", column = \"<column>\"%s)  # edit"),
            if (code$multi_env) ", env = \"<environment>\"" else "")
  } else {
    map <- map[!vapply(map, is_blank, logical(1))]
    paste0("map <- list(", paste(sprintf("%s = %s", names(map), q(unlist(map))),
                                 collapse = ", "), ")")
  }
  site_text <- if (!is.null(site) && !is_blank(site$column) && !is_blank(site$value)) {
    c(sprintf("# Analyse site %s only.", q(site$value)),
      sprintf("raw <- raw[trimws(as.character(raw[[%s]])) == %s, , drop = FALSE]",
              q(site$column), q(site$value)))
  }

  lines <- c(
    "# ---------------------------------------------------------------------------",
    sprintf("# ASReml-R script written by InterPlotComp %s on %s", package_version_text(),
            format(Sys.Date())),
    "#",
    paste0("# ", strwrap(paste("Model:", result$description), 74, exdent = 2)),
    "#",
    "# ASReml-R is commercial software licensed by VSNi. Run this in an R",
    "# installation where it is already installed and activated.",
    "# ---------------------------------------------------------------------------",
    "",
    "library(InterPlotComp)",
    "",
    sprintf("raw <- utils::read.csv(%s, header = %s, sep = %s, check.names = FALSE,",
            if (is.null(data_file)) "\"<path to your data file>\"" else q(data_file),
            if (isFALSE(header)) "FALSE" else "TRUE", q(sep %||% ",")),
    "                       stringsAsFactors = FALSE, comment.char = \"\",",
    "                       na.strings = c(\"\", \"NA\", \"na\", \".\", \"-\", \"*\"))",
    "names(raw) <- make.names(trimws(names(raw)), unique = TRUE)",
    site_text,
    map_text,
    sprintf("d <- prepare_trial_data(raw, map%s)",
            if (code$multi_env) ", multi_env = TRUE" else ""),
    if (code$spatial) "d <- complete_field_grid(d)",
    sprintf("nb <- add_neighbours(d, %s)", q(axis %||% "rows")),
    "d <- nb$data"
  )

  if (!is.null(code$trend) && nrow(code$trend)) {
    lines <- c(lines, "",
      "# Global field trend (Gilmour, Cullis & Verbyla 1997): linear row and column",
      "# covariates, centred on the midpoint of each site's grid.",
      "d$lrow <- d$Row_i - ave(d$Row_i, d$Env, FUN = function(x) (max(x) + 1) / 2)",
      "d$lcol <- d$Col_i - ave(d$Col_i, d$Env, FUN = function(x) (max(x) + 1) / 2)")
  }

  if (code$multi_env) {
    lines <- c(lines, "",
      "# facv() needs a factor with one level per direct and per competitive",
      "# environment effect; only its level count and labels matter.",
      "e <- levels(d$Env)",
      "effect_levels <- c(sprintf(\"D%d\", seq_along(e)), sprintf(\"C%d\", seq_along(e)))",
      "d$EffectEnv <- factor(rep(effect_levels, length.out = nrow(d)), levels = effect_levels)",
      "d$EnvDummy <- factor(rep(e, length.out = nrow(d)), levels = e)")
  }

  if (!is.null(code$removed) && nrow(code$removed)) {
    rm <- code$removed
    lines <- c(lines, "",
      sprintf("# %d observation(s) removed as outliers before the final fit", nrow(rm)),
      "# (|standardised conditional residual| above the screening threshold).",
      "removed <- data.frame(",
      sprintf("  Env = c(%s),", paste(q(rm$Environment), collapse = ", ")),
      sprintf("  Row_i = c(%s),", paste(rm$Row_index, collapse = ", ")),
      sprintf("  Col_i = c(%s)", paste(rm$Column_index, collapse = ", ")),
      ")",
      "drop <- paste(d$Env, d$Row_i, d$Col_i) %in% paste(removed$Env, removed$Row_i, removed$Col_i)",
      "d$Yield[drop] <- NA")
  }

  if (code$kinship) {
    lines <- c(lines, "",
      "# Relationship matrix: rebuild it from the same pedigree or marker file.",
      sprintf("# The fitted model used: %s.", code$kinship_label %||% "a relationship matrix"),
      "rel <- build_relationship(\"pedigree\", raw = pedigree, map = pedigree_map)  # edit",
      ".kinship <- rel$ginv",
      sprintf("for (v in %s) d[[v]] <- factor(as.character(d[[v]]), levels = rel$ids)",
              level_vector_text(code$equate)))
  }

  if (code$spatial) {
    lines <- c(lines, "",
      "# The separable residual needs rows varying fastest within columns.",
      if (code$multi_env) "d <- d[order(d$Env, d$Col_i, d$Row_i), ]"
      else "d <- d[order(d$Col_i, d$Row_i), ]")
  }

  lines <- c(lines, "",
    "fit <- asreml::asreml(",
    sprintf("  fixed = %s,", code$fixed),
    sprintf("  random = %s,", code$random),
    sprintf("  residual = %s,", code$residual),
    sprintf("  equate.levels = %s,", level_vector_text(code$equate)),
    "  na.action = asreml::na.method(y = \"include\", x = \"include\"),",
    sprintf("  data = d, maxit = %d, workspace = %s, keep.order = TRUE", code$maxit,
            q(code$workspace)),
    ")",
    "# Continue from the current estimates until ASReml reports convergence.",
    "rounds <- 0",
    "while (!isTRUE(fit$converge) && rounds < 15) {",
    "  fit <- update(fit)",
    "  rounds <- rounds + 1",
    "}",
    "",
    "summary(fit)$varcomp",
    "asreml::wald(fit, denDF = \"numeric\")"
  )
  lines <- lines[!is.na(lines)]

  if (!is.null(file)) {
    writeLines(lines, file, useBytes = FALSE)
    return(invisible(lines))
  }
  lines
}

#' Installed package version, or the source version during development.
#' @noRd
package_version_text <- function() {
  tryCatch(as.character(utils::packageVersion("InterPlotComp")),
           error = function(e) "(development)")
}
