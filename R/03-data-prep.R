# ---------------------------------------------------------------------------
# Data ingestion, validation and field-layout preparation
#
# Single-environment and multi-environment trials share one code path. A
# single-environment trial is represented internally as one environment named
# "Trial", so neighbour construction, grid completion and design summaries are
# written once and behave identically in both workspaces.
#
# Design decisions worth knowing about:
#
#  * Field coordinates are re-indexed to a contiguous 1..n integer grid *per
#    environment*, expanding integer labels across their full span. An entirely
#    absent field row would otherwise collapse out of the factor and make AR1
#    treat the plots either side of it as adjacent.
#
#  * Missing grid positions are padded with Yield = NA rather than rejected.
#    These records define the residual covariance layout; they carry no
#    genotype and therefore contribute no direct or competitive effect.
#
#  * Border plots legitimately have fewer neighbours. Their absent neighbour
#    factors are left as NA and handled by `na.method(x = "include")`, which
#    contributes a zero row to the design matrix. The neighbour count is
#    retained so it can be reported and used when interpreting pure-stand
#    values.
# ---------------------------------------------------------------------------

#' Read an uploaded delimited file with sensible missing-value handling.
#'
#' @param path file path from `fileInput`
#' @param header logical, first line holds column names
#' @param sep field separator
read_trial_file <- function(path, header = TRUE, sep = ",") {
  d <- utils::read.csv(
    path, header = header, sep = sep, check.names = FALSE,
    stringsAsFactors = FALSE, na.strings = c("", "NA", "na", ".", "-", "*"),
    comment.char = ""
  )
  if (!nrow(d)) stop("The uploaded file contains no data rows.", call. = FALSE)
  if (!ncol(d)) stop("The uploaded file contains no columns.", call. = FALSE)
  names(d) <- clean_names(names(d))
  d
}

#' Validate and reshape an uploaded trial into the internal analysis layout.
#'
#' @param raw data frame as uploaded
#' @param map named list of source column names: yield, geno, row, column and
#'   optionally env, rep, block
#' @param multi_env TRUE for the MET workspace
#' @return data frame with the internal analysis columns, carrying a
#'   `field_summary` attribute
prepare_trial_data <- function(raw, map, multi_env = FALSE) {
  required <- c("yield", "geno", "row", "column", if (multi_env) "env")
  missing_map <- required[vapply(required, function(k) is_blank(map[[k]]), logical(1))]
  if (length(missing_map)) {
    stop("Select a column for: ",
         paste(c(yield = "Response", geno = "Genotype", row = "Field row",
                 column = "Field column", env = "Environment")[missing_map],
               collapse = ", "), ".", call. = FALSE)
  }
  unknown <- setdiff(unlist(map[nzchar(unlist(map))]), names(raw))
  if (length(unknown)) {
    stop("These mapped columns are not in the file: ",
         paste(unknown, collapse = ", "), ".", call. = FALSE)
  }

  d <- data.frame(
    Yield = suppressWarnings(as.numeric(raw[[map$yield]])),
    Geno  = trimws(as.character(raw[[map$geno]])),
    Row_label    = trimws(as.character(raw[[map$row]])),
    Column_label = trimws(as.character(raw[[map$column]])),
    stringsAsFactors = FALSE
  )
  d$Env <- if (multi_env) trimws(as.character(raw[[map$env]])) else "Trial"

  # ---- response -----------------------------------------------------------
  n_numeric <- sum(!is.na(d$Yield))
  if (n_numeric == 0L) {
    stop("The selected response column '", map$yield,
         "' contains no numeric values. Check the decimal separator and any ",
         "text codes used for missing plots.", call. = FALSE)
  }
  n_coerced <- sum(is.na(d$Yield) & !is.na(raw[[map$yield]]) &
                     nzchar(trimws(as.character(raw[[map$yield]]))))
  if (n_numeric < 10L) {
    stop("Only ", n_numeric, " numeric response values were found. At least 10 ",
         "observed plots are needed to fit a competition model.", call. = FALSE)
  }

  # ---- identifiers --------------------------------------------------------
  d$Geno[!nzchar(d$Geno) | d$Geno %in% c("NA", ".")] <- NA_character_
  bad_geno <- which(is.na(d$Geno) & !is.na(d$Yield))
  if (length(bad_geno)) {
    stop(length(bad_geno), " plot(s) have an observed response but no genotype ",
         "(first at file row ", bad_geno[1] + 1L, "). Supply the genotype, or ",
         "set the response to NA if the plot identity is unknown.", call. = FALSE)
  }
  if (anyNA(d$Env) | any(!nzchar(d$Env))) {
    stop("Environment identifiers cannot be blank.", call. = FALSE)
  }
  missing_coord <- which(is.na(d$Row_label) | is.na(d$Column_label) |
                           !nzchar(d$Row_label) | !nzchar(d$Column_label))
  if (length(missing_coord)) {
    stop(length(missing_coord), " plot(s) have a missing field row or column ",
         "(first at file row ", missing_coord[1] + 1L, "). Physical field ",
         "coordinates are required for every record.", call. = FALSE)
  }

  d$Env <- factor(d$Env, levels = ordered_unique(d$Env))
  if (multi_env && nlevels(d$Env) < 2L) {
    stop("The multi-environment workspace needs at least two environments; ",
         "the file contains only '", levels(d$Env)[1],
         "'. Use the single-trial workspace instead.", call. = FALSE)
  }

  # ---- optional design factors -------------------------------------------
  for (key in c("rep", "block")) {
    if (!is_blank(map[[key]])) {
      value <- trimws(as.character(raw[[map[[key]]]]))
      value[!nzchar(value)] <- NA_character_
      d[[c(rep = "Rep", block = "Block")[key]]] <- value
    }
  }

  # ---- per-environment field indexing ------------------------------------
  # Levels are built independently within each environment so that trials of
  # different sizes, or with different coordinate origins, all map onto their
  # own contiguous 1..n grid.
  d$Row_i <- NA_integer_
  d$Col_i <- NA_integer_
  for (e in levels(d$Env)) {
    take <- d$Env == e
    d$Row_i[take] <- match(d$Row_label[take], coordinate_levels(d$Row_label[take]))
    d$Col_i[take] <- match(d$Column_label[take], coordinate_levels(d$Column_label[take]))
  }
  if (anyNA(d$Row_i) || anyNA(d$Col_i)) {
    stop("Field coordinates could not be indexed. Mixed numeric and text ",
         "labels in the same row or column column are the usual cause.",
         call. = FALSE)
  }

  duplicated_positions <- duplicated(d[c("Env", "Row_i", "Col_i")])
  if (any(duplicated_positions)) {
    first <- which(duplicated_positions)[1]
    stop(sum(duplicated_positions), " plot(s) share a field position with an ",
         "earlier record (first: environment '", as.character(d$Env[first]),
         "', row ", d$Row_label[first], ", column ", d$Column_label[first],
         "). Each Environment-Row-Column combination must identify one plot.",
         call. = FALSE)
  }

  d$Geno <- factor(d$Geno, levels = ordered_unique(stats::na.omit(d$Geno)))
  if (nlevels(d$Geno) < 3L) {
    stop("Only ", nlevels(d$Geno), " genotype(s) were found. A competition ",
         "model needs a genotype panel, not a single entry.", call. = FALSE)
  }

  d$Row    <- factor(d$Row_i,  levels = seq_len(max(d$Row_i)))
  d$Column <- factor(d$Col_i,  levels = seq_len(max(d$Col_i)))
  d$Padded <- FALSE

  d <- build_design_factors(d, multi_env = multi_env)
  d <- d[order(d$Env, d$Col_i, d$Row_i), , drop = FALSE]
  rownames(d) <- NULL

  attr(d, "multi_env")   <- multi_env
  attr(d, "n_coerced")   <- n_coerced
  attr(d, "field_summary") <- field_summary(d)
  d
}

#' Build replicate and block factors, nested within environment where relevant.
#'
#' Block labels are routinely re-used across replicates and across
#' environments. Nesting them into a single observed-level factor avoids the
#' empty cells that `Env:Rep:Block` would otherwise generate, which are a
#' common cause of singular Average Information matrices.
build_design_factors <- function(d, multi_env = FALSE) {
  has_rep   <- "Rep" %in% names(d)
  has_block <- "Block" %in% names(d)
  parts <- if (multi_env) list(d$Env) else list()

  if (has_rep) {
    d$RepF <- droplevels(interaction(c(parts, list(d$Rep)), drop = TRUE, sep = ":"))
    d$RepF[is.na(d$Rep)] <- NA
    d$RepF <- droplevels(d$RepF)
  }
  if (has_block) {
    block_parts <- c(parts, if (has_rep) list(d$Rep) else NULL, list(d$Block))
    d$BlockF <- droplevels(interaction(block_parts, drop = TRUE, sep = ":"))
    d$BlockF[is.na(d$Block)] <- NA
    d$BlockF <- droplevels(d$BlockF)
    # A block factor with one level per plot, or a single level overall,
    # carries no information and would only destabilise the fit.
    if (nlevels(d$BlockF) < 2L || nlevels(d$BlockF) >= sum(!is.na(d$Yield))) {
      d$BlockF <- NULL
    }
  }
  if (has_rep && !is.null(d$RepF) && nlevels(d$RepF) < 2L) d$RepF <- NULL
  d
}

#' Which design terms are available in the prepared data?
available_design_terms <- function(d) intersect(c("RepF", "BlockF"), names(d))

#' Per-environment field-layout summary, used for on-screen diagnostics.
field_summary <- function(d) {
  out <- lapply(levels(d$Env), function(e) {
    z <- d[d$Env == e, , drop = FALSE]
    n_rows <- max(z$Row_i)
    n_cols <- max(z$Col_i)
    counts <- table(droplevels(stats::na.omit(z$Geno)))
    data.frame(
      Environment = e,
      Rows = n_rows,
      Columns = n_cols,
      Grid = paste0(n_rows, " × ", n_cols),
      Plots = nrow(z),
      Observed = sum(!is.na(z$Yield)),
      Missing_response = sum(is.na(z$Yield)),
      Gaps_in_grid = n_rows * n_cols - nrow(z),
      Genotypes = length(counts),
      Min_reps = if (length(counts)) min(counts) else NA_integer_,
      Max_reps = if (length(counts)) max(counts) else NA_integer_,
      Unreplicated = if (length(counts)) sum(counts == 1L) else NA_integer_,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, out)
}

#' Pad every environment to its full rectangular grid with Yield = NA.
#'
#' Indexing with NA reproduces each column's class and factor levels, so the
#' padded records are structurally identical to real plots but carry no
#' response, no genotype and no design membership.
complete_field_grid <- function(d) {
  env_levels <- levels(d$Env)
  sections <- lapply(env_levels, function(e) {
    z <- d[d$Env == e, , drop = FALSE]
    grid <- expand.grid(Row_i = seq_len(max(z$Row_i)), Col_i = seq_len(max(z$Col_i)),
                        KEEP.OUT.ATTRS = FALSE)
    matched <- match(paste(grid$Row_i, grid$Col_i, sep = "/"),
                     paste(z$Row_i, z$Col_i, sep = "/"))
    padded <- z[matched, , drop = FALSE]
    padded$Env    <- factor(e, levels = env_levels)
    padded$Row_i  <- grid$Row_i
    padded$Col_i  <- grid$Col_i
    padded$Padded <- is.na(matched)
    padded$Row_label[padded$Padded]    <- as.character(grid$Row_i[padded$Padded])
    padded$Column_label[padded$Padded] <- as.character(grid$Col_i[padded$Padded])
    padded
  })
  out <- do.call(rbind, sections)
  out$Env    <- factor(as.character(out$Env), levels = env_levels)
  out$Row    <- factor(out$Row_i, levels = seq_len(max(out$Row_i)))
  out$Column <- factor(out$Col_i, levels = seq_len(max(out$Col_i)))
  rownames(out) <- NULL
  attr(out, "multi_env") <- attr(d, "multi_env")
  attr(out, "n_padded")  <- sum(out$Padded)
  out
}

# Neighbour offsets, as (row, column) displacements from the focal plot.
NEIGHBOUR_OFFSETS <- list(
  rows    = list(c(-1, 0), c(1, 0)),
  columns = list(c(0, -1), c(0, 1)),
  four    = list(c(-1, 0), c(1, 0), c(0, -1), c(0, 1))
)

NEIGHBOUR_LABELS <- c(
  rows    = "Adjacent field rows (Row ± 1, same column)",
  columns = "Adjacent columns (Column ± 1, same row)",
  four    = "Four orthogonal neighbours"
)

#' Attach neighbour-genotype factors to the trial data.
#'
#' Each neighbour column N1..Nk holds the genotype growing in the adjacent
#' plot, as a factor sharing the genotype level set. Absent neighbours (field
#' borders, padded positions, unplanted plots) stay NA and are absorbed by
#' `na.method(x = "include")` as a zero row in the design matrix.
#'
#' @param d prepared trial data
#' @param axis one of "rows", "columns", "four"
#' @return list(data, names, k, label)
add_neighbours <- function(d, axis = "rows") {
  offsets <- NEIGHBOUR_OFFSETS[[axis]]
  if (is.null(offsets)) stop("Unknown competition direction: ", axis, call. = FALSE)

  plot_key <- paste(as.integer(d$Env), d$Row_i, d$Col_i, sep = "/")
  nm <- paste0("N", seq_along(offsets))
  levs <- levels(d$Geno)

  for (i in seq_along(offsets)) {
    neighbour_key <- paste(as.integer(d$Env),
                           d$Row_i + offsets[[i]][1],
                           d$Col_i + offsets[[i]][2], sep = "/")
    d[[nm[i]]] <- factor(as.character(d$Geno[match(neighbour_key, plot_key)]),
                         levels = levs)
  }
  d$Neighbour_count <- rowSums(!is.na(d[nm]))
  list(data = d, names = nm, k = length(nm), label = unname(NEIGHBOUR_LABELS[axis]))
}

#' Diagnostics on how well the competition term is supported by the layout.
#'
#' Two things commonly undermine a competition model and are invisible in a
#' plain data preview: too many border plots (so most records carry an
#' incomplete neighbour set), and neighbour pairings that repeat across
#' replicates (so a genotype is nearly always beside the same neighbour, making
#' direct and competitive effects hard to separate).
competition_diagnostics <- function(d, neighbour_names) {
  observed <- d[!is.na(d$Yield) & !is.na(d$Geno), , drop = FALSE]
  k <- length(neighbour_names)

  pairs <- do.call(rbind, lapply(neighbour_names, function(n) {
    z <- observed[!is.na(observed[[n]]), c("Geno", n)]
    if (!nrow(z)) return(NULL)
    data.frame(focal = as.character(z$Geno), nb = as.character(z[[n]]),
               stringsAsFactors = FALSE)
  }))

  distinct_neighbours <- if (is.null(pairs)) 0 else {
    mean(tapply(pairs$nb, pairs$focal, function(x) length(unique(x))), na.rm = TRUE)
  }
  self_neighbour <- if (is.null(pairs)) 0 else mean(pairs$focal == pairs$nb) * 100

  list(
    n_observed = nrow(observed),
    full_neighbour_pct = 100 * mean(observed$Neighbour_count == k),
    mean_neighbours = mean(observed$Neighbour_count),
    border_plots = sum(observed$Neighbour_count < k),
    mean_distinct_neighbours = distinct_neighbours,
    self_neighbour_pct = self_neighbour,
    n_genotypes = nlevels(droplevels(observed$Geno)),
    k = k
  )
}

#' Human-readable warnings derived from `competition_diagnostics()`.
competition_warnings <- function(x) {
  msg <- character(0)
  if (x$full_neighbour_pct < 50) {
    msg <- c(msg, sprintf(
      "Only %.0f%% of observed plots have a complete set of %d neighbours. The competition effect is estimated mainly from interior plots; consider whether the chosen direction matches the field layout.",
      x$full_neighbour_pct, x$k))
  }
  if (x$mean_distinct_neighbours < 2) {
    msg <- c(msg, sprintf(
      "Each genotype sits beside only %.1f distinct neighbour genotypes on average. Direct and competitive effects are weakly separated in this layout, so treat the competition estimates as indicative.",
      x$mean_distinct_neighbours))
  }
  if (x$n_genotypes < 15) {
    msg <- c(msg, sprintf(
      "Only %d genotypes contribute observations. Genetic variance components, and especially the direct-competition covariance, will be imprecise.",
      x$n_genotypes))
  }
  msg
}
