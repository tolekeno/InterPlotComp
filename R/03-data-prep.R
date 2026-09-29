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
#' @noRd
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
#' @export
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

  # ---- optional covariate ------------------------------
  # A proxy for the physical cause of interference - plant height, canopy
  # width, root vigour. Centred so that a slope leaves the intercept as the
  # fitted mean at an average covariate value, which keeps predicted pure-stand
  # yields on the scale a breeder expects.
  if (!is_blank(map$covariate)) {
    trait <- suppressWarnings(as.numeric(raw[[map$covariate]]))
    if (all(is.na(trait))) {
      stop("The selected covariate '", map$covariate,
           "' contains no numeric values.", call. = FALSE)
    }
    if (sum(!is.na(trait)) < 0.5 * length(trait)) {
      stop("The covariate '", map$covariate, "' is missing for ",
           round(100 * mean(is.na(trait))), "% of plots. Adjusting for a covariate ",
           "that is mostly absent would bias the competitive effects more than ",
           "it corrects them.", call. = FALSE)
    }
    d$Covariate <- trait
    d$Covariate_c <- trait - mean(trait, na.rm = TRUE)
    attr(d, "covariate_mean") <- mean(trait, na.rm = TRUE)
    attr(d, "covariate_name") <- map$covariate
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
  attr(d, "covariate_mean")  <- if ("Covariate" %in% names(d)) mean(d$Covariate, na.rm = TRUE) else NULL
  attr(d, "covariate_name")  <- if (!is_blank(map$covariate)) map$covariate else NULL
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
#' @noRd
build_design_factors <- function(d, multi_env = FALSE) {
  has_rep   <- "Rep" %in% names(d)
  has_block <- "Block" %in% names(d)
  parts <- if (multi_env) list(d$Env) else list()

  if (has_rep) {
    # A factor, so that a MET can fit it within each site as at(Env):Rep.
    d$Rep <- factor(d$Rep, levels = ordered_unique(stats::na.omit(d$Rep)))
    d$RepF <- droplevels(interaction(c(parts, list(d$Rep)), drop = TRUE, sep = ":"))
    d$RepF[is.na(d$Rep)] <- NA
    d$RepF <- droplevels(d$RepF)
  }
  if (has_block) {
    d$Block <- factor(d$Block, levels = ordered_unique(stats::na.omit(d$Block)))
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
#' @noRd
available_design_terms <- function(d) intersect(c("RepF", "BlockF"), names(d))

#' A design term fitted within each site that can support it.
#'
#' A MET writes the term as `at(Env):f`, or `at(Env, <sites>):f` when some
#' sites have a single level of `f` and so nothing to estimate; a single trial
#' writes plain `f`. The `sites` attribute records the sites the term covers.
#' @param key the values whose distinct count decides whether a site is covered
#' @return the term, or NULL when no site has two or more levels
#' @noRd
site_specific_term <- function(d, f, key, multi_env = FALSE) {
  env <- droplevels(d$Env)
  n <- tapply(key, env, function(x) length(unique(stats::na.omit(x))))
  sites <- names(n)[!is.na(n) & n >= 2L]
  if (!length(sites)) return(NULL)
  term <- if (!multi_env) f else at_env_term(f, sites, levels(env))
  structure(term, sites = sites)
}

#' `at(Env):f` when a term covers every site, else `at(Env, <sites>):f`.
#' @noRd
at_env_term <- function(f, sites, all_sites) {
  if (setequal(sites, all_sites)) sprintf("at(Env):%s", f)
  else sprintf("at(Env, %s):%s", level_vector_text(sites), f)
}

#' Random row and column terms for the field layout.
#'
#' Rows and columns are part of the physical structure of every trial, so
#' their random effects are fitted alongside the spatial residual (Gilmour,
#' Cullis & Verbyla 1997): the AR1 x AR1 process models smooth local trend,
#' while `Row` and `Column` absorb whole-row and whole-column effects such as
#' harvesting or sowing direction. A MET fits them within each site,
#' `at(Env):Row`, so every site has its own row and column variance. A site
#' with a single row (or column) has nothing to estimate on that axis and is
#' left out of it. The `sites` attribute gives the sites each term covers.
#' @noRd
row_column_terms <- function(d, multi_env = FALSE) {
  out <- character(0)
  covered <- list()
  for (f in names(FIELD_AXES)) {
    term <- site_specific_term(d, f, d[[FIELD_AXES[[f]]]], multi_env)
    if (is.null(term)) next
    out <- c(out, as.character(term))
    covered[[term]] <- attr(term, "sites")
  }
  attr(out, "sites") <- covered
  out
}

FIELD_AXES <- c(Row = "Row_i", Column = "Col_i")

#' Is a design term one of the structural row or column terms?
#' @noRd
is_row_column_term <- function(x) grepl("(^|:)(Row|Column)$", x)

#' Random design terms for a fit: replicate, block, row and column.
#'
#' A MET fits the replicate and block within each site, `at(Env):Rep` and
#' `at(Env):Block`, so every site has its own replicate and block variance; a
#' single trial fits `RepF` and `BlockF`. Block labels are often re-used in
#' every replicate, and `at(Env):Block` would then merge block 1 of replicate 1
#' with block 1 of replicate 2, so at such a site the block term is written
#' `at(Env):Rep:Block` instead.
#'
#' A replicate or block factor that groups the plots exactly as the rows or
#' columns do - blocks laid out as whole columns, say - is the same random
#' effect under another name. Fitting both makes the Average Information
#' matrix singular, so the row or column term is kept, as the structural one,
#' and the duplicate is dropped with a note for the fitting log. In a MET the
#' check runs site by site: a site where blocks are whole columns is left out
#' of the block term, and the other sites keep their block variances.
#' @return character vector of terms with a `notes` attribute
#' @noRd
model_design_terms <- function(d, multi_env = FALSE) {
  blocking <- available_design_terms(d)
  layout <- row_column_terms(d, multi_env)
  sites <- attr(layout, "sites")
  # The grouping of the plots behind every site-specific term, for the
  # duplicate check below.
  keys <- list()
  stems <- list()
  for (l in layout) keys[[l]] <- d[[FIELD_AXES[[sub("^.*:", "", l)]]]]
  notes <- character(0)

  if (multi_env) {
    site_version <- function(term, f, key) {
      t <- site_specific_term(d, f, key, multi_env = TRUE)
      if (is.null(t)) return(setdiff(blocking, term))
      sites[[t]] <<- attr(t, "sites")
      keys[[t]] <<- key
      stems[[t]] <<- f
      replace(blocking, blocking == term, as.character(t))
    }
    if ("RepF" %in% blocking) blocking <- site_version("RepF", "Rep", d$Rep)
    if ("BlockF" %in% blocking) {
      labelled <- !is.na(d$Block) & !is.na(d$Rep %||% NA)
      reused <- "Rep" %in% names(d) && any(tapply(
        d$Rep[labelled], paste(d$Env, d$Block)[labelled],
        function(r) length(unique(r)) > 1L))
      if (reused) {
        notes <- c(notes, paste(
          "Block labels are re-used across replicates, so blocks are fitted",
          "within replicate and site, at(Env):Rep:Block."))
      }
      blocking <- site_version("BlockF", if (reused) "Rep:Block" else "Block",
                               d$BlockF)
    }
  }

  # The plots a term covers, and the group each of them falls in.
  observed <- !d$Padded %in% TRUE
  grouping <- function(term) {
    if (is.null(sites[[term]])) {
      return(list(covers = observed & !is.na(d[[term]]),
                  group = as.character(d[[term]])))
    }
    x <- keys[[term]]
    list(covers = observed & !is.na(x) & as.character(d$Env) %in% sites[[term]],
         group = paste(d$Env, x))
  }

  # Do two terms group the same plots identically, among the plots `within`?
  same_grouping <- function(b, l, within = TRUE) {
    gb <- grouping(b)
    gl <- grouping(l)
    cb <- gb$covers & within
    cl <- gl$covers & within
    if (!any(cb) || !identical(cb, cl)) return(FALSE)
    a <- gb$group[cb]
    z <- gl$group[cl]
    length(unique(a)) == length(unique(z)) &&
      length(unique(paste(a, z))) == length(unique(a))
  }
  what <- function(b) sub(" within .*$| \\(.*\\)$", "", pretty_term(b))
  axis_of <- function(l) tolower(sub("^.*:", "", l))

  kept <- character(0)
  for (b in blocking) {
    if (is.null(stems[[b]])) {
      # A single trial: a duplicate is dropped whole.
      dup <- Find(function(l) same_grouping(b, l), layout)
      if (is.null(dup)) {
        kept <- c(kept, b)
      } else {
        notes <- c(notes, sprintf(
          "The %s factor groups the plots exactly as the field %ss do, so it is fitted once, as the %s term.",
          what(b), axis_of(dup), axis_of(dup)))
      }
      next
    }
    # A MET: blocks may be whole columns at one site and cut across them at
    # the next, so the check runs site by site, and a site where the factor
    # duplicates a row or column term is left out of this term only.
    keep <- sites[[b]]
    for (s in sites[[b]]) {
      at_site <- as.character(d$Env) == s
      dup <- Find(function(l) s %in% sites[[l]] && same_grouping(b, l, at_site),
                  layout)
      if (!is.null(dup)) {
        keep <- setdiff(keep, s)
        notes <- c(notes, sprintf(
          "At %s the %s factor groups the plots exactly as the field %ss do, so it is fitted there once, as the %s term.",
          s, what(b), axis_of(dup), axis_of(dup)))
      }
    }
    if (length(keep)) {
      kept <- c(kept, if (identical(keep, sites[[b]])) b else
        at_env_term(stems[[b]], keep, levels(droplevels(d$Env))))
    }
  }
  out <- c(kept, layout)
  attributes(out) <- NULL
  attr(out, "notes") <- notes
  out
}

#' Per-environment field-layout summary, used for on-screen diagnostics.
#' @noRd
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
      Grid = paste0(n_rows, " \u00d7 ", n_cols),
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

#' Pad every environment to its full rectangular grid
#'
#' Inserts the field positions that are absent from the data, with a missing
#' response, so that each environment forms the complete rectangle an
#' AR1 x AR1 residual requires. Indexing with `NA` reproduces each column's
#' class and factor levels, so the padded records are structurally identical to
#' real plots but carry no response, no genotype and no design membership.
#'
#' @param d prepared trial data from [prepare_trial_data()].
#' @return The same data frame with padded rows added, a logical `Padded`
#'   column marking them, and an `n_padded` attribute giving how many were
#'   inserted.
#' @export
#' @examples
#' d <- prepare_trial_data(
#'   sample_single_trial(),
#'   list(yield = "Yield_t_ha", geno = "Genotype", row = "Row", column = "Column")
#' )
#' nrow(d)
#' nrow(complete_field_grid(d))
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
  rows    = "Adjacent field rows (Row \u00b1 1, same column)",
  columns = "Adjacent columns (Column \u00b1 1, same row)",
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
#' @export
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
  # Neighbour value of the covariate, summed over exactly the neighbours
  # that supply the competitive genetic effects. An absent neighbour
  # contributes zero, which after centring means "an average neighbour" - the
  # same convention na.method(x = "include") applies to the genetic terms.
  if ("Covariate_c" %in% names(d)) {
    nb_covariate <- matrix(NA_real_, nrow(d), length(offsets))
    for (i in seq_along(offsets)) {
      key_i <- paste(as.integer(d$Env),
                     d$Row_i + offsets[[i]][1],
                     d$Col_i + offsets[[i]][2], sep = "/")
      nb_covariate[, i] <- d$Covariate_c[match(key_i, plot_key)]
    }
    d$Covariate_nb <- rowSums(nb_covariate, na.rm = TRUE)
    d$Covariate_own <- ifelse(is.na(d$Covariate_c), 0, d$Covariate_c)
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
#' @param d The trial data with neighbour factors attached, i.e.
#'   `add_neighbours(...)$data`.
#' @param neighbour_names The neighbour column names, i.e.
#'   `add_neighbours(...)$names`.
#' @return A list with the number of observed plots, the percentage carrying a
#'   complete neighbour set, the mean number of neighbours, the number of
#'   border plots, the mean number of distinct neighbour genotypes per
#'   genotype, the percentage of self-neighbour pairings, the number of
#'   genotypes and the neighbour count `k`.
#' @seealso [competition_warnings()], which turns this into plain-English
#'   warnings.
#' @export
#' @examples
#' d <- prepare_trial_data(
#'   sample_single_trial(),
#'   list(yield = "Yield_t_ha", geno = "Genotype", row = "Row", column = "Column")
#' )
#' nb <- add_neighbours(complete_field_grid(d), "rows")
#' competition_diagnostics(nb$data, nb$names)
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

#' Warnings about a layout that cannot support a competition model
#'
#' Turns [competition_diagnostics()] into the specific sentences a user needs
#' to read before trusting a fit. Three things undermine a competition model
#' and are invisible in a plain data preview: too many border plots, neighbour
#' pairings that repeat across replicates, and too small a genotype panel.
#'
#' @param x The list returned by [competition_diagnostics()].
#' @return A character vector of warnings, empty when the layout raises none.
#' @export
#' @examples
#' d <- prepare_trial_data(
#'   sample_single_trial(),
#'   list(yield = "Yield_t_ha", geno = "Genotype", row = "Row", column = "Column")
#' )
#' nb <- add_neighbours(complete_field_grid(d), "rows")
#' competition_warnings(competition_diagnostics(nb$data, nb$names))
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
