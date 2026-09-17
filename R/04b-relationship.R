# ---------------------------------------------------------------------------
# Pedigree and genomic relationship matrices
#
# Genetic effects are by default independent between genotypes. Supplying a
# numerator relationship matrix A (from a pedigree) or a genomic relationship
# matrix G (from markers) replaces that assumption with a known covariance
# between relatives. For a competition model this matters twice over: the
# competitive effect is the harder of the two to estimate, and it borrows most
# of its information from relatives.
#
# HOW ASReml-R IS DRIVEN (verified against ASReml-R 4.2)
#
#  * `vm(f, ginv)` must appear in BOTH the model formula and the variance
#    formula of `str()`. Written as
#        str(~ vm(Geno, K) + vm(N1, K) + and(vm(N2, K)), ~ us(2):id(n))
#    ASReml accepts it, fits happily, and silently returns the *identity*
#    answer: the `str()` variance formula overrides the relationship. The
#    correct form ends `~ us(2):vm(Geno, K)`. This is a quiet wrong-answer
#    trap, not an error, so the application never builds the `id()` form when a
#    relationship matrix is in use.
#
#  * ASReml resolves the second argument of `vm()` from the *calling frame*,
#    not from the formula's environment. The object must therefore exist as an
#    ordinary local variable, named exactly as it appears in the formula text,
#    in the function that calls `asreml()`. That name is `.kinship` throughout.
#
#  * Every source (pedigree, kinship matrix, markers) is converted to the same
#    sparse inverse representation that `ainverse()` returns, so there is one
#    code path and one set of scoping rules.
#
#  * `vm()` expands the term to *all* individuals in the relationship matrix,
#    including ancestors with no plot in the trial. Their effects are predicted
#    and reported, flagged as not tested.
# ---------------------------------------------------------------------------

RELATIONSHIP_SOURCES <- c(
  "None \u2014 independent genotypes"               = "none",
  "Pedigree (identifier, male parent, female parent)" = "pedigree",
  "Relationship / kinship matrix (square)"          = "kinship",
  "Marker matrix (genotypes \u00d7 markers)"        = "markers"
)

#' Convert a relationship matrix to ASReml's sparse inverse representation.
#'
#' Reproduces the structure and attributes of `asreml::ainverse()`: the lower
#' triangle of the inverse as (row, column, value) with `rowNames`, `logdet`
#' and `inbreeding` attached, and class "ginv".
#'
#' `logdet` is log|K|, NOT log|K inverse|. ASReml adds it to the REML
#' log-likelihood, so the wrong sign shifts the reported log-likelihood, AIC
#' and BIC by 2 log|K| per genetic block while leaving the variance components
#' untouched - a discrepancy that is easy to miss.
#'
#' @param K a symmetric relationship matrix with dimnames
#' @param tol values below this in the inverse are treated as structural zeros
#' @noRd
kinship_to_ginv <- function(K, tol = 1e-10) {
  K <- as.matrix(K)
  ids <- rownames(K)
  if (is.null(ids)) stop("The relationship matrix needs row names identifying genotypes.",
                         call. = FALSE)
  K <- (K + t(K)) / 2

  Ki <- tryCatch(chol2inv(chol(K)), error = function(e) {
    tryCatch(solve(K), error = function(e2) {
      stop("The relationship matrix could not be inverted. It is singular, ",
           "which is normal for a genomic matrix: increase the blending ",
           "weight so a small multiple of the identity is added.", call. = FALSE)
    })
  })
  Ki <- (Ki + t(Ki)) / 2

  idx <- which(lower.tri(Ki, diag = TRUE) & abs(Ki) > tol, arr.ind = TRUE)
  out <- cbind(Row = idx[, 1], Column = idx[, 2], Ainverse = Ki[idx])
  out <- out[order(out[, 1], out[, 2]), , drop = FALSE]

  attr(out, "rowNames") <- ids
  attr(out, "geneticGroups") <- c(0L, 0L)
  attr(out, "logdet") <- as.numeric(determinant(K, logarithm = TRUE)$modulus)
  attr(out, "inbreeding") <- stats::setNames(diag(K) - 1, ids)
  class(out) <- c("ginv", "matrix", "array")
  out
}

#' Dense relationship matrix implied by a sparse inverse, for plotting.
#'
#' Only built for modest numbers of individuals; a heatmap of thousands of
#' genotypes is neither readable nor cheap.
#' @noRd
ginv_to_matrix <- function(g, max_n = 400L) {
  ids <- attr(g, "rowNames")
  n <- length(ids)
  if (n > max_n) return(NULL)
  Ki <- matrix(0, n, n)
  Ki[cbind(g[, 1], g[, 2])] <- g[, 3]
  Ki[cbind(g[, 2], g[, 1])] <- g[, 3]
  out <- tryCatch(solve(Ki), error = function(e) NULL)
  if (!is.null(out)) dimnames(out) <- list(ids, ids)
  out
}

# ---------------------------------------------------------------------------
# Pedigree
# ---------------------------------------------------------------------------

#' Validate and order a three-column pedigree, then build A-inverse.
#'
#' `ainverse()` requires every parent to appear as an identifier before it is
#' used, so unknown parents are added as founders and the pedigree is sorted so
#' that parents precede their progeny.
#' @noRd
build_pedigree_ginv <- function(raw, map) {
  need <- c("id", "sire", "dam")
  if (any(vapply(need, function(k) is_blank(map[[k]]), logical(1)))) {
    stop("Select the identifier, male-parent and female-parent columns of the pedigree.",
         call. = FALSE)
  }
  ped <- data.frame(
    ID   = trimws(as.character(raw[[map$id]])),
    Sire = trimws(as.character(raw[[map$sire]])),
    Dam  = trimws(as.character(raw[[map$dam]])),
    stringsAsFactors = FALSE
  )
  blank <- function(x) is.na(x) | !nzchar(x) | x %in% c("0", "NA", "na", ".", "-", "*")
  ped$Sire[blank(ped$Sire)] <- NA_character_
  ped$Dam[blank(ped$Dam)]   <- NA_character_

  if (any(blank(ped$ID))) stop("The pedigree contains rows with no identifier.", call. = FALSE)
  dup <- duplicated(ped$ID)
  if (any(dup)) {
    stop(sum(dup), " identifier(s) appear more than once in the pedigree (first: '",
         ped$ID[dup][1], "'). Each individual needs exactly one row.", call. = FALSE)
  }
  self <- which(ped$ID == ped$Sire | ped$ID == ped$Dam)
  if (length(self)) {
    stop("Individual '", ped$ID[self[1]], "' is listed as its own parent.", call. = FALSE)
  }

  # Parents with no row of their own become founders.
  missing_parents <- setdiff(stats::na.omit(c(ped$Sire, ped$Dam)), ped$ID)
  if (length(missing_parents)) {
    ped <- rbind(
      data.frame(ID = missing_parents, Sire = NA_character_, Dam = NA_character_,
                 stringsAsFactors = FALSE),
      ped)
  }
  ped <- sort_pedigree(ped)

  ginv <- tryCatch(asreml::ainverse(ped), error = function(e) {
    stop("ASReml could not build the inverse relationship matrix from this ",
         "pedigree.\n\nASReml reported: ", conditionMessage(e), call. = FALSE)
  })
  attr(ginv, "n_founders") <- sum(is.na(ped$Sire) & is.na(ped$Dam))
  attr(ginv, "pedigree") <- ped
  ginv
}

#' Order a pedigree so that every parent precedes its progeny.
#'
#' Repeated generation peeling rather than a recursive sort: it is simple,
#' terminates on a cycle, and reports the offending individuals.
#' @noRd
sort_pedigree <- function(ped) {
  placed <- character(0)
  out <- vector("list", 0)
  remaining <- ped
  while (nrow(remaining)) {
    ready <- (is.na(remaining$Sire) | remaining$Sire %in% placed) &
      (is.na(remaining$Dam) | remaining$Dam %in% placed)
    if (!any(ready)) {
      stop("The pedigree contains a loop: individual '", remaining$ID[1],
           "' is among its own ancestors. Correct the parentage and try again.",
           call. = FALSE)
    }
    out[[length(out) + 1L]] <- remaining[ready, , drop = FALSE]
    placed <- c(placed, remaining$ID[ready])
    remaining <- remaining[!ready, , drop = FALSE]
  }
  do.call(rbind, out)
}

# ---------------------------------------------------------------------------
# Genomic
# ---------------------------------------------------------------------------

#' Genomic relationship matrix from a marker matrix (VanRaden method 1).
#'
#' @param M numeric matrix, rows genotypes, columns markers, coded 0/1/2 or
#'   -1/0/1 (detected automatically)
#' @param blend proportion of the identity blended in. A raw G matrix is
#'   singular whenever there are fewer markers than genotypes, or duplicated
#'   genotypes, so a small ridge is required before it can be inverted.
#' @param min_maf markers below this minor allele frequency are dropped
#' @noRd
markers_to_grm <- function(M, blend = 0.01, min_maf = 0.01) {
  M <- as.matrix(M)
  if (is.null(rownames(M))) {
    stop("The marker file needs a first column of genotype identifiers.", call. = FALSE)
  }
  storage.mode(M) <- "numeric"

  # Accept -1/0/1 as well as 0/1/2.
  if (min(M, na.rm = TRUE) < -0.5) M <- M + 1

  # Mean-impute missing calls, then drop uninformative markers.
  if (anyNA(M)) {
    means <- colMeans(M, na.rm = TRUE)
    idx <- which(is.na(M), arr.ind = TRUE)
    M[idx] <- means[idx[, 2]]
  }
  p <- colMeans(M, na.rm = TRUE) / 2
  maf <- pmin(p, 1 - p)
  keep <- is.finite(maf) & maf >= min_maf
  if (sum(keep) < 10L) {
    stop("Only ", sum(keep), " informative marker(s) remain after filtering on ",
         "minor allele frequency. Check the marker coding (0/1/2 or -1/0/1).",
         call. = FALSE)
  }
  M <- M[, keep, drop = FALSE]
  p <- p[keep]

  Z <- sweep(M, 2, 2 * p, "-")
  denom <- 2 * sum(p * (1 - p))
  if (!is.finite(denom) || denom <= 0) {
    stop("The marker matrix carries no usable allele-frequency variation.", call. = FALSE)
  }
  G <- tcrossprod(Z) / denom
  G <- (G + t(G)) / 2

  blend <- max(0, min(0.5, blend))
  if (blend > 0) G <- (1 - blend) * G + blend * diag(nrow(G))
  dimnames(G) <- list(rownames(M), rownames(M))
  attr(G, "n_markers") <- ncol(M)
  attr(G, "blend") <- blend
  G
}

#' Read a square relationship matrix whose first column holds the identifiers.
#' @noRd
read_square_matrix <- function(raw) {
  ids <- trimws(as.character(raw[[1]]))
  body <- raw[, -1, drop = FALSE]
  num <- vapply(body, function(x) is.numeric(x) || !anyNA(suppressWarnings(as.numeric(x))),
                logical(1))
  if (!all(num)) {
    stop("Every column after the first must be numeric. The first column should ",
         "hold the genotype identifiers.", call. = FALSE)
  }
  K <- as.matrix(as.data.frame(lapply(body, function(x) as.numeric(as.character(x)))))
  if (nrow(K) != ncol(K)) {
    stop("The relationship matrix is ", nrow(K), " x ", ncol(K),
         "; it must be square with one row and one column per genotype.", call. = FALSE)
  }
  if (anyNA(K)) stop("The relationship matrix contains missing values.", call. = FALSE)
  dimnames(K) <- list(ids, ids)
  if (max(abs(K - t(K))) > 1e-6) {
    stop("The relationship matrix is not symmetric.", call. = FALSE)
  }
  K
}

# ---------------------------------------------------------------------------
# Assembly
# ---------------------------------------------------------------------------

#' Build a relationship object from whichever source the user supplied.
#'
#' @param type one of RELATIONSHIP_SOURCES
#' @param raw uploaded data frame (pedigree, matrix or markers)
#' @param map column mapping for a pedigree
#' @param blend identity blending weight for genomic matrices
#' @return list(ginv, ids, type, label, diagnostics, matrix)
#' @export
build_relationship <- function(type, raw = NULL, map = NULL, blend = 0.01) {
  if (identical(type, "none") || is.null(raw)) return(NULL)

  if (type == "pedigree") {
    ginv <- build_pedigree_ginv(raw, map)
    label <- "Pedigree numerator relationship matrix (A)"

  } else if (type == "kinship") {
    K <- read_square_matrix(raw)
    ev <- eigen(K, symmetric = TRUE, only.values = TRUE)$values
    if (min(ev) <= 1e-8) {
      # Blend rather than refuse: a kinship matrix estimated from markers is
      # routinely singular and the user's intent is unambiguous.
      K <- (1 - blend) * K + blend * diag(nrow(K))
      attr(K, "blend") <- blend
    }
    ginv <- kinship_to_ginv(K)
    label <- "Supplied relationship matrix"

  } else if (type == "markers") {
    G <- markers_to_grm(as.matrix_with_rownames(raw), blend = blend)
    ginv <- kinship_to_ginv(G)
    label <- sprintf("Genomic relationship matrix (VanRaden) from %d markers",
                     attr(G, "n_markers"))
  } else {
    stop("Unknown relationship source: ", type, call. = FALSE)
  }

  ids <- attr(ginv, "rowNames")
  list(
    ginv = ginv,
    ids = ids,
    type = type,
    label = label,
    matrix = ginv_to_matrix(ginv),
    diagnostics = relationship_diagnostics(ginv, type)
  )
}

#' First column becomes row names; the rest becomes a numeric matrix.
#' @noRd
as.matrix_with_rownames <- function(raw) {
  ids <- trimws(as.character(raw[[1]]))
  M <- as.matrix(as.data.frame(lapply(raw[, -1, drop = FALSE],
                                      function(x) suppressWarnings(as.numeric(as.character(x))))))
  rownames(M) <- ids
  M
}

#' Summary statistics about a relationship matrix, for the interface.
#' @noRd
relationship_diagnostics <- function(ginv, type) {
  ids <- attr(ginv, "rowNames")
  inb <- attr(ginv, "inbreeding")
  ped <- attr(ginv, "pedigree")
  list(
    n = length(ids),
    n_founders = attr(ginv, "n_founders") %||%
      (if (!is.null(ped)) sum(is.na(ped$Sire) & is.na(ped$Dam)) else NA_integer_),
    mean_inbreeding = if (!is.null(inb)) mean(inb, na.rm = TRUE) else NA_real_,
    max_inbreeding = if (!is.null(inb)) max(inb, na.rm = TRUE) else NA_real_,
    n_inbred = if (!is.null(inb)) sum(inb > 1e-8, na.rm = TRUE) else NA_integer_,
    type = type
  )
}

#' Check coverage and re-level the genetic factors onto the relationship ids.
#'
#' Every genotype with an observed plot must appear in the relationship matrix;
#' ASReml would otherwise fail with an opaque message. Individuals present in
#' the relationship matrix but not in the trial are kept: their effects are
#' predicted from their relatives, which is one of the main reasons to fit a
#' pedigree at all.
#'
#' @param d trial data carrying Geno and the neighbour factors
#' @param neighbour_names N1..Nk
#' @param rel relationship object from `build_relationship()`
#' @noRd
align_relationship <- function(d, neighbour_names, rel) {
  if (is.null(rel)) return(list(data = d, coverage = NULL))

  observed <- unique(as.character(d$Geno[!is.na(d$Yield) & !is.na(d$Geno)]))
  missing <- setdiff(observed, rel$ids)
  if (length(missing)) {
    stop(length(missing), " genotype(s) with observed plots are absent from the ",
         "relationship matrix, for example: ",
         paste(utils::head(sort(missing), 5), collapse = ", "),
         ". Add them to the pedigree or marker file, or turn the relationship ",
         "matrix off.", call. = FALSE)
  }

  for (v in c("Geno", neighbour_names)) {
    d[[v]] <- factor(as.character(d[[v]]), levels = rel$ids)
  }

  list(
    data = d,
    coverage = list(
      n_ids = length(rel$ids),
      n_in_trial = length(observed),
      n_extra = length(rel$ids) - length(observed)
    )
  )
}

#' Heatmap of the relationship matrix.
#' @noRd
plot_relationship <- function(rel, base_size = 12, caption = NULL, max_n = 80L) {
  K <- rel$matrix
  if (is.null(K)) {
    stop("The relationship matrix is too large to display as a heatmap.")
  }
  if (nrow(K) > max_n) {
    # Show the most connected individuals rather than an unreadable full grid.
    keep <- order(rowSums(abs(K)), decreasing = TRUE)[seq_len(max_n)]
    keep <- sort(keep)
    K <- K[keep, keep, drop = FALSE]
    caption <- paste(c(sprintf("Showing the %d most connected individuals of %d.",
                               max_n, nrow(rel$matrix)), caption), collapse = " ")
  }
  long <- matrix_to_long(K, "Relationship")
  long$Row <- factor(long$Row, levels = rownames(K))
  long$Column <- factor(long$Column, levels = rownames(K))

  ggplot2::ggplot(long, ggplot2::aes(.data$Column, .data$Row,
                                     fill = .data$Relationship)) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_gradientn(colours = SEQUENTIAL, name = "Relationship") +
    ggplot2::scale_y_discrete(limits = rev(rownames(K))) +
    ggplot2::coord_equal(expand = FALSE) +
    ggplot2::labs(
      title = "Relationship matrix",
      subtitle = wrap_subtitle(paste(
        "Diagonal is 1 + inbreeding; off-diagonal values are twice the kinship",
        "between two individuals")),
      x = NULL, y = NULL, caption = wrap_caption(caption)) +
    theme_trial(base_size, grid = "none") +
    ggplot2::theme(
      axis.text = ggplot2::element_text(size = base_size * 0.5),
      axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5),
      legend.position = "right",
      legend.key.height = grid::unit(1.6, "lines"),
      legend.key.width = grid::unit(0.55, "lines"))
}

#' One-row-per-individual record of the relationship matrix, for the workbook.
#'
#' Returns NULL when no relationship was used, so `save_results_workbook()`
#' simply omits the sheet.
#' @noRd
relationship_export <- function(result) {
  rel <- result$relationship
  if (is.null(rel)) return(NULL)
  inb <- attr(rel$ginv, "inbreeding")
  ped <- attr(rel$ginv, "pedigree")
  out <- data.frame(
    Individual = rel$ids,
    Inbreeding = if (!is.null(inb)) as.numeric(inb[rel$ids]) else NA_real_,
    stringsAsFactors = FALSE
  )
  if (!is.null(ped)) {
    out$Male_parent <- ped$Sire[match(out$Individual, ped$ID)]
    out$Female_parent <- ped$Dam[match(out$Individual, ped$ID)]
  }
  out$Source <- rel$label
  out
}
