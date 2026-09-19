# ---------------------------------------------------------------------------
# Publication-quality figures
#
# Every figure is a ggplot object, which keeps three things consistent:
# the on-screen rendering, the interactive plotly version, and the exported
# file. `base_size` is the single knob that scales all text, so a figure
# exported at 300-600 dpi keeps the same visual proportions as the screen
# version instead of shrinking its labels into illegibility.
# ---------------------------------------------------------------------------

#' Caption giving the fitted model, so exported figures are self-documenting.
#' @noRd
model_caption <- function(result, extra = NULL) {
  paste(c(paste0("Model: ", result$description), extra), collapse = " | ")
}

#' Wrap long captions so they do not run off the canvas.
#' @noRd
wrap_caption <- function(x, width = 110) {
  if (is.null(x) || !nzchar(x)) return(NULL)
  paste(strwrap(x, width = width), collapse = "\n")
}

#' Wrap a subtitle.
#'
#' Subtitles are set larger than captions, so they need a narrower measure or
#' they run past the right edge of the canvas.
#' @noRd
wrap_subtitle <- function(x, width = 88) wrap_caption(x, width)

# ---------------------------------------------------------------------------
# Field layout
# ---------------------------------------------------------------------------

#' Field-plan heatmap of an observed or fitted quantity.
#'
#' The single most useful diagnostic for a spatially analysed trial: it shows
#' the physical field, so a fertility gradient, a headland effect or a
#' mis-entered coordinate is visible immediately.
#'
#' @param d data frame with Row, Column and the value column
#' @param value name of the column to map
#' @param title,subtitle figure text
#' @param diverging use the diverging palette (residuals) or sequential (yield)
#' @param facet facet by environment when TRUE
#' @noRd
plot_field_map <- function(d, value = "Observed", title = NULL, subtitle = NULL,
                           diverging = FALSE, facet = FALSE, base_size = 12,
                           caption = NULL, fill_label = NULL) {
  d <- d[stats::complete.cases(d[c("Row", "Column")]), , drop = FALSE]
  limit <- max(abs(d[[value]]), na.rm = TRUE)

  p <- ggplot2::ggplot(d, ggplot2::aes(x = .data$Column, y = .data$Row,
                                       fill = .data[[value]])) +
    ggplot2::geom_tile(colour = "white", linewidth = 0.25) +
    # Environments differ in size, so faceted field plans need free scales.
    # ggplot2 4.x rejects free scales alongside a fixed coordinate ratio, so
    # the square-plot aspect is only imposed on a single, unfaceted field.
    (if (facet) ggplot2::coord_cartesian(expand = FALSE)
     else ggplot2::coord_equal(expand = FALSE)) +
    # Faceted panels are small, so they take fewer axis labels.
    ggplot2::scale_x_continuous(breaks = scales::breaks_pretty(n = if (facet) 4 else 6)) +
    ggplot2::scale_y_reverse(breaks = scales::breaks_pretty(n = if (facet) 4 else 6)) +
    ggplot2::labs(title = title, subtitle = wrap_subtitle(subtitle), x = "Field column",
                  y = "Field row", fill = fill_label %||% value,
                  caption = wrap_caption(caption)) +
    theme_trial(base_size, grid = "none") +
    ggplot2::theme(legend.position = "right",
                   legend.key.height = grid::unit(1.6, "lines"),
                   legend.key.width = grid::unit(0.55, "lines"))

  p <- p + if (diverging) {
    ggplot2::scale_fill_gradientn(colours = DIVERGING, limits = c(-limit, limit),
                                  na.value = "#E9EDEA")
  } else {
    ggplot2::scale_fill_gradientn(colours = SEQUENTIAL, na.value = "#E9EDEA")
  }
  if (facet) p <- p + ggplot2::facet_wrap(~ Env, scales = "free")
  p
}

# ---------------------------------------------------------------------------
# Genotype-level figures
# ---------------------------------------------------------------------------

#' Direct effect against competitive effect.
#'
#' The central figure of a competition analysis. The quadrants separate the
#' genotypes a breeder cares about: high direct effect with a benign
#' competitive effect is an unambiguously good selection, whereas high direct
#' effect with an aggressive competitive effect means part of the apparent
#' advantage was taken from its neighbours and will not carry into a pure
#' stand or a farmer's field.
#' @noRd
plot_direct_vs_competition <- function(genetic, k = 2, label_n = 12,
                                       base_size = 12, caption = NULL) {
  d <- genetic[stats::complete.cases(genetic[c("Direct_effect", "Competition_effect")]), ]
  if (!nrow(d)) stop("No genotypes have both a direct and a competitive effect.")

  d$Pure <- d$Direct_effect + k * d$Competition_effect
  top <- utils::head(d[order(-d$Pure), ], label_n)

  p <- ggplot2::ggplot(d, ggplot2::aes(x = .data$Direct_effect,
                                       y = .data$Competition_effect)) +
    ggplot2::geom_hline(yintercept = 0, colour = PAL$muted, linetype = "dashed",
                        linewidth = 0.4) +
    ggplot2::geom_vline(xintercept = 0, colour = PAL$muted, linetype = "dashed",
                        linewidth = 0.4) +
    # Contours of constant pure-stand value: genotypes on the same line have
    # the same expected monoculture performance.
    ggplot2::geom_abline(slope = -1 / k,
                         intercept = stats::quantile(d$Pure, c(0.1, 0.5, 0.9),
                                                     na.rm = TRUE) / k,
                         colour = PAL$pure, linetype = "dotted", linewidth = 0.4) +
    ggplot2::geom_point(ggplot2::aes(size = .data$Pure, colour = .data$Pure),
                        alpha = 0.85) +
    ggplot2::scale_colour_gradientn(colours = DIVERGING, name = "Pure-stand effect") +
    ggplot2::scale_size_continuous(range = c(1.6, 5), guide = "none") +
    ggplot2::labs(
      title = "Direct versus competitive genetic effects",
      subtitle = wrap_subtitle(sprintf(paste(
        "Horizontal axis: performance in the genotype's own plot.",
        "Vertical axis: its effect on neighbouring plots.",
        "Dotted lines join genotypes of equal pure-stand value (direct + %d x competitive)."), k)),
      x = "Direct effect",
      y = "Competitive effect",
      caption = wrap_caption(caption)) +
    theme_trial(base_size)

  if (has_pkg("ggrepel") && nrow(top)) {
    p <- p + ggrepel::geom_text_repel(
      data = top, ggplot2::aes(label = .data$Genotype),
      size = base_size * 0.24, colour = PAL$ink, max.overlaps = 20,
      min.segment.length = 0.2, segment.colour = PAL$muted, segment.size = 0.25)
  }
  p
}

#' Ranked pure-stand (or direct) effects with exact confidence intervals.
#'
#' Error bars use the prediction error variance of the plotted combination, so
#' the interval around a pure-stand value is correct rather than the far too
#' wide one obtained by adding the direct and competitive standard errors.
#' @noRd
plot_ranking <- function(genetic,
                         effect = c("Predicted_pure_stand_yield",
                                    "Pure_stand_effect", "Direct_effect"),
                         se_col = NULL, top_n = 30, base_size = 12,
                         caption = NULL, conf = 0.95) {
  effect <- match.arg(effect)
  # Predicted pure-stand yield is the default: it is what selection acts on,
  # and it carries the same standard error as the effect it is built from,
  # the trial mean being a common constant.
  if (identical(effect, "Predicted_pure_stand_yield") &&
      !effect %in% names(genetic)) {
    effect <- "Pure_stand_effect"
  }
  if (is.null(se_col)) {
    se_col <- c(Predicted_pure_stand_yield = "SE_pure_stand",
                Pure_stand_effect = "SE_pure_stand",
                Direct_effect = "SE_direct")[[effect]]
  }
  d <- genetic[!is.na(genetic[[effect]]), , drop = FALSE]
  d <- utils::head(d[order(-d[[effect]]), ], top_n)
  d$Genotype <- factor(d$Genotype, levels = rev(d$Genotype))

  has_se <- !is.null(se_col) && se_col %in% names(d) && any(is.finite(d[[se_col]]))
  z <- stats::qnorm(1 - (1 - conf) / 2)

  label <- switch(effect,
                  Predicted_pure_stand_yield = "Predicted pure-stand performance",
                  Pure_stand_effect = "Pure-stand genetic effect",
                  Direct_effect = "Direct genetic effect")

  p <- ggplot2::ggplot(d, ggplot2::aes(y = .data$Genotype, x = .data[[effect]]))
  if (has_se) {
    # geom_linerange rather than the geom_errorbarh deprecated in ggplot2 4.0.
    p <- p + ggplot2::geom_linerange(
      ggplot2::aes(xmin = .data[[effect]] - z * .data[[se_col]],
                   xmax = .data[[effect]] + z * .data[[se_col]]),
      colour = PAL$muted, linewidth = 0.5)
  }
  p <- p +
    ggplot2::geom_vline(
      xintercept = if (identical(effect, "Predicted_pure_stand_yield")) {
        mean(genetic[[effect]], na.rm = TRUE)
      } else 0,
      colour = PAL$muted, linetype = "dashed", linewidth = 0.4) +
    ggplot2::geom_point(ggplot2::aes(colour = .data[[effect]]), size = 2.4) +
    ggplot2::scale_colour_gradientn(colours = DIVERGING, guide = "none") +
    ggplot2::labs(
      title = sprintf("Top %d genotypes by %s", nrow(d), tolower(label)),
      subtitle = wrap_subtitle(if (has_se) {
        sprintf("Points are BLUPs; bars are %.0f%% intervals from the exact prediction error variance", conf * 100)
      } else "Points are BLUPs; no exact prediction error variance was available"),
      x = label, y = NULL, caption = wrap_caption(caption)) +
    theme_trial(base_size, grid = "y")
  p
}

#' How much does accounting for competition change the selection decision?
#'
#' A slope chart between the direct-effect ranking and the pure-stand ranking.
#' Large crossings are the practical payoff of the competition model: those
#' genotypes would have been mis-selected on direct effects alone.
#' @noRd
plot_rank_change <- function(genetic, top_n = 25, base_size = 12, caption = NULL) {
  d <- genetic[stats::complete.cases(genetic[c("Rank_direct", "Rank_pure_stand")]), ]
  d <- utils::head(d[order(d$Rank_pure_stand), ], top_n)
  long <- rbind(
    data.frame(Genotype = d$Genotype, Stage = "Direct effect", Rank = d$Rank_direct,
               Change = d$Rank_change, stringsAsFactors = FALSE),
    data.frame(Genotype = d$Genotype, Stage = "Pure-stand value", Rank = d$Rank_pure_stand,
               Change = d$Rank_change, stringsAsFactors = FALSE)
  )
  long$Stage <- factor(long$Stage, levels = c("Direct effect", "Pure-stand value"))

  ggplot2::ggplot(long, ggplot2::aes(x = .data$Stage, y = .data$Rank,
                                     group = .data$Genotype)) +
    ggplot2::geom_line(ggplot2::aes(colour = .data$Change), linewidth = 0.7,
                       alpha = 0.85) +
    ggplot2::geom_point(ggplot2::aes(colour = .data$Change), size = 2) +
    ggplot2::geom_text(
      data = long[long$Stage == "Pure-stand value", ],
      ggplot2::aes(label = .data$Genotype), hjust = -0.18,
      size = base_size * 0.22, colour = PAL$body) +
    ggplot2::scale_y_reverse(breaks = scales::breaks_pretty()) +
    ggplot2::scale_colour_gradient2(low = PAL$danger, mid = PAL$muted,
                                    high = PAL$primary, midpoint = 0,
                                    name = "Places gained") +
    ggplot2::scale_x_discrete(expand = ggplot2::expansion(mult = c(0.08, 0.30))) +
    ggplot2::labs(
      title = "Selection decisions change once competition is modelled",
      subtitle = wrap_subtitle("Rank on the direct effect alone compared with rank on the pure-stand value"),
      x = NULL, y = "Rank (1 = best)", caption = wrap_caption(caption)) +
    theme_trial(base_size, grid = "y")
}

#' Variance components as a share of the total.
#' @noRd
plot_variance_components <- function(varcomp, base_size = 12, caption = NULL) {
  d <- varcomp[is.finite(varcomp$Pct_of_total) & varcomp$Pct_of_total > 0, , drop = FALSE]
  if (!nrow(d)) stop("No positive variance components to plot.")
  d <- d[order(d$Pct_of_total), ]
  d$Component <- factor(d$Component, levels = d$Component)

  ggplot2::ggplot(d, ggplot2::aes(y = .data$Component, x = .data$Pct_of_total)) +
    ggplot2::geom_col(fill = PAL$primary, width = 0.7) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.1f%%", .data$Pct_of_total)),
                       hjust = -0.15, size = base_size * 0.24, colour = PAL$body) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, 0.16))) +
    ggplot2::labs(title = "Contribution of each variance component",
                  subtitle = "Percentage of the total estimated variance",
                  x = "Share of total variance (%)", y = NULL,
                  caption = wrap_caption(caption)) +
    theme_trial(base_size, grid = "y")
}

# ---------------------------------------------------------------------------
# Diagnostics
# ---------------------------------------------------------------------------

#' Residual diagnostics: fitted values, normal quantiles and distribution.
#' @noRd
plot_residual_diagnostics <- function(res, base_size = 12, caption = NULL) {
  res <- res[is.finite(res$Residual) & is.finite(res$Fitted), , drop = FALSE]
  if (!nrow(res)) stop("No residuals are available for diagnostics.")

  # Titles align to the panel, not the whole plot: with plot-aligned titles the
  # left-hand panel's title runs under the right-hand panel in a 2 x 2 grid.
  panel_title <- ggplot2::theme(plot.title.position = "panel",
                                plot.title = ggplot2::element_text(
                                  size = base_size * 1.02, face = "bold"))

  p1 <- ggplot2::ggplot(res, ggplot2::aes(.data$Fitted, .data$Std_residual)) +
    ggplot2::geom_hline(yintercept = 0, colour = PAL$muted, linetype = "dashed") +
    ggplot2::geom_hline(yintercept = c(-3, 3), colour = PAL$danger,
                        linetype = "dotted", linewidth = 0.4) +
    ggplot2::geom_point(colour = PAL$primary, alpha = 0.55, size = 1.5) +
    ggplot2::geom_smooth(method = "loess", formula = y ~ x, se = FALSE,
                         colour = PAL$accent, linewidth = 0.7) +
    ggplot2::labs(title = "Residuals vs fitted",
                  x = "Fitted value", y = "Standardised residual") +
    theme_trial(base_size) + panel_title

  q <- data.frame(sample = sort(res$Std_residual))
  q$theoretical <- theoretical_quantiles(nrow(q))
  p2 <- ggplot2::ggplot(q, ggplot2::aes(.data$theoretical, .data$sample)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, colour = PAL$muted,
                         linetype = "dashed") +
    ggplot2::geom_point(colour = PAL$primary, alpha = 0.55, size = 1.5) +
    ggplot2::labs(title = "Normal quantile-quantile",
                  x = "Theoretical quantile", y = "Standardised residual") +
    theme_trial(base_size) + panel_title

  p3 <- ggplot2::ggplot(res, ggplot2::aes(.data$Std_residual)) +
    ggplot2::geom_histogram(bins = 30, fill = PAL$primary, colour = "white",
                            linewidth = 0.2) +
    ggplot2::labs(title = "Residual distribution",
                  x = "Standardised residual", y = "Plots") +
    theme_trial(base_size) + panel_title

  p4 <- plot_field_map(res, "Std_residual",
                       title = "Residuals on the field plan",
                       diverging = TRUE, facet = length(unique(res$Env)) > 1,
                       base_size = base_size, fill_label = "Std.
residual") +
    panel_title

  guidance <- paste(
    "Read together: a trend or funnel against fitted values, or curvature in",
    "the quantile-quantile plot, indicates unmodelled structure; patches of one",
    "colour on the field plan indicate spatial trend the model has not removed."
  )
  if (has_pkg("patchwork")) {
    patchwork::wrap_plots(p1, p2, p3, p4, ncol = 2) +
      patchwork::plot_annotation(
        title = "Residual diagnostics",
        caption = wrap_caption(paste(guidance, caption, sep = " ")),
        theme = theme_trial(base_size))
  } else {
    p1 + ggplot2::labs(caption = wrap_caption(paste(guidance, caption)))
  }
}

#' Sample variogram of the residuals.
#'
#' For a separable AR1 x AR1 model the sample variogram should rise smoothly to
#' a plateau. Ridges along a displacement axis, or a surface that keeps rising,
#' indicate trend the spatial model has not absorbed (Gilmour, Cullis &
#' Verbyla 1997).
#' @noRd
plot_variogram <- function(v, base_size = 12, caption = NULL) {
  if (is.null(v) || !nrow(v)) stop("ASReml did not return a sample variogram.")
  ggplot2::ggplot(v, ggplot2::aes(x = .data$Column, y = .data$Row,
                                  fill = .data$Semivariance)) +
    ggplot2::geom_tile(colour = "white", linewidth = 0.25) +
    ggplot2::scale_fill_gradientn(colours = SEQUENTIAL, name = "Semivariance") +
    ggplot2::coord_equal(expand = FALSE) +
    ggplot2::labs(
      title = "Sample variogram of the residuals",
      subtitle = wrap_subtitle("Should rise smoothly to a plateau; ridges or a continuing rise indicate unmodelled field trend"),
      x = "Column displacement", y = "Row displacement",
      caption = wrap_caption(caption)) +
    theme_trial(base_size, grid = "none") +
    ggplot2::theme(legend.position = "right",
                   legend.key.height = grid::unit(1.6, "lines"))
}

# ---------------------------------------------------------------------------
# MET figures
# ---------------------------------------------------------------------------

#' Genetic-correlation heatmap between environments.
#' @noRd
plot_correlation_heatmap <- function(m, title, subtitle = NULL, base_size = 12,
                                     caption = NULL, show_values = TRUE) {
  m <- as.matrix(m)
  if (!nrow(m) || nrow(m) != ncol(m)) stop("A square correlation matrix is required.")
  # The diagonal is 1 by definition and carries no information; showing it only
  # anchors the colour scale on a value that is never in question.
  diag(m) <- NA_real_
  long <- matrix_to_long(m, "Correlation")
  long$Row <- factor(long$Row, levels = rownames(m))
  long$Column <- factor(long$Column, levels = rownames(m))

  p <- ggplot2::ggplot(long, ggplot2::aes(.data$Column, .data$Row,
                                          fill = .data$Correlation)) +
    ggplot2::geom_tile(colour = "white", linewidth = 0.6) +
    ggplot2::scale_fill_gradientn(colours = DIVERGING, limits = c(-1, 1),
                                  na.value = PAL$surface,
                                  name = "Genetic correlation",
                                  breaks = seq(-1, 1, 0.5)) +
    ggplot2::scale_y_discrete(limits = rev(rownames(m))) +
    ggplot2::coord_equal(expand = FALSE) +
    ggplot2::labs(title = title, subtitle = wrap_subtitle(subtitle), x = NULL, y = NULL,
                  caption = wrap_caption(caption)) +
    theme_trial(base_size, grid = "none") +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      legend.position = "right",
      legend.key.height = grid::unit(1.8, "lines"),
      legend.key.width = grid::unit(0.55, "lines"))

  if (show_values && nrow(m) <= 16) {
    labelled <- long[!is.na(long$Correlation), , drop = FALSE]
    p <- p + ggplot2::geom_text(
      data = labelled,
      ggplot2::aes(label = sprintf("%.2f", .data$Correlation),
                   colour = abs(.data$Correlation) > 0.6),
      size = base_size * 0.23, fontface = "bold", show.legend = FALSE) +
      ggplot2::scale_colour_manual(values = c(`TRUE` = "white", `FALSE` = PAL$ink))
  }
  p
}

#' Direct, competitive and pure-stand genetic variance in each environment.
#' @noRd
plot_environment_variances <- function(v, base_size = 12, caption = NULL) {
  long <- rbind(
    data.frame(Environment = v$Environment, Effect = "Direct",
               Variance = v$Direct_variance),
    data.frame(Environment = v$Environment, Effect = "Competition",
               Variance = v$Competition_variance),
    data.frame(Environment = v$Environment, Effect = "Pure stand",
               Variance = v$Pure_stand_variance)
  )
  long$Effect <- factor(long$Effect, levels = c("Direct", "Competition", "Pure stand"))

  ggplot2::ggplot(long, ggplot2::aes(x = .data$Environment, y = .data$Variance,
                                     fill = .data$Effect)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.78), width = 0.72) +
    ggplot2::scale_fill_manual(values = effect_colours(), name = NULL) +
    ggplot2::labs(
      title = "Genetic variance by environment",
      subtitle = wrap_subtitle("Pure-stand variance combines the direct and competitive variances with their covariance"),
      x = NULL, y = "Genetic variance", caption = wrap_caption(caption)) +
    theme_trial(base_size, grid = "y") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))
}

#' Stability of pure-stand performance across environments.
#'
#' A parallel-coordinates view of the selection candidates. Lines that stay
#' parallel indicate stable genotypes; lines that cross indicate crossover
#' genotype-by-environment interaction and therefore environment-specific
#' recommendations.
#' @noRd
plot_stability <- function(values, top_n = 12, base_size = 12, caption = NULL) {
  d <- values[values$Status == "Estimable", , drop = FALSE]
  mean_effect <- tapply(d$Pure_stand_effect, d$Genotype, mean, na.rm = TRUE)
  keep <- names(sort(mean_effect, decreasing = TRUE))[seq_len(min(top_n, length(mean_effect)))]
  d <- d[d$Genotype %in% keep, , drop = FALSE]

  p <- ggplot2::ggplot(d, ggplot2::aes(x = .data$Environment, y = .data$Pure_stand_effect,
                                       group = .data$Genotype,
                                       colour = .data$Genotype)) +
    ggplot2::geom_hline(yintercept = 0, colour = PAL$muted, linetype = "dashed",
                        linewidth = 0.4) +
    ggplot2::geom_line(linewidth = 0.7, alpha = 0.85) +
    ggplot2::geom_point(size = 1.9) +
    ggplot2::labs(
      title = sprintf("Pure-stand performance of the top %d genotypes", length(keep)),
      subtitle = wrap_subtitle("Crossing lines indicate crossover genotype-by-environment interaction"),
      x = NULL, y = "Pure-stand genetic effect", colour = NULL,
      caption = wrap_caption(caption)) +
    theme_trial(base_size, grid = "y") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1),
                   legend.position = "right",
                   legend.text = ggplot2::element_text(size = base_size * 0.7))
  if (has_pkg("viridisLite")) {
    p <- p + ggplot2::scale_colour_manual(
      values = viridisLite::viridis(length(keep), end = 0.9))
  }
  p
}

#' Percentage of genetic variance explained by the factor-analytic factors.
#' @noRd
plot_fa_summary <- function(fa, base_size = 12, caption = NULL) {
  fa$Environment <- factor(fa$Environment, levels = unique(fa$Environment))
  ggplot2::ggplot(fa, ggplot2::aes(x = .data$Environment,
                                   y = .data$Variance_explained_pct,
                                   fill = .data$Effect)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.78), width = 0.72) +
    ggplot2::geom_hline(yintercept = 75, colour = PAL$danger, linetype = "dotted") +
    ggplot2::scale_fill_manual(values = effect_colours(), name = NULL) +
    ggplot2::scale_y_continuous(limits = c(0, 100)) +
    ggplot2::labs(
      title = "Genetic variance explained by the factor-analytic factors",
      subtitle = wrap_subtitle("Environments below the dotted line behave idiosyncratically and pool poorly with the rest"),
      x = NULL, y = "Variance explained (%)", caption = wrap_caption(caption)) +
    theme_trial(base_size, grid = "y") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))
}

#' Direct against competitive effects, one panel per environment.
#' @noRd
plot_met_scatter <- function(values, k = 2, base_size = 12, caption = NULL) {
  d <- values[values$Status == "Estimable", , drop = FALSE]
  ggplot2::ggplot(d, ggplot2::aes(.data$Direct_effect, .data$Competition_effect)) +
    ggplot2::geom_hline(yintercept = 0, colour = PAL$muted, linetype = "dashed",
                        linewidth = 0.35) +
    ggplot2::geom_vline(xintercept = 0, colour = PAL$muted, linetype = "dashed",
                        linewidth = 0.35) +
    ggplot2::geom_point(ggplot2::aes(colour = .data$Pure_stand_effect), alpha = 0.8,
                        size = 1.7) +
    ggplot2::scale_colour_gradientn(colours = DIVERGING, name = "Pure-stand effect") +
    ggplot2::facet_wrap(~ Environment) +
    ggplot2::labs(
      title = "Direct and competitive effects within each environment",
      subtitle = wrap_subtitle(sprintf("Pure-stand value is direct + %d x competitive", k)),
      x = "Direct effect", y = "Competitive effect",
      caption = wrap_caption(caption)) +
    theme_trial(base_size)
}
