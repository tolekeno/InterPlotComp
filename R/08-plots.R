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
#'
#' The measure follows the type size, because the type size follows the canvas
#' width (see `figure_base_size()`). A fixed character count wraps correctly at
#' one width only: at 110 characters a caption set for an 18 cm figure runs
#' straight off a 9 cm single-column one. The references below are measured
#' from the rendered output at the 14 pt base size on an 18 cm canvas: a
#' caption at 0.82 of the base fits about 92 characters, a subtitle at 0.92
#' about 76. Because the base size itself grows as the square root of the
#' width, characters-per-line works out proportional to the base size.
#'
#' @param x text to wrap, or NULL
#' @param base_size the figure's base type size
#' @param ref characters that fit at the 14 pt reference size
#' @noRd
wrap_caption <- function(x, base_size = 14, ref = 92) {
  if (is.null(x) || !nzchar(x)) return(NULL)
  width <- max(28L, as.integer(round(ref * base_size / 14)))
  paste(strwrap(x, width = width), collapse = "\n")
}

#' Wrap a subtitle.
#'
#' Subtitles are set larger than captions, so they need a narrower measure or
#' they run past the right edge of the canvas.
#' @noRd
wrap_subtitle <- function(x, base_size = 14) wrap_caption(x, base_size, ref = 76)

# ---------------------------------------------------------------------------
# Field layout
# ---------------------------------------------------------------------------

#' Field-plan heatmap of an observed or fitted quantity.
#'
#' The single most useful diagnostic for a spatially analysed trial: it shows
#' the physical field, so a fertility gradient, a headland effect or a
#' mis-entered coordinate is visible immediately.
#'
#' @param d A data frame with `Row`, `Column` and the value column. The
#'   coordinates are positions on the field, so they are mapped on continuous
#'   axes; the prepared trial data holds them as factors in `Row`/`Column` and
#'   as integers in `Row_i`/`Col_i`, and either form is accepted here.
#' @param value Name of the column to map.
#' @param title,subtitle Figure text.
#' @param diverging Use the diverging palette (for residuals and other signed
#'   quantities) rather than the sequential one (for yields).
#' @param facet Facet by environment, for a multi-environment trial.
#' @param fill_label Legend title; defaults to `value`.
#' @param base_size Base font size in points.
#' @param caption Optional caption placed under the figure.
#' @return A [ggplot2::ggplot()] object.
#' @export
#' @examples
#' d <- prepare_trial_data(
#'   sample_single_trial(),
#'   list(yield = "Yield_t_ha", geno = "Genotype", row = "Row", column = "Column")
#' )
#' plot_field_map(
#'   data.frame(Row = d$Row_i, Column = d$Col_i, Observed = d$Yield),
#'   value = "Observed", title = "Observed yield on the field plan"
#' )
plot_field_map <- function(d, value = "Observed", title = NULL, subtitle = NULL,
                           diverging = FALSE, facet = FALSE, base_size = 12,
                           caption = NULL, fill_label = NULL) {
  if (!value %in% names(d)) {
    stop("The field map has no '", value, "' column to map.", call. = FALSE)
  }
  # A factor coordinate is a discrete value on a continuous scale, which fails
  # only at draw time with ggplot2's generic "Discrete value supplied to a
  # continuous scale". Converting through the level *label* keeps the field
  # position, whereas as.integer() on a factor would return the level code.
  for (axis in c("Row", "Column")) {
    if (is.factor(d[[axis]])) d[[axis]] <- as.numeric(as.character(d[[axis]]))
  }
  d <- d[stats::complete.cases(d[c("Row", "Column")]), , drop = FALSE]
  if (!nrow(d)) stop("No plots have both a field row and a field column.",
                     call. = FALSE)
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
    ggplot2::labs(title = title, subtitle = wrap_subtitle(subtitle, base_size), x = "Field column",
                  y = "Field row", fill = fill_label %||% value,
                  caption = wrap_caption(caption, base_size)) +
    theme_trial(base_size, grid = "none") +
    ggplot2::theme(legend.position = "right",
                   legend.key.height = grid::unit(1.6, "lines"),
                   legend.key.width = grid::unit(0.55, "lines"),
                   # The guide title sits above its key. Against a right-hand
                   # bar that puts it level with the plot title, which they
                   # then collide with on a narrow panel.
                   legend.box.margin = ggplot2::margin(t = 8))

  p <- p + if (diverging) {
    ggplot2::scale_fill_gradientn(colours = DIVERGING, limits = c(-limit, limit),
                                  na.value = PAL$line_soft)
  } else {
    ggplot2::scale_fill_gradientn(colours = SEQUENTIAL, na.value = PAL$line_soft)
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
#' @param genetic The genotype table from `fit_single_model()$genetic`, or any
#'   data frame with the same columns.
#' @param k Number of neighbours per plot, from `fit_single_model()$k`. It sets
#'   the weight on the competitive effect in the pure-stand value.
#' @param label_n Number of top genotypes to label.
#' @param base_size Base font size in points. [save_figure()] chooses this from
#'   the export width; pass it explicitly only when composing a figure by hand.
#' @param caption Optional caption placed under the figure, wrapped to the
#'   plot width.
#' @return A [ggplot2::ggplot()] object.
#' @seealso [plot_rank_change()] for the consequence of this figure for
#'   selection.
#' @export
#' @examples
#' g <- data.frame(
#'   Genotype = sprintf("MZ%03d", 1:12),
#'   Direct_effect = c(0.8, 0.6, 0.5, 0.3, 0.2, 0, -0.1, -0.3, -0.4, -0.6, -0.7, -0.9),
#'   Competition_effect = c(-0.3, 0.1, -0.2, 0.2, -0.1, 0.05, 0.1, -0.05, 0.15,
#'                          0.2, -0.1, 0.25)
#' )
#' g$Pure_stand_effect <- g$Direct_effect + 2 * g$Competition_effect
#' plot_direct_vs_competition(g, k = 2)
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
    # A thin surface-coloured ring separates overlapping markers, which reads
    # far more cleanly in a dense cloud than blanket transparency.
    ggplot2::geom_point(ggplot2::aes(size = .data$Pure, fill = .data$Pure),
                        shape = 21, colour = PAL$surface, stroke = 0.4) +
    scale_diverging(d$Pure, centre = 0, aesthetic = "fill",
                    name = "Pure-stand effect") +
    ggplot2::scale_size_continuous(range = c(1.6, 5), guide = "none") +
    # Labels are repelled outwards, so the panel needs margin on both axes or
    # the extreme genotypes - exactly the ones worth naming - are clipped.
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = 0.10)) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = 0.10)) +
    ggplot2::labs(
      title = "Direct versus competitive genetic effects",
      subtitle = wrap_subtitle(sprintf(paste(
        "Horizontal axis: performance in the genotype's own plot.",
        "Vertical axis: its effect on neighbouring plots.",
        "Dotted lines join genotypes of equal pure-stand value (direct + %d x competitive)."), k), base_size),
      x = "Direct effect",
      y = "Competitive effect",
      caption = wrap_caption(caption, base_size)) +
    theme_trial(base_size)

  if (has_pkg("ggrepel") && nrow(top)) {
    p <- p + ggrepel::geom_text_repel(
      data = top, ggplot2::aes(label = .data$Genotype),
      size = base_size * 0.24, colour = PAL$ink, max.overlaps = 20,
      min.segment.length = 0.2, segment.colour = PAL$muted, segment.size = 0.25,
      box.padding = 0.4, xlim = c(NA, NA), ylim = c(NA, NA))
  }
  p
}

#' Ranked pure-stand (or direct) effects with exact confidence intervals.
#'
#' Error bars use the prediction error variance of the plotted combination, so
#' the interval around a pure-stand value is correct rather than the far too
#' wide one obtained by adding the direct and competitive standard errors.
#' @param genetic The genotype table from `fit_single_model()$genetic`, or any
#'   data frame with the same columns.
#' @param effect Which quantity to rank on. `"Predicted_pure_stand_yield"`
#'   is the default because it is what selection acts on; it falls back to
#'   `"Pure_stand_effect"` when the fitted mean is unavailable.
#' @param se_col Name of the standard-error column. Chosen from `effect` when
#'   `NULL`; error bars are omitted if no usable column is present.
#' @param top_n Number of genotypes to show.
#' @param conf Confidence level for the error bars.
#' @param base_size Base font size in points. [save_figure()] chooses this from
#'   the export width; pass it explicitly only when composing a figure by hand.
#' @param caption Optional caption placed under the figure, wrapped to the
#'   plot width.
#' @return A [ggplot2::ggplot()] object.
#' @export
#' @examples
#' g <- data.frame(
#'   Genotype = sprintf("MZ%03d", 1:10),
#'   Pure_stand_effect = seq(0.9, -0.9, length.out = 10),
#'   SE_pure_stand = rep(0.15, 10)
#' )
#' plot_ranking(g, effect = "Pure_stand_effect", top_n = 10)
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
    # Colour carries no information here - the position along the axis already
    # is the value - so the points take the single series colour of the
    # quantity being ranked rather than a ramp that repeats the x axis.
    ggplot2::geom_point(size = 2.4, colour = series_colour(effect)) +
    ggplot2::labs(
      title = sprintf("Top %d genotypes by %s", nrow(d), tolower(label)),
      subtitle = wrap_subtitle(if (has_se) {
        sprintf("Points are BLUPs; bars are %.0f%% intervals from the exact prediction error variance", conf * 100)
      } else "Points are BLUPs; no exact prediction error variance was available", base_size),
      x = label, y = NULL, caption = wrap_caption(caption, base_size)) +
    theme_trial(base_size, grid = "y")
  p
}

#' How much does accounting for competition change the selection decision?
#'
#' A slope chart between the direct-effect ranking and the pure-stand ranking.
#' Large crossings are the practical payoff of the competition model: those
#' genotypes would have been mis-selected on direct effects alone.
#' @param genetic The genotype table from `fit_single_model()$genetic`, or any
#'   data frame with the same columns.
#' @param top_n Number of genotypes to show, taken from the top of the
#'   pure-stand ranking.
#' @param base_size Base font size in points. [save_figure()] chooses this from
#'   the export width; pass it explicitly only when composing a figure by hand.
#' @param caption Optional caption placed under the figure, wrapped to the
#'   plot width.
#' @return A [ggplot2::ggplot()] object.
#' @export
#' @examples
#' g <- data.frame(
#'   Genotype = sprintf("MZ%03d", 1:8),
#'   Rank_direct = c(1, 2, 3, 4, 5, 6, 7, 8),
#'   Rank_pure_stand = c(4, 1, 6, 2, 8, 3, 5, 7)
#' )
#' g$Rank_change <- g$Rank_direct - g$Rank_pure_stand
#' plot_rank_change(g)
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
    # Ranks start at 1, so a pretty-break axis that offers 0 labels a position
    # that cannot exist.
    ggplot2::scale_y_reverse(
      breaks = function(x) {
        b <- scales::breaks_pretty()(x)
        unique(c(1, b[b >= 1]))
      }) +
    # Red-to-green poles are the one pairing deuteranopes cannot resolve, so
    # the shared blue-to-earth ramp is used instead, centred on "no change".
    scale_diverging(long$Change, centre = 0, aesthetic = "colour",
                    name = "Places gained") +
    ggplot2::scale_x_discrete(expand = ggplot2::expansion(mult = c(0.08, 0.30))) +
    ggplot2::labs(
      title = "Selection decisions change once competition is modelled",
      subtitle = wrap_subtitle("Rank on the direct effect alone compared with rank on the pure-stand value", base_size),
      x = NULL, y = "Rank (1 = best)", caption = wrap_caption(caption, base_size)) +
    theme_trial(base_size, grid = "y")
}

#' Variance components as a share of the total.
#' @param varcomp The variance-parameter table from
#'   `fit_single_model()$varcomp` or `fit_met_model()$varcomp`. Bars are
#'   labelled with the `Interpretation` column where present, so that ASReml
#'   parameter strings never reach the figure.
#' @param base_size Base font size in points. [save_figure()] chooses this from
#'   the export width; pass it explicitly only when composing a figure by hand.
#' @param caption Optional caption placed under the figure, wrapped to the
#'   plot width.
#' @return A [ggplot2::ggplot()] object.
#' @export
#' @examples
#' vc <- data.frame(
#'   Component = c("Geno", "N1", "Column:Row!R"),
#'   Interpretation = c("Direct genetic variance", "Competitive genetic variance",
#'                      "Spatial residual variance"),
#'   Estimate = c(0.27, 0.07, 0.31),
#'   Pct_of_total = c(41.5, 10.8, 47.7)
#' )
#' plot_variance_components(vc)
plot_variance_components <- function(varcomp, base_size = 12, caption = NULL) {
  d <- varcomp[is.finite(varcomp$Pct_of_total) & varcomp$Pct_of_total > 0, , drop = FALSE]
  if (!nrow(d)) stop("No positive variance components to plot.")
  d <- d[order(d$Pct_of_total), ]
  # Axis labels are the breeder-facing names, not the ASReml parameter strings
  # that produced them; the raw names remain in the variance-parameter table.
  d$Label <- if ("Interpretation" %in% names(d)) d$Interpretation else d$Component
  d$Label <- factor(d$Label, levels = unique(d$Label))

  ggplot2::ggplot(d, ggplot2::aes(y = .data$Label, x = .data$Pct_of_total)) +
    ggplot2::geom_col(fill = PAL$primary, width = 0.7) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.1f%%", .data$Pct_of_total)),
                       hjust = -0.15, size = base_size * 0.24, colour = PAL$body) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, 0.16))) +
    ggplot2::labs(
      title = "Contribution of each variance component",
      subtitle = wrap_subtitle(paste(
        "Share of the total estimated variance, residual included.",
        "Covariances and correlations are not variances and are excluded from",
        "the total; see the variance-parameter table for every estimate."),
        base_size),
      x = "Share of total variance (%)", y = NULL,
      caption = wrap_caption(caption, base_size)) +
    theme_trial(base_size, grid = "y")
}

# ---------------------------------------------------------------------------
# Diagnostics
# ---------------------------------------------------------------------------

#' Residual diagnostics: fitted values, normal quantiles and distribution.
#' @param res The residual table from `fit_single_model()$residuals` or
#'   `fit_met_model()$residuals`, holding at least `Fitted`, `Residual` and
#'   `Std_residual`.
#' @param base_size Base font size in points. [save_figure()] chooses this from
#'   the export width; pass it explicitly only when composing a figure by hand.
#' @param caption Optional caption placed under the figure, wrapped to the
#'   plot width.
#' @return A [ggplot2::ggplot()] object.
#' @details Four panels are drawn when \pkg{patchwork} is installed; without
#'   it the residual-versus-fitted panel is returned on its own.
#' @export
#' @examples
#' set.seed(1)
#' res <- data.frame(Fitted = rnorm(60, 8, 0.5), Residual = rnorm(60, 0, 0.3),
#'                   Row = rep(1:10, 6), Column = rep(1:6, each = 10))
#' res$Std_residual <- res$Residual / sd(res$Residual)
#' plot_residual_diagnostics(res)
plot_residual_diagnostics <- function(res, base_size = 12, caption = NULL) {
  res <- res[is.finite(res$Residual) & is.finite(res$Fitted), , drop = FALSE]
  if (!nrow(res)) stop("No residuals are available for diagnostics.")

  # Each panel of the 2 x 2 grid is half the width of the canvas, so it takes
  # the type size that width would be given on its own - by the same square-root
  # rule the export uses. Handing every panel the full-canvas size is what makes
  # the two right-hand titles run off the page.
  panel_size <- base_size * sqrt(0.5)

  # Titles align to the panel, not the whole plot: with plot-aligned titles the
  # left-hand panel's title runs under the right-hand panel in a 2 x 2 grid.
  panel_title <- ggplot2::theme(plot.title.position = "panel",
                                plot.title = ggplot2::element_text(
                                  size = panel_size * 1.02, face = "bold"))

  p1 <- ggplot2::ggplot(res, ggplot2::aes(.data$Fitted, .data$Std_residual)) +
    ggplot2::geom_hline(yintercept = 0, colour = PAL$muted, linetype = "dashed") +
    ggplot2::geom_hline(yintercept = c(-3, 3), colour = PAL$danger,
                        linetype = "dotted", linewidth = 0.4) +
    ggplot2::geom_point(colour = PAL$primary, alpha = 0.55, size = 1.5) +
    ggplot2::geom_smooth(method = "loess", formula = y ~ x, se = FALSE,
                         colour = PAL$accent, linewidth = 0.7) +
    ggplot2::labs(title = "Residuals vs fitted",
                  x = "Fitted value", y = "Standardised residual") +
    theme_trial(panel_size) + panel_title

  q <- data.frame(sample = sort(res$Std_residual))
  q$theoretical <- theoretical_quantiles(nrow(q))
  p2 <- ggplot2::ggplot(q, ggplot2::aes(.data$theoretical, .data$sample)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, colour = PAL$muted,
                         linetype = "dashed") +
    ggplot2::geom_point(colour = PAL$primary, alpha = 0.55, size = 1.5) +
    ggplot2::labs(title = "Normal quantile-quantile",
                  x = "Theoretical quantile", y = "Standardised residual") +
    theme_trial(panel_size) + panel_title

  p3 <- ggplot2::ggplot(res, ggplot2::aes(.data$Std_residual)) +
    ggplot2::geom_histogram(bins = 30, fill = PAL$primary, colour = "white",
                            linewidth = 0.2) +
    ggplot2::labs(title = "Residual distribution",
                  x = "Standardised residual", y = "Plots") +
    theme_trial(panel_size) + panel_title

  p4 <- plot_field_map(res, "Std_residual",
                       title = "Residuals on the field plan",
                       diverging = TRUE, facet = length(unique(res$Env)) > 1,
                       base_size = panel_size, fill_label = "Std.
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
        caption = wrap_caption(paste(guidance, caption, sep = " "), base_size),
        theme = theme_trial(base_size))
  } else {
    p1 + ggplot2::labs(caption = wrap_caption(paste(guidance, caption), base_size))
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
      subtitle = wrap_subtitle("Should rise smoothly to a plateau; ridges or a continuing rise indicate unmodelled field trend", base_size),
      x = "Column displacement", y = "Row displacement",
      caption = wrap_caption(caption, base_size)) +
    theme_trial(base_size, grid = "none") +
    ggplot2::theme(legend.position = "right",
                   legend.key.height = grid::unit(1.6, "lines"),
                   legend.box.margin = ggplot2::margin(t = 8))
}

# ---------------------------------------------------------------------------
# MET figures
# ---------------------------------------------------------------------------

#' Genetic-correlation heatmap between environments.
#' @param m A square correlation matrix with dimnames, such as
#'   `fit_met_model()$matrices$direct_cor`.
#' @param title Figure title, naming the effect the matrix describes.
#' @param subtitle Optional subtitle.
#' @param show_values Print the correlation in each cell.
#' @param base_size Base font size in points. [save_figure()] chooses this from
#'   the export width; pass it explicitly only when composing a figure by hand.
#' @param caption Optional caption placed under the figure, wrapped to the
#'   plot width.
#' @return A [ggplot2::ggplot()] object.
#' @export
#' @examples
#' m <- matrix(c(1, 0.84, 0.64, 0.84, 1, 0.58, 0.64, 0.58, 1), 3, 3,
#'             dimnames = list(paste0("Env0", 1:3), paste0("Env0", 1:3)))
#' plot_correlation_heatmap(m, title = "Direct effects")
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
    ggplot2::labs(title = title, subtitle = wrap_subtitle(subtitle, base_size), x = NULL, y = NULL,
                  caption = wrap_caption(caption, base_size)) +
    theme_trial(base_size, grid = "none") +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      legend.position = "right",
      legend.key.height = grid::unit(1.8, "lines"),
      legend.key.width = grid::unit(0.55, "lines"),
      legend.box.margin = ggplot2::margin(t = 8))

  if (show_values && nrow(m) <= 16) {
    labelled <- long[!is.na(long$Correlation), , drop = FALSE]
    # The label colour is taken from the luminance of the step it is drawn on,
    # so a value never comes out as white text on a pale part of the ramp.
    fills <- scales::gradient_n_pal(DIVERGING)(
      scales::rescale(labelled$Correlation, from = c(-1, 1)))
    labelled$Label_colour <- contrast_text(fills)
    p <- p + ggplot2::geom_text(
      data = labelled,
      ggplot2::aes(label = sprintf("%.2f", .data$Correlation)),
      colour = labelled$Label_colour,
      size = base_size * 0.23, fontface = "bold", show.legend = FALSE)
  }
  p
}

#' Direct, competitive and pure-stand genetic variance in each environment.
#' @param v The per-environment variance table from
#'   `fit_met_model()$variance`, holding `Environment`, `Direct_variance`,
#'   `Competition_variance` and `Pure_stand_variance`.
#' @param base_size Base font size in points. [save_figure()] chooses this from
#'   the export width; pass it explicitly only when composing a figure by hand.
#' @param caption Optional caption placed under the figure, wrapped to the
#'   plot width.
#' @return A [ggplot2::ggplot()] object.
#' @export
#' @examples
#' v <- data.frame(
#'   Environment = paste0("Env0", 1:3),
#'   Direct_variance = c(0.30, 0.40, 0.20),
#'   Competition_variance = c(0.05, 0.07, 0.04),
#'   Pure_stand_variance = c(0.22, 0.31, 0.15)
#' )
#' plot_environment_variances(v)
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
      subtitle = wrap_subtitle("Pure-stand variance combines the direct and competitive variances with their covariance", base_size),
      x = NULL, y = "Genetic variance", caption = wrap_caption(caption, base_size)) +
    theme_trial(base_size, grid = "y") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))
}

#' Stability of pure-stand performance across environments.
#'
#' A parallel-coordinates view of the selection candidates. Lines that stay
#' parallel indicate stable genotypes; lines that cross indicate crossover
#' genotype-by-environment interaction and therefore environment-specific
#' recommendations.
#' @param values The genotype-by-environment table from
#'   `fit_met_model()$values`, holding `Genotype`, `Environment`,
#'   `Pure_stand_effect` and `Status`.
#' @param top_n Number of genotypes to trace, taken from the top of the
#'   across-environment mean.
#' @param base_size Base font size in points. [save_figure()] chooses this from
#'   the export width; pass it explicitly only when composing a figure by hand.
#' @param caption Optional caption placed under the figure, wrapped to the
#'   plot width.
#' @return A [ggplot2::ggplot()] object.
#' @export
#' @examples
#' values <- expand.grid(Genotype = sprintf("MZ%02d", 1:6),
#'                       Environment = paste0("Env0", 1:3),
#'                       stringsAsFactors = FALSE)
#' set.seed(2)
#' values$Pure_stand_effect <- rnorm(nrow(values), 0, 0.4)
#' values$Status <- "Estimable"
#' plot_stability(values, top_n = 6)
plot_stability <- function(values, top_n = 12, base_size = 12, caption = NULL) {
  d <- values[values$Status == "Estimable", , drop = FALSE]
  mean_effect <- tapply(d$Pure_stand_effect, d$Genotype, mean, na.rm = TRUE)
  keep <- names(sort(mean_effect, decreasing = TRUE))[seq_len(min(top_n, length(mean_effect)))]
  d <- d[d$Genotype %in% keep, , drop = FALSE]

  # Emphasis rather than one hue per genotype. Giving each of a dozen lines its
  # own colour puts the whole set past the point where adjacent hues can be
  # told apart - and under colour-vision deficiency well past it - so the few
  # genotypes the reader is actually choosing between are drawn in the series
  # colours and directly labelled, and the rest form a recessive backdrop that
  # still shows the spread and the crossovers.
  n_lead <- min(3L, length(keep))
  lead <- keep[seq_len(n_lead)]
  d$Lead <- factor(ifelse(d$Genotype %in% lead, d$Genotype, "Other"),
                   levels = c(lead, "Other"))
  lead_colours <- stats::setNames(
    c(PAL$direct, PAL$competition, PAL$pure)[seq_len(n_lead)], lead)
  back <- d[!d$Genotype %in% lead, , drop = FALSE]
  front <- d[d$Genotype %in% lead, , drop = FALSE]

  last_env <- levels(factor(d$Environment))
  last_env <- last_env[length(last_env)]
  tips <- front[as.character(front$Environment) == last_env, , drop = FALSE]

  p <- ggplot2::ggplot(mapping = ggplot2::aes(x = .data$Environment,
                                              y = .data$Pure_stand_effect,
                                              group = .data$Genotype)) +
    ggplot2::geom_hline(yintercept = 0, colour = PAL$muted, linetype = "dashed",
                        linewidth = 0.4) +
    ggplot2::geom_line(data = back, colour = PAL$faint, linewidth = 0.45,
                       alpha = 0.7) +
    ggplot2::geom_point(data = back, colour = PAL$faint, size = 1.2, alpha = 0.7) +
    ggplot2::geom_line(data = front,
                       ggplot2::aes(colour = .data$Genotype), linewidth = 0.9) +
    ggplot2::geom_point(data = front,
                        ggplot2::aes(colour = .data$Genotype), size = 2.2) +
    ggplot2::scale_colour_manual(values = lead_colours, name = NULL,
                                 breaks = lead) +
    ggplot2::scale_x_discrete(expand = ggplot2::expansion(mult = c(0.04, 0.22))) +
    ggplot2::labs(
      title = sprintf("Pure-stand performance of the top %d genotypes", length(keep)),
      subtitle = wrap_subtitle(sprintf(paste(
        "Crossing lines indicate crossover genotype-by-environment interaction.",
        "The %d highest-ranked genotypes are named; the remaining %d are shown",
        "in grey for context."), n_lead, length(keep) - n_lead), base_size),
      x = NULL, y = "Pure-stand genetic effect",
      caption = wrap_caption(caption, base_size)) +
    theme_trial(base_size, grid = "y") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))

  # A legend is always present for two or more series; four or fewer are also
  # direct-labelled, so identity never rests on colour alone.
  if (nrow(tips)) {
    p <- p + ggplot2::geom_text(
      data = tips,
      ggplot2::aes(label = .data$Genotype, colour = .data$Genotype),
      hjust = -0.18, size = base_size * 0.24, fontface = "bold",
      show.legend = FALSE)
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
      subtitle = wrap_subtitle("Environments below the dotted line behave idiosyncratically and pool poorly with the rest", base_size),
      x = NULL, y = "Variance explained (%)", caption = wrap_caption(caption, base_size)) +
    theme_trial(base_size, grid = "y") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))
}

#' Direct against competitive effects, one panel per environment.
#' @param values The genotype-by-environment table from
#'   `fit_met_model()$values`.
#' @param k Number of neighbours per plot, from `fit_met_model()$k`.
#' @param base_size Base font size in points. [save_figure()] chooses this from
#'   the export width; pass it explicitly only when composing a figure by hand.
#' @param caption Optional caption placed under the figure, wrapped to the
#'   plot width.
#' @return A [ggplot2::ggplot()] object.
#' @export
#' @examples
#' values <- expand.grid(Genotype = sprintf("MZ%02d", 1:8),
#'                       Environment = paste0("Env0", 1:2),
#'                       stringsAsFactors = FALSE)
#' set.seed(3)
#' values$Direct_effect <- rnorm(nrow(values), 0, 0.5)
#' values$Competition_effect <- rnorm(nrow(values), 0, 0.2)
#' values$Pure_stand_effect <- values$Direct_effect + 2 * values$Competition_effect
#' values$Status <- "Estimable"
#' plot_met_scatter(values, k = 2)
plot_met_scatter <- function(values, k = 2, base_size = 12, caption = NULL) {
  d <- values[values$Status == "Estimable", , drop = FALSE]
  ggplot2::ggplot(d, ggplot2::aes(.data$Direct_effect, .data$Competition_effect)) +
    ggplot2::geom_hline(yintercept = 0, colour = PAL$muted, linetype = "dashed",
                        linewidth = 0.35) +
    ggplot2::geom_vline(xintercept = 0, colour = PAL$muted, linetype = "dashed",
                        linewidth = 0.35) +
    ggplot2::geom_point(ggplot2::aes(fill = .data$Pure_stand_effect), shape = 21,
                        colour = PAL$surface, stroke = 0.3, size = 1.9) +
    scale_diverging(d$Pure_stand_effect, centre = 0, aesthetic = "fill",
                    name = "Pure-stand effect") +
    ggplot2::facet_wrap(~ Environment) +
    ggplot2::labs(
      title = "Direct and competitive effects within each environment",
      subtitle = wrap_subtitle(sprintf("Pure-stand value is direct + %d x competitive", k), base_size),
      x = "Direct effect", y = "Competitive effect",
      caption = wrap_caption(caption, base_size)) +
    theme_trial(base_size)
}
