# Ranked pure-stand (or direct) effects with exact confidence intervals.

Error bars use the prediction error variance of the plotted combination,
so the interval around a pure-stand value is correct rather than the far
too wide one obtained by adding the direct and competitive standard
errors.

## Usage

``` r
plot_ranking(
  genetic,
  effect = c("Predicted_pure_stand_yield", "Pure_stand_effect", "Direct_effect"),
  se_col = NULL,
  top_n = 30,
  base_size = 12,
  caption = NULL,
  conf = 0.95
)
```

## Arguments

- genetic:

  The genotype table from `fit_single_model()$genetic`, or any data
  frame with the same columns.

- effect:

  Which quantity to rank on. `"Predicted_pure_stand_yield"` is the
  default because it is what selection acts on; it falls back to
  `"Pure_stand_effect"` when the fitted mean is unavailable.

- se_col:

  Name of the standard-error column. Chosen from `effect` when `NULL`;
  error bars are omitted if no usable column is present.

- top_n:

  Number of genotypes to show.

- base_size:

  Base font size in points.
  [`save_figure()`](https://tolekeno.github.io/InterPlotComp/reference/save_figure.md)
  chooses this from the export width; pass it explicitly only when
  composing a figure by hand.

- caption:

  Optional caption placed under the figure, wrapped to the plot width.

- conf:

  Confidence level for the error bars.

## Value

A
[`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html)
object.

## Examples

``` r
g <- data.frame(
  Genotype = sprintf("MZ%03d", 1:10),
  Pure_stand_effect = seq(0.9, -0.9, length.out = 10),
  SE_pure_stand = rep(0.15, 10)
)
plot_ranking(g, effect = "Pure_stand_effect", top_n = 10)
```
