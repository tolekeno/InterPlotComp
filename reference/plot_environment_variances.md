# Direct, competitive and pure-stand genetic variance in each environment.

Direct, competitive and pure-stand genetic variance in each environment.

## Usage

``` r
plot_environment_variances(v, base_size = 12, caption = NULL)
```

## Arguments

- v:

  The per-environment variance table from `fit_met_model()$variance`,
  holding `Environment`, `Direct_variance`, `Competition_variance` and
  `Pure_stand_variance`.

- base_size:

  Base font size in points.
  [`save_figure()`](https://tolekeno.github.io/InterPlotComp/reference/save_figure.md)
  chooses this from the export width; pass it explicitly only when
  composing a figure by hand.

- caption:

  Optional caption placed under the figure, wrapped to the plot width.

## Value

A
[`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html)
object.

## Examples

``` r
v <- data.frame(
  Environment = paste0("Env0", 1:3),
  Direct_variance = c(0.30, 0.40, 0.20),
  Competition_variance = c(0.05, 0.07, 0.04),
  Pure_stand_variance = c(0.22, 0.31, 0.15)
)
plot_environment_variances(v)
```
