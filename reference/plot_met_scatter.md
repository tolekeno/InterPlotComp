# Direct against competitive effects, one panel per environment.

Direct against competitive effects, one panel per environment.

## Usage

``` r
plot_met_scatter(values, k = 2, base_size = 12, caption = NULL)
```

## Arguments

- values:

  The genotype-by-environment table from `fit_met_model()$values`.

- k:

  Number of neighbours per plot, from `fit_met_model()$k`.

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
values <- expand.grid(Genotype = sprintf("MZ%02d", 1:8),
                      Environment = paste0("Env0", 1:2),
                      stringsAsFactors = FALSE)
set.seed(3)
values$Direct_effect <- rnorm(nrow(values), 0, 0.5)
values$Competition_effect <- rnorm(nrow(values), 0, 0.2)
values$Pure_stand_effect <- values$Direct_effect + 2 * values$Competition_effect
values$Status <- "Estimable"
plot_met_scatter(values, k = 2)
```
