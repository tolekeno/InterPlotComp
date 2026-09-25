# Genetic-correlation heatmap between environments.

Genetic-correlation heatmap between environments.

## Usage

``` r
plot_correlation_heatmap(
  m,
  title,
  subtitle = NULL,
  base_size = 12,
  caption = NULL,
  show_values = TRUE
)
```

## Arguments

- m:

  A square correlation matrix with dimnames, such as
  `fit_met_model()$matrices$direct_cor`.

- title:

  Figure title, naming the effect the matrix describes.

- subtitle:

  Optional subtitle.

- base_size:

  Base font size in points.
  [`save_figure()`](https://tolekeno.github.io/InterPlotComp/reference/save_figure.md)
  chooses this from the export width; pass it explicitly only when
  composing a figure by hand.

- caption:

  Optional caption placed under the figure, wrapped to the plot width.

- show_values:

  Print the correlation in each cell.

## Value

A
[`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html)
object.

## Examples

``` r
m <- matrix(c(1, 0.84, 0.64, 0.84, 1, 0.58, 0.64, 0.58, 1), 3, 3,
            dimnames = list(paste0("Env0", 1:3), paste0("Env0", 1:3)))
plot_correlation_heatmap(m, title = "Direct effects")
```
