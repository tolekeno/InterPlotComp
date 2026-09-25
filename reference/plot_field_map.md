# Field-plan heatmap of an observed or fitted quantity.

The single most useful diagnostic for a spatially analysed trial: it
shows the physical field, so a fertility gradient, a headland effect or
a mis-entered coordinate is visible immediately.

## Usage

``` r
plot_field_map(
  d,
  value = "Observed",
  title = NULL,
  subtitle = NULL,
  diverging = FALSE,
  facet = FALSE,
  base_size = 12,
  caption = NULL,
  fill_label = NULL
)
```

## Arguments

- d:

  A data frame with `Row`, `Column` and the value column. The
  coordinates are positions on the field, so they are mapped on
  continuous axes; the prepared trial data holds them as factors in
  `Row`/`Column` and as integers in `Row_i`/`Col_i`, and either form is
  accepted here.

- value:

  Name of the column to map.

- title, subtitle:

  Figure text.

- diverging:

  Use the diverging palette (for residuals and other signed quantities)
  rather than the sequential one (for yields).

- facet:

  Facet by environment, for a multi-environment trial.

- base_size:

  Base font size in points.

- caption:

  Optional caption placed under the figure.

- fill_label:

  Legend title; defaults to `value`.

## Value

A
[`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html)
object.

## Examples

``` r
d <- prepare_trial_data(
  sample_single_trial(),
  list(yield = "Yield_t_ha", geno = "Genotype", row = "Row", column = "Column")
)
plot_field_map(
  data.frame(Row = d$Row_i, Column = d$Col_i, Observed = d$Yield),
  value = "Observed", title = "Observed yield on the field plan"
)
```
