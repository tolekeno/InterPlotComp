# Fit the single-trial competition model.

Fit the single-trial competition model.

## Usage

``` r
fit_single_model(d, neighbour_names, opts, progress = NULL)
```

## Arguments

- d:

  prepared trial data (already grid-completed if spatial)

- neighbour_names:

  N1..Nk

- opts:

  list of user options. Besides the model settings it takes
  `field_trend`, the linear field-trend covariates to fit as fixed
  effects (`"lrow"`, `"lcol"`, `c("lrow", "lcol")` or `"both"`; Gilmour,
  Cullis & Verbyla 1997), and `outliers` (`"none"`, `"detect"` or
  `"remove"`) with `outlier_threshold` (default 4) for screening on
  standardised conditional residuals. See
  [`fit_met_model()`](https://tolekeno.github.io/InterPlotComp/reference/fit_met_model.md)
  for the details, which are shared.

- progress:

  optional function(i, n, reason) for the busy indicator

## Value

a rich result list consumed by the UI
