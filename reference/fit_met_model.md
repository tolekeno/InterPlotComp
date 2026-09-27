# Fit the multi-environment competition model

Fits environment-specific direct and competitive genotype effects
sharing one joint covariance across environments, with an AR1 x AR1
residual per environment. Requires 'ASReml-R' and a valid licence.

## Usage

``` r
fit_met_model(d, neighbour_names, opts, progress = NULL)
```

## Arguments

- d:

  Prepared MET data from
  [`prepare_trial_data()`](https://tolekeno.github.io/InterPlotComp/reference/prepare_trial_data.md)
  with `multi_env = TRUE`, grid-completed by
  [`complete_field_grid()`](https://tolekeno.github.io/InterPlotComp/reference/complete_field_grid.md)
  for a spatial model, and carrying the neighbour factors added by
  [`add_neighbours()`](https://tolekeno.github.io/InterPlotComp/reference/add_neighbours.md).

- neighbour_names:

  Names of the neighbour factors, from
  [`add_neighbours()`](https://tolekeno.github.io/InterPlotComp/reference/add_neighbours.md).

- opts:

  Named list of fitting options: `structure` (`"facv"` or `"diag"`; the
  `"separable"` and `"fa"` structures were removed in 3.9.0), `rank`,
  `spatial`, `nugget`, `auto_simplify`, `exact_se`, `compare_baseline`,
  `maxit`, `workspace`, `cinv_limit`, an optional `relationship` from
  [`build_relationship()`](https://tolekeno.github.io/InterPlotComp/reference/build_relationship.md),
  and:

  - `field_trend` - global field-trend adjustment (Gilmour, Cullis &
    Verbyla 1997). A named list with one entry per site to adjust, each
    `"lrow"`, `"lcol"` or `"both"`, for example
    `list(Env01 = "both", Env03 = "lrow")`; or an unnamed
    `c("lrow", "lcol")` to adjust every site the same way. Each selected
    site gets its own fixed linear slope, `at(Env, <site>):lrow`.

  - `outliers` - `"none"` (default), `"detect"` to flag observations
    whose standardised conditional residual exceeds `outlier_threshold`,
    or `"remove"` to flag them, set their response to missing and refit.

  - `outlier_threshold` - the absolute standardised residual beyond
    which an observation is an outlier; default 4.

- progress:

  Optional `function(i, n, reason)` called as the simplification ladder
  advances.

## Value

A list holding the fitted model, the genetic values by environment, the
direct, competitive and pure-stand covariance and correlation matrices,
variance summaries, the fitting log and residuals. When outlier
screening is on, `outliers` holds the flagged and removed observations
and a per-site summary; `field_trend` holds the trend specification; and
`model_code` feeds
[`asreml_script()`](https://tolekeno.github.io/InterPlotComp/reference/asreml_script.md).
