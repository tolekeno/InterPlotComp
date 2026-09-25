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

  Named list of fitting options: `structure` (one of `MET_STRUCTURES`),
  `rank`, `spatial`, `nugget`, `auto_simplify`, `exact_se`,
  `compare_baseline`, `maxit`, `workspace`, `cinv_limit` and an optional
  `relationship` from
  [`build_relationship()`](https://tolekeno.github.io/InterPlotComp/reference/build_relationship.md).

- progress:

  Optional `function(i, n, reason)` called as the simplification ladder
  advances.

## Value

A list holding the fitted model, the genetic values by environment, the
direct, competitive and pure-stand covariance and correlation matrices,
variance summaries, the fitting log and residuals.
