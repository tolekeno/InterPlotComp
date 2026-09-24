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

  list of user options

- progress:

  optional function(i, n, reason) for the busy indicator

## Value

a rich result list consumed by the UI
