# Pad every environment to its full rectangular grid

Inserts the field positions that are absent from the data, with a
missing response, so that each environment forms the complete rectangle
an AR1 x AR1 residual requires. Indexing with `NA` reproduces each
column's class and factor levels, so the padded records are structurally
identical to real plots but carry no response, no genotype and no design
membership.

## Usage

``` r
complete_field_grid(d)
```

## Arguments

- d:

  prepared trial data from
  [`prepare_trial_data()`](https://tolekeno.github.io/InterPlotComp/reference/prepare_trial_data.md).

## Value

The same data frame with padded rows added, a logical `Padded` column
marking them, and an `n_padded` attribute giving how many were inserted.

## Examples

``` r
d <- prepare_trial_data(
  sample_single_trial(),
  list(yield = "Yield_t_ha", geno = "Genotype", row = "Row", column = "Column")
)
nrow(d)
#> [1] 179
nrow(complete_field_grid(d))
#> [1] 180
```
