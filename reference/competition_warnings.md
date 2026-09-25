# Warnings about a layout that cannot support a competition model

Turns
[`competition_diagnostics()`](https://tolekeno.github.io/InterPlotComp/reference/competition_diagnostics.md)
into the specific sentences a user needs to read before trusting a fit.
Three things undermine a competition model and are invisible in a plain
data preview: too many border plots, neighbour pairings that repeat
across replicates, and too small a genotype panel.

## Usage

``` r
competition_warnings(x)
```

## Arguments

- x:

  The list returned by
  [`competition_diagnostics()`](https://tolekeno.github.io/InterPlotComp/reference/competition_diagnostics.md).

## Value

A character vector of warnings, empty when the layout raises none.

## Examples

``` r
d <- prepare_trial_data(
  sample_single_trial(),
  list(yield = "Yield_t_ha", geno = "Genotype", row = "Row", column = "Column")
)
nb <- add_neighbours(complete_field_grid(d), "rows")
competition_warnings(competition_diagnostics(nb$data, nb$names))
#> character(0)
```
