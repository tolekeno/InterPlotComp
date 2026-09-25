# Diagnostics on how well the competition term is supported by the layout.

Two things commonly undermine a competition model and are invisible in a
plain data preview: too many border plots (so most records carry an
incomplete neighbour set), and neighbour pairings that repeat across
replicates (so a genotype is nearly always beside the same neighbour,
making direct and competitive effects hard to separate).

## Usage

``` r
competition_diagnostics(d, neighbour_names)
```

## Arguments

- d:

  The trial data with neighbour factors attached, i.e.
  `add_neighbours(...)$data`.

- neighbour_names:

  The neighbour column names, i.e. `add_neighbours(...)$names`.

## Value

A list with the number of observed plots, the percentage carrying a
complete neighbour set, the mean number of neighbours, the number of
border plots, the mean number of distinct neighbour genotypes per
genotype, the percentage of self-neighbour pairings, the number of
genotypes and the neighbour count `k`.

## See also

[`competition_warnings()`](https://tolekeno.github.io/InterPlotComp/reference/competition_warnings.md),
which turns this into plain-English warnings.

## Examples

``` r
d <- prepare_trial_data(
  sample_single_trial(),
  list(yield = "Yield_t_ha", geno = "Genotype", row = "Row", column = "Column")
)
nb <- add_neighbours(complete_field_grid(d), "rows")
competition_diagnostics(nb$data, nb$names)
#> $n_observed
#> [1] 176
#> 
#> $full_neighbour_pct
#> [1] 85.22727
#> 
#> $mean_neighbours
#> [1] 1.852273
#> 
#> $border_plots
#> [1] 26
#> 
#> $mean_distinct_neighbours
#> [1] 5.166667
#> 
#> $self_neighbour_pct
#> [1] 0
#> 
#> $n_genotypes
#> [1] 60
#> 
#> $k
#> [1] 2
#> 
```
