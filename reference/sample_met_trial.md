# Worked example: a multi-environment trial series

Four environments of different sizes, sharing a core genotype set with
environment-specific additions, moderate crossover
genotype-by-environment interaction, unequal replication, six failed
plots and one internal grid hole.

## Usage

``` r
sample_met_trial()
```

## Value

A data frame with columns \`Environment\`, \`Row\`, \`Column\`, \`Rep\`,
\`Block\`, \`Genotype\` and \`Yield_t_ha\`.

## Examples

``` r
d <- sample_met_trial()
table(d$Environment)
#> 
#> Env01 Env02 Env03 Env04 
#>   108   111   100   120 
```
