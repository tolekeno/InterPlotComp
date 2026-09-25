# Worked example: a pedigree for the example trials

A conventional breeding structure: ten unrelated founders crossed as
five males by five females, so the trial entries form overlapping
full-sib and half-sib families. That is exactly the structure a
relationship matrix exploits, and it lets the worked example demonstrate
the feature end to end.

## Usage

``` r
sample_pedigree(genotypes = NULL)
```

## Arguments

- genotypes:

  Character vector of entry identifiers to assign parents to. Defaults
  to the genotypes of both worked example trials.

## Value

A data frame with columns `Genotype`, `Male_parent` and `Female_parent`,
founders having `NA` parents.

## Examples

``` r
ped <- sample_pedigree()
head(ped)
#>     Genotype Male_parent Female_parent
#> 1 FOUNDER_M1        <NA>          <NA>
#> 2 FOUNDER_M2        <NA>          <NA>
#> 3 FOUNDER_M3        <NA>          <NA>
#> 4 FOUNDER_M4        <NA>          <NA>
#> 5 FOUNDER_M5        <NA>          <NA>
#> 6 FOUNDER_F1        <NA>          <NA>
```
