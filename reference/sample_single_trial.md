# Worked example: a single trial with inter-plot competition

Simulates one 15 x 12 single-row-plot trial of 60 genotypes from the
known parameters in `SIM_TRUTH`, so that a fitted model can be checked
against the values that generated the data. Includes the imperfections
that break naive code: an incomplete block design, AR1 spatial trend,
three failed plots and one position physically absent from the field.

## Usage

``` r
sample_single_trial()
```

## Value

A data frame with columns `Row`, `Column`, `Rep`, `Block`, `Genotype`
and `Yield_t_ha`.

## Examples

``` r
d <- sample_single_trial()
str(d)
#> 'data.frame':    179 obs. of  7 variables:
#>  $ Row            : int  1 2 3 4 5 6 7 8 9 10 ...
#>  $ Column         : int  1 1 1 1 1 1 1 1 1 1 ...
#>  $ Rep            : num  1 1 1 1 1 1 1 1 1 1 ...
#>  $ Block          : int  1 1 1 1 1 1 1 1 1 1 ...
#>  $ Genotype       : chr  "MZ001" "MZ037" "MZ006" "MZ004" ...
#>  $ Plant_height_cm: num  219 220 155 202 213 ...
#>  $ Yield_t_ha     : num  8.96 9.18 7.02 8.41 8.44 ...
#>  - attr(*, "effects")=List of 2
#>   ..$ direct     : Named num [1:60] 0.3124 -0.6478 0.0835 -0.0508 -0.4 ...
#>   .. ..- attr(*, "names")= chr [1:60] "MZ001" "MZ002" "MZ003" "MZ004" ...
#>   ..$ competition: Named num [1:60] -0.1213 0.2201 -0.0728 -0.059 -0.3246 ...
#>   .. ..- attr(*, "names")= chr [1:60] "MZ001" "MZ002" "MZ003" "MZ004" ...
```
