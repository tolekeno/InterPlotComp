# Basic workflow

``` r

library(InterPlotComp)
```

This vignette runs a complete single-trial analysis, from a raw data
frame to a ranked list of selection candidates. Everything up to the fit
itself runs without ASReml-R.

## 1. The data

The package simulates its worked examples from known parameters, so a
fitted model can be checked against the values that generated the data.
The single trial is 15 field rows by 12 columns, 60 genotypes in an
incomplete block design, with AR1 spatial trend, three failed plots and
one field position physically absent:

``` r

trial <- sample_single_trial()
head(trial)
#>   Row Column Rep Block Genotype Plant_height_cm Yield_t_ha
#> 1   1      1   1     1    MZ001           219.2      8.965
#> 2   2      1   1     1    MZ037           219.8      9.184
#> 3   3      1   1     1    MZ006           154.6      7.016
#> 4   4      1   1     1    MZ004           201.6      8.408
#> 5   5      1   1     1    MZ046           213.4      8.436
#> 6   6      1   1     1    MZ054           198.8      8.107
dim(trial)
#> [1] 179   7
```

Your own data needs one row per plot and, at minimum, four columns: the
response, the genotype, and the two **physical field coordinates**. The
coordinates are not optional and not decorative — they are what makes
both the spatial model and the neighbour lookup possible.

## 2. Preparing the trial

[`prepare_trial_data()`](https://tolekeno.github.io/InterPlotComp/reference/prepare_trial_data.md)
validates the file and reshapes it into the internal analysis layout.
You tell it which of your columns is which:

``` r

prepared <- prepare_trial_data(
  trial,
  map = list(yield  = "Yield_t_ha",
             geno   = "Genotype",
             row    = "Row",
             column = "Column",
             rep    = "Rep",
             block  = "Block")
)
```

It refuses data that cannot support a competition model rather than
fitting something meaningless — too few observed plots, a single
genotype, two plots claiming the same field position, an observed yield
with no genotype attached. It also re-indexes the field coordinates onto
a contiguous grid, which matters more than it sounds: an entirely absent
field row would otherwise collapse out of the factor and make AR1 treat
the plots either side of it as adjacent.

A per-environment layout summary comes back attached:

``` r

attr(prepared, "field_summary")
#>   Environment Rows Columns    Grid Plots Observed Missing_response Gaps_in_grid
#> 1       Trial   15      12 15 × 12   179      176                3            1
#>   Genotypes Min_reps Max_reps Unreplicated
#> 1        60        2        3            0
```

## 3. Completing the field grid

A separable AR1 × AR1 residual needs a complete rectangle. Missing
positions are padded with a missing response; they define the residual
covariance layout and carry no genotype, so they contribute no genetic
effect.

``` r

complete <- complete_field_grid(prepared)
c(observed = nrow(prepared), padded = nrow(complete))
#> observed   padded 
#>      179      180
attr(complete, "n_padded")
#> [1] 1
```

## 4. Attaching neighbours

[`add_neighbours()`](https://tolekeno.github.io/InterPlotComp/reference/add_neighbours.md)
records which genotype is growing in each adjacent plot. The axis should
match the direction in which plots actually compete — for single-row
plots that is almost always along the field rows.

``` r

nb <- add_neighbours(complete, axis = "rows")
nb$k        # number of neighbours per plot
#> [1] 2
nb$label
#> [1] "Adjacent field rows (Row ± 1, same column)"
```

Border plots legitimately have fewer neighbours; their absent neighbour
factors stay `NA` and are absorbed as a zero row in the design matrix.

``` r

table(nb$data$Neighbour_count)
#> 
#>   1   2 
#>  26 154
```

## 5. Fitting the model

``` r

result <- fit_single_model(
  nb$data, nb$names,
  opts = list(
    structure        = "us",    # unstructured direct-competition covariance
    spatial          = TRUE,    # AR1 x AR1 residual
    nugget           = TRUE,
    auto_simplify    = TRUE,    # fall back if the full model will not converge
    exact_se         = TRUE,
    cinv_limit       = 5000L,
    maxit            = 30L,
    workspace        = "1gb",
    compare_baseline = TRUE,    # test whether competition improves the fit
    max_rounds       = 15L
  )
)

result$converged
result$description
```

`auto_simplify = TRUE` is the safety net. The direct–competition
covariance is the hardest parameter in the model to estimate, and on a
small or awkwardly laid-out trial the full specification may not
converge. Rather than failing, the fit walks down a ladder of nested
models, giving up the least defensible assumption first, and tells you
what it gave up:

``` r

result$log
```

## 6. The variance components

``` r

result$variance[, c("Component", "Estimate")]
```

The data were simulated with a direct variance of 0.36, a competitive
variance of 0.09 and a direct–competition correlation of −0.55. A
negative correlation is the case that matters to a breeder: the
genotypes that yield most in their own plot are the ones suppressing
their neighbours, so part of their apparent advantage is borrowed and
will not survive into a pure stand.

## 7. The genotype table

``` r

g <- result$genetic
head(g[, c("Genotype", "Direct_effect", "Competition_effect",
           "Pure_stand_effect", "Rank_direct", "Rank_pure_stand",
           "Rank_change")], 10)
```

`Rank_change` is the practical output: how far each genotype moves once
its effect on its neighbours is accounted for.

``` r

summary(g$Rank_change)
sum(abs(g$Rank_change) >= 5)   # genotypes that move five places or more
```

## 8. Figures

``` r

plot_direct_vs_competition(g, k = result$k)
```

The quadrants separate the decisions. A genotype high on the direct axis
and benign on the competitive axis is an unambiguously good selection.
One high on both is flattered by the trial.

``` r

plot_ranking(g, top_n = 20)
```

## 9. Was competition worth modelling?

``` r

result$comparison$lrt
```

This is a likelihood-ratio test against the identical model with the
competitive effects removed. Both models are iterated to convergence and
carry the same fixed and residual structure, so the test isolates the
competition term. If it is not significant, report the simpler model.

## Doing all of this in the interface

``` r

run_app()
```

The Shiny interface performs exactly this pipeline, with the diagnostics
shown alongside each step and every table and figure exportable.
