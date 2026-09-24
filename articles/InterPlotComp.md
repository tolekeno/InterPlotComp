# Installation and setup

## What this package is for

In an unbordered single-row-plot trial, every plot is exposed to
whatever is growing beside it. A vigorous entry takes light, water and
nutrients from its neighbours, so part of the yield recorded against it
was taken from the plots either side. The trial rewards it twice: once
for its own performance and once for depressing the plots it is compared
against.

`InterPlotComp` fits the direct–competition model that separates those
two things. For every genotype it estimates

- the **direct effect** — what the genotype expresses in its own plot;
- the **competitive effect** — what it does to a neighbouring plot;
- the **pure-stand value** — what it would express with neighbours like
  itself, which is the quantity selection should act on.

The whole pipeline is available as functions, and as a Shiny interface
launched with
[`run_app()`](https://tolekeno.github.io/InterPlotComp/reference/run_app.md).

## Installing the package

``` r

# install.packages("remotes")
remotes::install_github("tolekeno/InterPlotComp")
```

The required packages are pulled in automatically. Several optional
packages each add one feature — interactive figures, higher-quality
image export, Excel output — and every one of them degrades to a working
fallback, so a minimal installation is fully usable:

``` r

remotes::install_github("tolekeno/InterPlotComp", dependencies = TRUE)
```

R 4.1 or later is required.

## Installing ASReml-R

**ASReml-R is commercial software licensed by
[VSNi](https://vsni.co.uk). It is not on CRAN, it is not bundled with
this package, and it is not installed by any of the commands above.** It
is the only engine this package has: without a working, licensed
ASReml-R installation no model can be fitted.

To use the modelling functions you must

1.  obtain ASReml-R 4.1 or later from VSNi;
2.  install it into the same R library that runs `InterPlotComp`;
3.  activate its licence, in a plain R session, with
    `asreml::asreml.license.activate()`.

`InterPlotComp` never reads, writes, embeds, stores or transmits a
licence key. It uses whatever ASReml installation and already-activated
licence exist in the R process that runs it.

> **Never commit a licence file or activation key** to a shared or
> public repository, and never bake one into a container image. The
> package’s `.gitignore` excludes the usual licence file names as a
> safety net.

## Checking your installation

Everything that does not fit a model works without ASReml, so you can
confirm the package itself is healthy first:

``` r

library(InterPlotComp)

trial <- sample_single_trial()
str(trial)
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

To check whether the modelling engine is available:

``` r

asreml_ready <- requireNamespace("asreml", quietly = TRUE)
asreml_ready
#> [1] FALSE
```

That call is also what performs the licence checkout, so `FALSE` means
either “not installed” or “installed but not licensed”. The package
reports the two cases separately when you try to fit:

``` r

run_app()   # the interface opens either way and says which applies
```

The interface deliberately opens without ASReml so that an installation
or licensing problem is visible immediately, rather than after you have
uploaded a large file and waited for a fit that was never going to run.
Every data panel, field plan and design summary works; only the fitting
buttons are blocked.

## Where to go next

| Vignette | What it covers |
|----|----|
| *Basic workflow* | Preparing a trial and fitting a single-trial model |
| *Advanced workflow* | Relationship matrices, covariates and the MET model |
| *Interpreting the output* | What each number means and when not to trust it |

``` r

vignette("basic-workflow", package = "InterPlotComp")
```
