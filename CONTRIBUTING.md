# Contributing to InterPlotComp

Thank you for taking the time to contribute. This document explains how
to report a problem, how to propose a change, and the one constraint
that makes this package different from most: **the modelling engine is
commercial software that neither you nor the maintainer can
redistribute.**

## The ASReml-R constraint

Every model in this package is fitted by
[ASReml-R](https://vsni.co.uk/software/asreml-r), which is licensed by
VSNi. It is not on CRAN, it is not bundled here, and it cannot be
installed as a dependency.

That has three consequences for contributors:

- **You do not need ASReml to contribute.** Data preparation,
  relationship matrices, plotting, the Shiny interface and all of their
  tests run without it. The test suite skips the model-fitting tests
  cleanly when ASReml is absent or unlicensed (`skip_without_asreml()`
  in `tests/testthat/helper-asreml.R`).
- **If you do have a licence**, please run the full suite before opening
  a pull request, and say so in the description — those tests are the
  only ones that exercise the fitting code.
- **Never commit a licence file or activation key**, and never paste one
  into an issue. `.gitignore` excludes the usual file names as a safety
  net, but it cannot catch everything. The package never reads, writes,
  stores or transmits a licence key, and it must stay that way.

## Reporting a bug

Open an issue at <https://github.com/tolekeno/InterPlotComp/issues>
including:

1.  the output of
    [`sessionInfo()`](https://rdrr.io/r/utils/sessionInfo.html);
2.  the ASReml-R version, if the problem involves fitting
    (`packageVersion("asreml")`);
3.  a **reproducible example**. Please build it from the packaged worked
    examples where you can —
    [`sample_single_trial()`](https://tolekeno.github.io/InterPlotComp/reference/sample_single_trial.md),
    [`sample_met_trial()`](https://tolekeno.github.io/InterPlotComp/reference/sample_met_trial.md)
    and
    [`sample_pedigree()`](https://tolekeno.github.io/InterPlotComp/reference/sample_pedigree.md)
    are deterministic, so an example built from them reproduces exactly
    on the maintainer’s machine:

``` r

library(InterPlotComp)
d  <- prepare_trial_data(sample_single_trial(), map = list(...))
nb <- add_neighbours(complete_field_grid(d), "rows")
fit_single_model(nb$data, nb$names, opts = list(...))
#> the error you saw
```

If the problem only appears with your own data, please describe the
layout (field dimensions, replication, how many genotypes, which
columns) rather than attaching unpublished trial results.

A model that does not converge, or that falls down the simplification
ladder, is usually a property of the data rather than a bug — but please
do report it if the reported reason looks wrong, or if the interface
fails to explain what it gave up.

## Proposing a change

1.  Open an issue first for anything beyond a typo, so the approach can
    be agreed before you write code.
2.  Fork, branch from `main`, and keep one logical change per pull
    request.
3.  Add a test. Every bug fix should come with a test that fails without
    it.
4.  Run the checks below.
5.  Add a bullet to `NEWS.md` under a new heading for the development
    version.

## Development setup

``` r

install.packages(c("devtools", "roxygen2", "testthat", "knitr", "rmarkdown",
                   "pkgdown"))

devtools::load_all()     # load the package from source
devtools::document()     # regenerate NAMESPACE and man/ from roxygen comments
devtools::test()         # run the test suite
devtools::check()        # full R CMD check
```

To run the application straight from a source checkout, without
installing:

``` r

shiny::runApp()          # uses the top-level app.R
```

`R/_disable_autoload.R` stops Shiny auto-sourcing `R/`, so the load
order in `app.R` is authoritative.

## Code style

The existing code is the specification; please match it rather than the
general conventions of any one style guide.

- Two-space indent, no tabs, lines under 80 characters.
- `snake_case` for functions and variables.
- Files in `R/` are numbered to fix the load order. Put new code in the
  file whose subject it belongs to rather than creating a new one,
  unless it is a genuinely new area.
- Every exported function needs a roxygen block with `@param`, `@return`
  and a runnable `@examples` section. Examples must not require ASReml;
  wrap any that would in
  `if (requireNamespace("asreml", quietly = TRUE))` or `\dontrun{}`.
- Internal functions are marked `@noRd`.

### Two conventions worth stating explicitly

**Comments explain *why*, not *what*.** The existing comments record the
traps that were found the hard way — why `id()` must not appear in a
[`str()`](https://rdrr.io/r/utils/str.html) variance formula when a
relationship matrix is in use, why the field grid is padded, why
[`library(asreml)`](https://rdrr.io/r/base/library.html) is not called.
Please write that kind of comment, and please do not delete the ones
that are there.

**Internal names must never reach a user.** `RepF`, `BlockF`, `N1`,
`EffectEnv` are convenient in formulae and must be translated before
they appear in a table, a figure caption or an error message.
`pretty_term()` and `interpret_component()` do this.

## Tests

The suite uses testthat edition 3.

``` r

devtools::test()                              # everything
testthat::test_file("tests/testthat/test-data-prep.R")
```

Tests that fit a model must begin with `skip_without_asreml()`. Tests
that do not fit a model must not need ASReml at all — that division is
what keeps the package contributable without a commercial licence.

## Checking before you submit

``` r

devtools::document()
devtools::test()
devtools::check()                             # expect 0 errors, 0 warnings
```

`R CMD check --as-cran` reports one unavoidable NOTE, that `asreml` in
`Suggests` is not in a mainstream repository. That is inherent to the
package and is not something to fix.

## Code of conduct

Please note that this project is released with a [Contributor Code of
Conduct](https://tolekeno.github.io/InterPlotComp/CODE_OF_CONDUCT.md).
By participating you agree to abide by its terms.

## Licence

Contributions are accepted under the [MIT
Licence](https://tolekeno.github.io/InterPlotComp/LICENSE.md) that
covers the project.
