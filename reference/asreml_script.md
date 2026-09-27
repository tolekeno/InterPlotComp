# Write the fitted model as a stand-alone ASReml-R script

Produces an R script that rebuilds the analysis data with the package's
own preparation functions, applies the same field-trend covariates and
the same outlier removal, and calls `asreml()` with the formulae that
were actually fitted - the model the simplification ladder settled on,
not merely the one requested. Running it reproduces the fit outside the
interface.

## Usage

``` r
asreml_script(
  result,
  data_file = NULL,
  map = NULL,
  axis = "rows",
  sep = ",",
  header = TRUE,
  site = NULL,
  file = NULL
)
```

## Arguments

- result:

  A result from
  [`fit_single_model()`](https://tolekeno.github.io/InterPlotComp/reference/fit_single_model.md)
  or
  [`fit_met_model()`](https://tolekeno.github.io/InterPlotComp/reference/fit_met_model.md).

- data_file:

  Path of the data file, written into the script. Leave `NULL` for a
  placeholder to edit.

- map:

  The column mapping passed to
  [`prepare_trial_data()`](https://tolekeno.github.io/InterPlotComp/reference/prepare_trial_data.md),
  written into the script. Leave `NULL` for a placeholder.

- axis:

  The competition direction passed to
  [`add_neighbours()`](https://tolekeno.github.io/InterPlotComp/reference/add_neighbours.md).

- sep, header:

  Field separator of the data file, and whether its first line holds the
  column names.

- site:

  For a single trial taken from a multi-site file, a list with the site
  `column` and the `value` analysed; the script filters to it.

- file:

  Optional path to write the script to.

## Value

The script as a character vector of lines, invisibly when `file` is
given.
