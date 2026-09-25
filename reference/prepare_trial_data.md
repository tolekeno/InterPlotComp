# Validate and reshape an uploaded trial into the internal analysis layout.

Validate and reshape an uploaded trial into the internal analysis
layout.

## Usage

``` r
prepare_trial_data(raw, map, multi_env = FALSE)
```

## Arguments

- raw:

  data frame as uploaded

- map:

  named list of source column names: yield, geno, row, column and
  optionally env, rep, block

- multi_env:

  TRUE for the MET workspace

## Value

data frame with the internal analysis columns, carrying a
`field_summary` attribute
