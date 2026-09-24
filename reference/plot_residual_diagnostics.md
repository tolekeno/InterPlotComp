# Residual diagnostics: fitted values, normal quantiles and distribution.

Residual diagnostics: fitted values, normal quantiles and distribution.

## Usage

``` r
plot_residual_diagnostics(res, base_size = 12, caption = NULL)
```

## Arguments

- res:

  The residual table from \`fit_single_model()\$residuals\` or
  \`fit_met_model()\$residuals\`, holding at least \`Fitted\`,
  \`Residual\` and \`Std_residual\`.

- base_size:

  Base font size in points. \[save_figure()\] chooses this from the
  export width; pass it explicitly only when composing a figure by hand.

- caption:

  Optional caption placed under the figure, wrapped to the plot width.

## Value

A \[ggplot2::ggplot()\] object.

## Details

Four panels are drawn when patchwork is installed; without it the
residual-versus-fitted panel is returned on its own.

## Examples

``` r
set.seed(1)
res <- data.frame(Fitted = rnorm(60, 8, 0.5), Residual = rnorm(60, 0, 0.3),
                  Row = rep(1:10, 6), Column = rep(1:6, each = 10))
res$Std_residual <- res$Residual / sd(res$Residual)
plot_residual_diagnostics(res)
```
