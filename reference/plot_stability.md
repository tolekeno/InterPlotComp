# Stability of pure-stand performance across environments.

A parallel-coordinates view of the selection candidates. Lines that stay
parallel indicate stable genotypes; lines that cross indicate crossover
genotype-by-environment interaction and therefore environment-specific
recommendations.

## Usage

``` r
plot_stability(values, top_n = 12, base_size = 12, caption = NULL)
```

## Arguments

- values:

  The genotype-by-environment table from \`fit_met_model()\$values\`,
  holding \`Genotype\`, \`Environment\`, \`Pure_stand_effect\` and
  \`Status\`.

- top_n:

  Number of genotypes to trace, taken from the top of the
  across-environment mean.

- base_size:

  Base font size in points. \[save_figure()\] chooses this from the
  export width; pass it explicitly only when composing a figure by hand.

- caption:

  Optional caption placed under the figure, wrapped to the plot width.

## Value

A \[ggplot2::ggplot()\] object.

## Examples

``` r
values <- expand.grid(Genotype = sprintf("MZ%02d", 1:6),
                      Environment = paste0("Env0", 1:3),
                      stringsAsFactors = FALSE)
set.seed(2)
values$Pure_stand_effect <- rnorm(nrow(values), 0, 0.4)
values$Status <- "Estimable"
plot_stability(values, top_n = 6)
```
