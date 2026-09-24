# Direct effect against competitive effect.

The central figure of a competition analysis. The quadrants separate the
genotypes a breeder cares about: high direct effect with a benign
competitive effect is an unambiguously good selection, whereas high
direct effect with an aggressive competitive effect means part of the
apparent advantage was taken from its neighbours and will not carry into
a pure stand or a farmer's field.

## Usage

``` r
plot_direct_vs_competition(
  genetic,
  k = 2,
  label_n = 12,
  base_size = 12,
  caption = NULL
)
```

## Arguments

- genetic:

  The genotype table from \`fit_single_model()\$genetic\`, or any data
  frame with the same columns.

- k:

  Number of neighbours per plot, from \`fit_single_model()\$k\`. It sets
  the weight on the competitive effect in the pure-stand value.

- label_n:

  Number of top genotypes to label.

- base_size:

  Base font size in points. \[save_figure()\] chooses this from the
  export width; pass it explicitly only when composing a figure by hand.

- caption:

  Optional caption placed under the figure, wrapped to the plot width.

## Value

A \[ggplot2::ggplot()\] object.

## See also

\[plot_rank_change()\] for the consequence of this figure for selection.

## Examples

``` r
g <- data.frame(
  Genotype = sprintf("MZ%03d", 1:12),
  Direct_effect = c(0.8, 0.6, 0.5, 0.3, 0.2, 0, -0.1, -0.3, -0.4, -0.6, -0.7, -0.9),
  Competition_effect = c(-0.3, 0.1, -0.2, 0.2, -0.1, 0.05, 0.1, -0.05, 0.15,
                         0.2, -0.1, 0.25)
)
g$Pure_stand_effect <- g$Direct_effect + 2 * g$Competition_effect
plot_direct_vs_competition(g, k = 2)
```
