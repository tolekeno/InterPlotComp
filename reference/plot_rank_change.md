# How much does accounting for competition change the selection decision?

A slope chart between the direct-effect ranking and the pure-stand
ranking. Large crossings are the practical payoff of the competition
model: those genotypes would have been mis-selected on direct effects
alone.

## Usage

``` r
plot_rank_change(genetic, top_n = 25, base_size = 12, caption = NULL)
```

## Arguments

- genetic:

  The genotype table from \`fit_single_model()\$genetic\`, or any data
  frame with the same columns.

- top_n:

  Number of genotypes to show, taken from the top of the pure-stand
  ranking.

- base_size:

  Base font size in points. \[save_figure()\] chooses this from the
  export width; pass it explicitly only when composing a figure by hand.

- caption:

  Optional caption placed under the figure, wrapped to the plot width.

## Value

A \[ggplot2::ggplot()\] object.

## Examples

``` r
g <- data.frame(
  Genotype = sprintf("MZ%03d", 1:8),
  Rank_direct = c(1, 2, 3, 4, 5, 6, 7, 8),
  Rank_pure_stand = c(4, 1, 6, 2, 8, 3, 5, 7)
)
g$Rank_change <- g$Rank_direct - g$Rank_pure_stand
plot_rank_change(g)
```
