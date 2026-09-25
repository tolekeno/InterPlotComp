# Attach neighbour-genotype factors to the trial data.

Each neighbour column N1..Nk holds the genotype growing in the adjacent
plot, as a factor sharing the genotype level set. Absent neighbours
(field borders, padded positions, unplanted plots) stay NA and are
absorbed by `na.method(x = "include")` as a zero row in the design
matrix.

## Usage

``` r
add_neighbours(d, axis = "rows")
```

## Arguments

- d:

  prepared trial data

- axis:

  one of "rows", "columns", "four"

## Value

list(data, names, k, label)
