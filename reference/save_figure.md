# Save a ggplot to file at a chosen size and resolution.

Save a ggplot to file at a chosen size and resolution.

## Usage

``` r
save_figure(
  plot_fun,
  file,
  format = "png",
  width = 18,
  height = 12,
  units = "cm",
  dpi = 600
)
```

## Arguments

- plot_fun:

  A function of one argument, `base_size`, returning a
  [`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html).
  Taking a function rather than a finished plot is what lets the text
  size follow the export width, so a figure saved at 9 cm and the same
  figure at 18 cm both come out legible.

- file:

  Destination path. The format is taken from `format`, not from the file
  extension.

- format:

  One of `"png"`, `"tiff"`, `"pdf"`, `"svg"` or `"eps"`.

- width, height:

  Size in `units`.

- units:

  `"cm"` or `"in"`.

- dpi:

  Resolution for the raster formats; ignored for vector ones.

## Value

The path, invisibly. Called for its side effect of writing the file.

## See also

`figure_base_size()` for how the text size is chosen.

## Examples

``` r
f <- tempfile(fileext = ".png")
save_figure(
  function(base_size) {
    plot_direct_vs_competition(
      data.frame(Genotype = c("A", "B", "C"),
                 Direct_effect = c(0.5, 0, -0.5),
                 Competition_effect = c(-0.2, 0.1, 0.2),
                 Pure_stand_effect = c(0.1, 0.2, -0.1)),
      base_size = base_size)
  },
  file = f, format = "png", width = 12, height = 9, dpi = 150
)
file.exists(f)
#> [1] TRUE
unlink(f)
```
