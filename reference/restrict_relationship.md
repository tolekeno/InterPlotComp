# Restrict a relationship matrix to the genotypes in a trial

Genotypes that appear only in the relationship matrix - with no plot in
the trial - are predicted from their relatives only when the
relationship comes from a **pedigree**. A pedigree defines those
individuals: parents and ancestors are what connect the tested
genotypes, and their predicted values are a recognised product of a
pedigree analysis. A kinship or marker matrix, by contrast, often covers
a whole germplasm panel of which the trial is a small part, and
predicting every untested line from it is not something the user asked
for. For those sources the matrix is cut down to the trial's genotypes
and inverted again; the block of the relationship matrix is taken, never
the block of its inverse, which would be a different relationship.

## Usage

``` r
restrict_relationship(rel, ids)
```

## Arguments

- rel:

  a relationship object from
  [`build_relationship()`](https://tolekeno.github.io/InterPlotComp/reference/build_relationship.md).

- ids:

  the genotypes in the trial. Duplicates and missing values are ignored,
  so the genotype and neighbour columns of the trial data can be passed
  as they are.

## Value

`rel` unchanged for a pedigree; otherwise a relationship object over the
listed genotypes that occur in it, in the matrix's own order.

## Examples

``` r
K <- diag(4) + 0.25
dimnames(K) <- list(paste0("G", 1:4), paste0("G", 1:4))
raw <- data.frame(ID = rownames(K), K, check.names = FALSE)
rel <- build_relationship("kinship", raw)
restrict_relationship(rel, c("G1", "G3", NA, "G3"))$ids
#> [1] "G1" "G3"
```
