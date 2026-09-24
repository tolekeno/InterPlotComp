# Build a relationship object from whichever source the user supplied.

Build a relationship object from whichever source the user supplied.

## Usage

``` r
build_relationship(type, raw = NULL, map = NULL, blend = 0.01)
```

## Arguments

- type:

  one of RELATIONSHIP_SOURCES

- raw:

  uploaded data frame (pedigree, matrix or markers)

- map:

  column mapping for a pedigree

- blend:

  identity blending weight for genomic matrices

## Value

list(ginv, ids, type, label, diagnostics, matrix)
