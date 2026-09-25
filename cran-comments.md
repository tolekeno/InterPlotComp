# cran-comments.md

## Submission

This is a new submission of InterPlotComp 3.8.1. Version 3.8.0 was never
submitted; 3.8.1 adds only bug fixes on top of it (see NEWS.md).

## Test environments

* Windows 11, R 4.6.1 (local)
* GitHub Actions: Windows, macOS, Ubuntu (R devel, release, oldrel-1)

## R CMD check results

0 errors | 0 warnings | 1 note

The package website at <https://tolekeno.github.io/InterPlotComp/> is live, so
the only NOTE is the one below.

## The one remaining NOTE

```
Suggests or Enhances not in mainstream repositories:
  asreml
```

This is expected and cannot be resolved. **ASReml-R is commercial software
licensed by VSNi.** It is distributed directly to licensees and is not
available from CRAN or from any public repository, so no
`Additional_repositories` field can be supplied.

The package has been written so that this does not affect a CRAN check:

* `asreml` is in `Suggests`, never in `Imports` or `Depends`.
* It is loaded lazily, inside `load_asreml()`, only when a model is actually
  fitted. The package loads, the Shiny interface opens, and every data
  preparation, diagnostic and plotting function works without it.
* Its namespace is loaded with `loadNamespace()`; the package never attaches
  it with `library()` and never calls `:::` on it.
* **No example, test or vignette requires it.** Every one of them is guarded:
  tests call `skip_without_asreml()` (which checks both installation and
  licence), and vignette chunks that fit a model carry
  `eval = requireNamespace("asreml", quietly = TRUE)`. A machine without
  ASReml runs the entire suite and builds every vignette; it simply skips the
  fitting.

## Licensing

The package neither contains nor requires an ASReml licence key. It uses
whatever ASReml installation and already-activated licence exist in the R
process that runs it, and it never reads, writes, embeds, stores or transmits
a licence key. This is documented in the DESCRIPTION, the package-level help,
the README and the installation vignette.

## Downstream dependencies

None; this is a new submission.
