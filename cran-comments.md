# cran-comments.md

## Test environments

* Windows 11, R 4.6.1 (local)
* GitHub Actions: Windows, Ubuntu (R devel, release, oldrel-1)

**Not yet checked on macOS.** The macOS job reported "R CMD check found ERRORs"
while all other platforms passed, and it was removed from the matrix rather
than diagnosed. This must be resolved before submission, because CRAN runs its
own macOS checks.

## R CMD check results

0 errors | 0 warnings | 1 note

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

## Before submitting

`R CMD check --as-cran` also reports the pkgdown URL
`https://tolekeno.github.io/InterPlotComp/` as a 404. **Enable GitHub Pages for
the repository (Settings to Pages, source `gh-pages`) and let the `pkgdown`
workflow run once.** The URL resolves after that, and the NOTE disappears. If
the site is not going to be published, remove the second URL from `DESCRIPTION`
and the documentation link from `README.md` instead.

## Downstream dependencies

None; this is a new submission.
