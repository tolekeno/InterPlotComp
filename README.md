# Inter-plot Competition Analysis for Plant Breeding Trials

A Shiny application that fits **direct–competition mixed models** to unbordered
single-row-plot breeding trials, using **ASReml-R** as its computational engine.
It estimates, for every genotype, the effect it expresses in its own plot, the
effect it imposes on its neighbours, and the value it would express in a pure
stand — in a single trial or across a series of environments.

It implements the analysis of Keno et al. (2026), which showed that inter-plot
competition in hybrid maize multi-environment yield trials in Ethiopia can
reduce the rate of genetic gain
([TAG 139:174](https://doi.org/10.1007/s00122-026-05255-0)).

Version 3.1.0.

---

## ⚠️ ASReml-R licence

**ASReml-R is commercial software licensed by [VSNi](https://vsni.co.uk). This
application supplies no licence of any kind.**

* It uses whatever ASReml-R installation and **already-activated licence** exist
  in the R process that runs it.
* It never reads, writes, embeds, stores, transmits or renews a licence key, and
  it has no fallback engine — without a valid licence, no model can be fitted.
* Install and activate ASReml-R separately, following VSNi's instructions, in
  the same R installation you use to launch the app.
* **Never commit a licence file or activation key** to a shared or public
  repository, and never bundle one into a container image.

The interface opens and lets you inspect data even when ASReml-R is absent, so
that the installation problem is visible immediately rather than after a long
upload.

---

## Installation

R 4.1 or later.

```r
# Required
install.packages(c("shiny", "bslib", "htmltools", "ggplot2", "scales", "DT", "Matrix"))

# Optional — each adds one feature; the app runs without them
install.packages(c("bsicons", "shinyWidgets", "shinycssloaders", "plotly",
                   "thematic", "ragg", "svglite", "ggrepel", "patchwork",
                   "viridisLite", "writexl"))
```

ASReml-R comes from VSNi, not CRAN.

## Running

```r
source("run_app.R")     # checks dependencies, reports what is missing, launches
```

or

```r
shiny::runApp("path/to/this/folder")
```

Run it in the R installation where ASReml-R is licensed. On a machine with
several R versions, using the wrong one is the usual reason the app reports that
ASReml-R is missing — the **Guide → Session** panel shows which R and which
library paths are actually in use.

---

## The model

For plot *i*:

```
y_i = μ + design_i + d_g(i) + Σ_{j ∈ N(i)} c_g(j) + s_i + e_i
```

| Term | Meaning |
|---|---|
| `d` | **Direct effect** — how the genotype performs in its own plot |
| `c` | **Competitive effect** — how much the genotype raises or lowers a neighbouring plot |
| `d + k·c` | **Pure-stand value** — what the genotype expresses when every neighbour is itself (`k` = number of competing neighbours) |
| `s` | AR1 × AR1 separable spatial field trend |

In ASReml-R:

```r
random   = ~ Rep + Block + str(~ Geno + N1 + and(N2), ~ us(2):id(nGeno))
residual = ~ ar1v(Column):ar1(Row)
```

`and()` adds `N2`'s design matrix onto `N1`'s rather than creating new effects,
so both neighbours of a plot draw on one competitive effect vector.
`equate.levels` forces `Geno`, `N1` and `N2` to share a single genotype level
set, so effect *g* in the direct block and effect *g* in the competitive block
are the same genotype. `us(2)` then estimates Var(d), Var(c) and Cov(d, c)
directly.

Border plots legitimately have fewer neighbours. Their absent neighbour factors
stay `NA` and are absorbed by `na.method(x = "include")` as a zero row in the
design matrix — they contribute no competitive effect rather than being dropped.

### Multi-environment model

```r
fixed    = Yield ~ Env
random   = ~ Rep + Block + str(~ Env:Geno + Env:N1 + and(Env:N2),
                               ~ facv(EffectEnv, r):id(nGeno))
residual = ~ dsum(~ ar1v(Column):ar1(Row) | Env)
```

The 2·E direct and competitive environment effects share one joint covariance
matrix. `dsum()` gives each environment its own spatial section, so trials may
differ in size. True `Env:Geno` interaction terms are used rather than a
pre-combined factor, because a pre-combined factor drops the unobserved cells of
a sparse genotype × environment table and then no longer conforms with its
variance structure.

Three structures are offered:

| Structure | Parameters | Assumption |
|---|---|---|
| **Joint factor-analytic** | 2E(r+1) | Direct and competitive effects may have different G×E patterns. Most general. |
| **Separable us(2) × FA** | 3 + E(r+1) | One shared environment correlation pattern. Fits where the joint model is singular. |
| **Diagonal** | 2E | No between-environment correlation. The null model for G×E. |

Pure-stand covariance across environments:

```
G_pure = G_direct + k² G_competition + k (G_dc + G_dc')
```

### Pedigree and genomic relationships

By default genotypes are treated as unrelated. Supplying a relationship matrix
replaces that assumption with the known covariance between relatives, which
helps the **competitive effect most** — it is the harder of the two to estimate
and borrows the most information from family.

| Source | Input | Built with |
|---|---|---|
| **Pedigree** | individual, male parent, female parent | `asreml::ainverse()` |
| **Relationship / kinship matrix** | square matrix, first column = identifiers | supplied directly |
| **Marker matrix** | genotypes × markers, coded 0/1/2 or −1/0/1 | VanRaden genomic relationship |

The model becomes

```r
random = ~ Rep + Block +
  str(~ vm(Geno, K) + vm(N1, K) + and(vm(N2, K)), ~ us(2):vm(Geno, K))
```

> **A quiet wrong-answer trap.** Writing the variance formula as
> `~ us(2):id(n)` while the model formula uses `vm()` is accepted by ASReml,
> converges happily, and silently returns the **independent-genotype** answer —
> the `str()` variance formula overrides the relationship. `vm()` must appear on
> **both** sides. This application never builds the `id()` form when a
> relationship matrix is in use.
>
> ASReml also resolves the second argument of `vm()` from the *calling frame*,
> not from the formula's environment, so the object is held as an ordinary local
> named `.kinship` in the function that calls `asreml()`.

What changes in the output:

* The direct variance becomes an **additive** genetic variance, so the reported
  heritability is narrow-sense rather than entry-mean.
* Individuals in the relationship matrix with no plot in the trial — parents,
  for instance — still receive predicted effects from their relatives, flagged
  *Relative only*. This is how you get **parental breeding values out of a
  progeny trial**.
* Every genotype with an observed plot must appear in the relationship matrix;
  the application refuses to fit rather than quietly dropping the rest.

A genomic relationship matrix is singular whenever there are fewer markers than
genotypes, or duplicated lines, so a small ridge is blended toward the identity
before inversion. The blending weight is adjustable.

For the MET workspace the same substitution applies, with the genotype dimension
of the joint covariance becoming `vm(Geno, K)`.

---

## What the application produces

**Genetic values** — direct, competitive and pure-stand effects per genotype
(per genotype × environment for a MET), with exact standard errors,
reliabilities, predicted pure-stand yield, and rank change between the direct
and pure-stand orderings.

**Variance components** — direct, competitive, their covariance and
correlation, pure-stand variance, spatial and nugget variances, each with its
share of the total and a boundary flag; plus Cullis generalised heritability.

**Model comparison** — the same model refitted without competitive effects, with
AIC, BIC and a likelihood-ratio test, so the question *does competition actually
matter here?* is answered rather than assumed.

**Figures** — direct-vs-competitive scatter with pure-stand contours, ranking
with exact confidence intervals, rank-change slope chart, variance-component
shares, field plans, residual diagnostics, sample variogram, and for a MET:
genetic-correlation heatmaps, environment variances, stability across
environments and factor-analytic fit.

**Exports** — every figure downloads as PNG, PDF, TIFF (LZW), SVG or EPS at a
chosen canvas size and resolution, defaulting to **600 dpi** at journal column
widths. Every table downloads as CSV or Excel, and the whole analysis downloads
as one workbook plus one multi-page vector PDF.

---

## Input data

One row per physical plot:

| Column | Required | Notes |
|---|---|---|
| Response | yes | Plot yield or another continuous trait |
| Genotype | yes | Entry identifier |
| Field row | yes | **Physical** field coordinate, not a spreadsheet line number |
| Field column | yes | **Physical** field coordinate |
| Environment | MET only | Site/year identifier |
| Replicate | no | |
| Block | no | Nested within replicate (and environment) automatically |

A pedigree, kinship matrix or marker file is supplied separately, in the
**Genetic relationship** section of the sidebar. A worked example pedigree
matching both example trials can be downloaded there.

Rules:

* Each Environment–Row–Column combination must identify exactly one plot.
* **Keep failed plots.** Leave the genotype and set the response to blank/`NA` —
  the plot still competes with its neighbours, so deleting the row loses real
  information.
* **Leave gaps as gaps.** Positions absent from the field (alleys, paths) are
  padded internally with a missing response so the spatial grid stays
  rectangular. They carry no genotype and no competitive effect.
* Field axes labelled with integers are expanded to their full span, so an
  entirely absent field row still occupies a level. Without this, AR1 would
  treat the plots either side of it as physically adjacent.

Both workspaces offer a **worked example** simulated from known parameters
(listed in the Guide), so you can confirm the model recovers them before
trusting it on your own data.

---

## Interpreting the output

* **Direct–competition correlation** is usually negative: genotypes that yield
  well in their own plot do so partly at their neighbours' expense. A strongly
  negative value is the clearest evidence that plot yield overstates genetic
  merit.
* **Pure-stand genetic variance** is often much smaller than the direct
  variance, because the two effects partly cancel. It is the variance that
  genuine genetic gain can act on.
* **Reliability** (1 − PEV/σ²_g) above roughly 0.5 means the prediction is
  dependable enough to select on.
* **Rank change** identifies the genotypes that would have been mis-selected on
  plot yield alone. This is the practical payoff of the model.
* **MET genetic correlations**: high positive values mean environments can be
  treated as one selection environment; low or negative values indicate
  crossover interaction. Use the **pure-stand** correlations for monoculture and
  on-farm recommendations.

### When not to trust the result

The data panel warns automatically about most of these:

* Each genotype borders only one or two distinct neighbour genotypes — direct
  and competitive effects are then poorly separated.
* Most plots are on a border, so few records carry a complete neighbour set.
* Fewer than about 15 genotypes contribute observations.
* The direct–competition correlation is pinned at ±1, or a variance sits at a
  boundary (flagged in the variance table). Boundary components have invalid
  standard errors.
* The residual field plan still shows large patches of one colour — spatial
  trend the model has not absorbed may have gone into the competitive effects.
* The application had to fall back to a simpler model. Every fallback is
  reported; read the fitting log before quoting estimates.

---

## Robustness: the simplification ladder

Competition models are prone to singular Average Information matrices. Rather
than failing, or silently using `ai.sing = TRUE`, the application steps down an
ordered ladder of **nested** models, giving up the least defensible assumption
first, and reports exactly which model was fitted and why.

Single trial: `us(2)` → `corgh(2)` → drop nugget → drop the direct–competition
covariance → reduce design terms → drop the spatial residual.

MET: reduce FA rank → drop nugget → separable covariance → diagonal covariance →
reduce design terms → independent residuals.

Only recoverable numerical failures advance the ladder; a genuine data or syntax
error aborts immediately with its own message.

---

## Source layout

```
app.R                 entry point, navigation, theme
run_app.R             launcher with dependency checks
R/00-packages.R       required vs optional dependencies
R/01-utils.R          shared helpers
R/02-theme.R          Bootstrap 5 theme, palette, ggplot theme
R/03-data-prep.R      validation, field indexing, grid padding, neighbours
R/04-simulate.R       worked example data
R/04b-relationship.R  pedigree / genomic relationship matrices
R/05-model-core.R     ASReml interface, variance extraction, exact PEVs
R/06-model-single.R   single-trial model
R/07-model-met.R      multi-environment model
R/08-plots.R          publication-quality figures
R/09-download.R       high-resolution figure and table export
R/10-ui-components.R  reusable interface pieces
R/10b-mod-relationship.R  relationship-matrix input module
R/11-mod-single.R     single-trial workspace
R/12-mod-met.R        multi-environment workspace
R/13-mod-guide.R      guide and about
```

---

## Deployment

Local execution, or an institution-managed Shiny Server, is normally simplest.
A server deployment needs a valid ASReml-R installation and a licence that
permits execution on that server. Commercial ASReml-R binaries generally cannot
be installed automatically by public hosting services.

---

## Licence

This application is released under the MIT Licence (see `LICENSE`).

That covers **this code only**. ASReml-R is separate commercial software
licensed by VSNi under its own terms; the MIT licence here grants no rights to
it whatsoever, and the application cannot compute without a valid ASReml-R
licence that you obtain and activate yourself.

---

## References

* Keno T., Mace E., Godwin I., Jordan D. & Kelly A. (2026) Inter-plot
  competition in hybrid maize multi-environment yield trials in Ethiopia can
  reduce rate of genetic gain. *Theoretical and Applied Genetics* 139:174.
  [doi:10.1007/s00122-026-05255-0](https://doi.org/10.1007/s00122-026-05255-0)
  — the study this application implements.
* Besag J. & Kempton R. (1986) Statistical analysis of field experiments using
  neighbouring plots. *Biometrics* 42:231–251.
* Stringer J.K., Cullis B.R. & Thompson R. (2011) Improved analysis of trials
  with competition. *JABES* 16:269–281.
* Gilmour A.R., Cullis B.R. & Verbyla A.P. (1997) Accounting for natural and
  extraneous variation in the analysis of field experiments. *JABES* 2:269–293.
* Smith A., Cullis B.R. & Thompson R. (2001) Analyzing variety by environment
  data using multiplicative mixed models. *Biometrics* 57:1138–1147.
* Cullis B.R., Smith A.B. & Coombes N.E. (2006) On the design of early
  generation variety trials with correlated data. *JABES* 11:381–393.
* VanRaden P.M. (2008) Efficient methods to compute genomic predictions.
  *Journal of Dairy Science* 91:4414–4423.
