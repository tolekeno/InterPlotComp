# Interpreting the output

``` r

library(InterPlotComp)
```

Fitting the model is the easy part. This vignette is about reading it:
what each quantity means, how the pieces relate, and — the section that
matters most — when the answer should not be trusted.

## The three genotype effects

For genotype *i* the model estimates a direct effect $`D_i`$ and a
competitive effect $`C_i`$. With $`k`$ neighbours per plot, the
pure-stand effect is

``` math
P_i = D_i + k\,C_i
```

because in a pure stand the genotype is its own neighbour on all $`k`$
sides.

| Quantity | Meaning | Sign convention |
|----|----|----|
| `Direct_effect` | What the genotype expresses in its own plot | Higher is better |
| `Competition_effect` | What it does to a neighbouring plot | **Positive is better**: it *raises* its neighbour |
| `Pure_stand_effect` | What it expresses with neighbours like itself | Higher is better |

The sign convention on the competitive effect is the one people get
wrong. A **negative** competitive effect means the genotype *suppresses*
its neighbours — it is the aggressive competitor, and it is flattered by
the trial.

``` r

g <- fit$genetic
head(g[, c("Genotype", "Direct_effect", "Competition_effect",
           "Pure_stand_effect", "Competitor_type")], 8)
```

## The variance components

``` r

fit$variance[, c("Component", "Estimate", "Interpretation")]
```

The pure-stand variance is not a free parameter; it follows from the
other three:

``` math
\sigma^2_P = \sigma^2_D + k^2\sigma^2_C + 2k\,\sigma_{DC}
```

``` r

cmp <- fit$components
c(reported  = cmp$pure,
  recomputed = cmp$direct + fit$k^2 * cmp$competition + 2 * fit$k * cmp$covariance)
```

### The direct–competition correlation

This single number carries most of the biology.

- **Negative** — high-yielding genotypes suppress their neighbours. Part
  of their measured advantage was taken from the plots they are compared
  against, and will not carry into a pure stand or a farmer’s field.
  This is the usual finding, and the reason to fit the model at all.
- **Near zero** — the two effects are unrelated; competition adds noise
  but not bias to the ranking.
- **Positive** — unusual; check the field layout and the neighbour axis
  before believing it.

``` r

cmp$correlation
```

## Reliability and heritability

``` r

fit$heritability
```

`direct` is generalised heritability on the direct effect; `pure` is the
same quantity on the pure-stand value. **The pure-stand figure is almost
always the lower of the two**, and that is not a defect of the model —
it is the honest statement that the pure-stand value is a derived
quantity combining two estimates and their covariance, so it carries
more uncertainty than either.

Per-genotype reliability is on the usual 0–1 scale:

``` r

summary(g$Reliability_direct)
```

## Does the ranking actually change?

This is the question the whole analysis exists to answer.

``` r

summary(g$Rank_change)
sum(abs(g$Rank_change) >= 5)
```

``` r

plot_rank_change(g, top_n = 25)
```

If nothing moves, competition is present but harmless for selection, and
you should say so. If entries move substantially, the trial as recorded
was selecting partly on aggression.

## Was competition worth modelling?

``` r

fit$comparison$table
fit$comparison$lrt
```

A likelihood-ratio test against the identical model without competitive
effects. Both models are iterated to convergence and share the same
fixed and residual structure, so the test isolates the competition term.

Note that testing a variance component at its boundary makes the
ordinary chi-squared p-value **conservative** — the real p-value is
smaller than the one reported, so a significant result stays
significant.

## Model diagnostics

``` r

plot_residual_diagnostics(fit$residuals)
```

``` r

fit$fit_stats
fit$converged
fit$convergence_rounds
```

`convergence_rounds` counts how many restarts were needed after the
first fit ran out of iterations. A model that merely stopped iterating
is not safe to quote, so the fit keeps restarting from the current
estimates until ASReml reports convergence.

## When not to trust the result

Read the fitting log first. Every one of these is reported by the
package itself, in the interface and in the returned object.

**1. The model fell down the simplification ladder.**

``` r

fit$fallback_used
fit$log
```

If the direct–competition covariance was constrained to zero, you did
not measure it — you assumed it. Report which model you actually fitted.

**2. Too few plots have a complete neighbour set.** The competitive
effect is then estimated mainly from interior plots:

``` r

diag_nb <- competition_diagnostics(nb$data, nb$names)
diag_nb[c("full_neighbour_pct", "mean_neighbours", "border_plots")]
#> $full_neighbour_pct
#> [1] 85.22727
#> 
#> $mean_neighbours
#> [1] 1.852273
#> 
#> $border_plots
#> [1] 26
```

**3. Each genotype sits beside too few distinct neighbours.** If a
genotype is nearly always beside the same neighbour, the direct and
competitive effects are weakly separated and the design cannot tell them
apart:

``` r

diag_nb$mean_distinct_neighbours
#> [1] 5.166667
```

**4. The genotype panel is small.** Below roughly 15 genotypes the
genetic variances, and especially the direct–competition covariance, are
too imprecise to interpret.

``` r

competition_warnings(diag_nb)
#> character(0)
```

An empty character vector means none of these applies.

**5. A component is at its boundary.** A variance pinned at zero is a
constraint that bound, not an estimate:

``` r

fit$varcomp[fit$varcomp$At_boundary, c("Component", "Estimate", "Bound")]
```

**6. The competition direction does not match the layout.**
[`add_neighbours()`](https://tolekeno.github.io/InterPlotComp/reference/add_neighbours.md)
will happily use whichever axis you name. For single-row plots,
competition is along the field rows; using `"columns"` there estimates
something real but not what you meant.

## Reporting

A defensible methods paragraph states:

- the model actually fitted, including anything the ladder gave up
  (`fit$description` and `fit$log`);
- the residual structure (`fit$spec$spatial`, and the AR1 correlations
  from `fit$varcomp`);
- the estimated variance components with standard errors;
- the direct–competition correlation;
- whether competition improved the fit (`fit$comparison$lrt`);
- whether, and by how much, the ranking changed.

``` r

fit$description
```
