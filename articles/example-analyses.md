# Example analyses

``` r

library(InterPlotComp)
```

Three worked analyses, each answering a question a breeder actually
asks. The worked example data are simulated from known parameters, so
every fit can be checked against the values that generated it —
something a real data set can never offer.

| Parameter                      | Simulated value                    |
|--------------------------------|------------------------------------|
| Direct genetic variance        | 0.36                               |
| Competitive genetic variance   | 0.09                               |
| Direct–competition correlation | −0.55                              |
| Block variance                 | 0.05                               |
| Spatial variance               | 0.22 (AR1 0.60 rows, 0.35 columns) |
| Nugget variance                | 0.06                               |
| Grand mean                     | 8.2                                |

## Analysis 1 — Does competition change who I select?

``` r

nb <- add_neighbours(
  complete_field_grid(
    prepare_trial_data(sample_single_trial(),
                       list(yield = "Yield_t_ha", geno = "Genotype",
                            row = "Row", column = "Column",
                            rep = "Rep", block = "Block"))),
  axis = "rows")
```

``` r

fit <- fit_single_model(
  nb$data, nb$names,
  opts = list(structure = "us", spatial = TRUE, nugget = TRUE,
              auto_simplify = TRUE, exact_se = TRUE, cinv_limit = 5000L,
              maxit = 30L, workspace = "1gb", compare_baseline = TRUE,
              max_rounds = 15L))

g <- fit$genetic
```

**Did the model recover what generated the data?**

``` r

data.frame(
  Parameter = c("Direct variance", "Competitive variance",
                "Direct-competition correlation"),
  Simulated = c(0.36, 0.09, -0.55),
  Estimated = round(c(fit$components$direct, fit$components$competition,
                      fit$components$correlation), 3)
)
```

**Would selecting on the trial mean have been a mistake?**

``` r

top_naive <- head(g$Genotype[order(-g$Direct_effect)], 10)
top_pure  <- head(g$Genotype[order(-g$Pure_stand_effect)], 10)

length(intersect(top_naive, top_pure))          # how many of the top 10 survive
setdiff(top_naive, top_pure)                    # selected on aggression
setdiff(top_pure,  top_naive)                   # missed by the naive ranking
```

The entries in `setdiff(top_naive, top_pure)` are the ones the trial
flattered: their yield advantage was partly taken from their neighbours.

``` r

plot_direct_vs_competition(g, k = fit$k)
```

**Is it statistically worth it?**

``` r

fit$comparison$lrt
```

## Analysis 2 — Which genotypes are the aggressors?

The competitive effect is the quantity no conventional analysis produces
at all. A negative value means the genotype suppresses its neighbours.

``` r

aggressors <- g[order(g$Competition_effect), ]
head(aggressors[, c("Genotype", "Direct_effect", "Competition_effect",
                    "Pure_stand_effect", "Competitor_type")], 8)
```

The genotypes to worry about are those that are **high on direct effect
and strongly negative on competitive effect** — the ones that look best
in the trial for the worst reason:

``` r

flattered <- g[g$Direct_effect > stats::quantile(g$Direct_effect, 0.75) &
                 g$Competition_effect < stats::quantile(g$Competition_effect, 0.25), ]
flattered[, c("Genotype", "Direct_effect", "Competition_effect",
              "Rank_direct", "Rank_pure_stand", "Rank_change")]
```

Conversely, genotypes with a **positive** competitive effect are good
neighbours; they are under-rated by the trial and their rank improves
once competition is modelled.

``` r

good_neighbours <- g[order(-g$Competition_effect), ]
head(good_neighbours[, c("Genotype", "Competition_effect", "Rank_change")], 5)
```

## Analysis 3 — Is the effect consistent across environments?

``` r

met_nb <- add_neighbours(
  complete_field_grid(
    prepare_trial_data(
      sample_met_trial(),
      list(yield = "Yield_t_ha", geno = "Genotype", row = "Row",
           column = "Column", env = "Environment", rep = "Rep",
           block = "Block"),
      multi_env = TRUE)),
  axis = "rows")
```

``` r

met_fit <- fit_met_model(
  met_nb$data, met_nb$names,
  opts = list(structure = "facv", rank = 1L, spatial = TRUE, nugget = FALSE,
              auto_simplify = TRUE, exact_se = FALSE, cinv_limit = 0L,
              maxit = 25L, workspace = "2gb", compare_baseline = FALSE,
              max_rounds = 10L))

met_fit$variance
```

**Does competition bite equally hard everywhere?**

``` r

plot_environment_variances(met_fit$variance)
```

**Do the environments agree about which genotypes are good?**

``` r

round(met_fit$matrices$direct_cor, 3)
```

The worked example was built with a shared genetic core plus
environment-specific deviation, so the environments should come out
genuinely but incompletely correlated — which is exactly the situation
the factor-analytic model exists to describe.

``` r

plot_stability(met_fit$values, top_n = 10)
```

**Selecting across the series**

``` r

mean_pure <- stats::aggregate(Pure_stand_effect ~ Genotype,
                              data = met_fit$values, FUN = mean)
head(mean_pure[order(-mean_pure$Pure_stand_effect), ], 10)
```

## Exporting results

``` r

write.csv(fit$genetic,  "genotype_values.csv",     row.names = FALSE)
write.csv(fit$variance, "variance_components.csv", row.names = FALSE)

save_figure(function(bs) plot_direct_vs_competition(fit$genetic, fit$k,
                                                    base_size = bs),
            file = "direct_vs_competition.png",
            format = "png", width = 18, height = 12, dpi = 600)
```

The Shiny interface exposes all of this through download buttons,
including a multi-sheet Excel workbook and a multi-page PDF of every
figure:

``` r

run_app()
```
