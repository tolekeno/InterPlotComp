# Package index

## Running the application

The Shiny interface.
[`run_app()`](https://tolekeno.github.io/InterPlotComp/reference/run_app.md)
is the usual entry point; the other two are exposed so a Shiny Server or
shinyapps.io deployment can build the application directly.

- [`run_app()`](https://tolekeno.github.io/InterPlotComp/reference/run_app.md)
  : Launch the inter-plot competition application
- [`app_ui()`](https://tolekeno.github.io/InterPlotComp/reference/app_ui.md)
  : User interface for the inter-plot competition application
- [`app_server()`](https://tolekeno.github.io/InterPlotComp/reference/app_server.md)
  : Server logic for the inter-plot competition application

## Preparing a trial

The scripted pipeline, in the order it is used: validate and reshape the
trial, pad the field to a complete rectangle for a spatial residual,
then attach the neighbouring genotypes.

- [`prepare_trial_data()`](https://tolekeno.github.io/InterPlotComp/reference/prepare_trial_data.md)
  : Validate and reshape an uploaded trial into the internal analysis
  layout.
- [`complete_field_grid()`](https://tolekeno.github.io/InterPlotComp/reference/complete_field_grid.md)
  : Pad every environment to its full rectangular grid
- [`add_neighbours()`](https://tolekeno.github.io/InterPlotComp/reference/add_neighbours.md)
  : Attach neighbour-genotype factors to the trial data.

## Fitting the model

Both fitting functions require a licensed ASReml-R installation, which
is commercial software and is not supplied by this package.

- [`fit_single_model()`](https://tolekeno.github.io/InterPlotComp/reference/fit_single_model.md)
  : Fit the single-trial competition model.
- [`fit_met_model()`](https://tolekeno.github.io/InterPlotComp/reference/fit_met_model.md)
  : Fit the multi-environment competition model

## Genetic relationships

Replace the default assumption of independent genotypes with a pedigree,
a kinship matrix or a marker-derived genomic relationship matrix.

- [`build_relationship()`](https://tolekeno.github.io/InterPlotComp/reference/build_relationship.md)
  : Build a relationship object from whichever source the user supplied.

## Diagnostics

Whether the field layout can actually support a competition model. Both
are worth running before a fit, not after.

- [`competition_diagnostics()`](https://tolekeno.github.io/InterPlotComp/reference/competition_diagnostics.md)
  : Diagnostics on how well the competition term is supported by the
  layout.
- [`competition_warnings()`](https://tolekeno.github.io/InterPlotComp/reference/competition_warnings.md)
  : Warnings about a layout that cannot support a competition model

## Figures

Every figure is a ggplot2 object, so it can be themed, combined or saved
like any other.
[`save_figure()`](https://tolekeno.github.io/InterPlotComp/reference/save_figure.md)
scales the text to the export size.

- [`plot_field_map()`](https://tolekeno.github.io/InterPlotComp/reference/plot_field_map.md)
  : Field-plan heatmap of an observed or fitted quantity.
- [`plot_direct_vs_competition()`](https://tolekeno.github.io/InterPlotComp/reference/plot_direct_vs_competition.md)
  : Direct effect against competitive effect.
- [`plot_ranking()`](https://tolekeno.github.io/InterPlotComp/reference/plot_ranking.md)
  : Ranked pure-stand (or direct) effects with exact confidence
  intervals.
- [`plot_rank_change()`](https://tolekeno.github.io/InterPlotComp/reference/plot_rank_change.md)
  : How much does accounting for competition change the selection
  decision?
- [`plot_variance_components()`](https://tolekeno.github.io/InterPlotComp/reference/plot_variance_components.md)
  : Variance components as a share of the total.
- [`plot_residual_diagnostics()`](https://tolekeno.github.io/InterPlotComp/reference/plot_residual_diagnostics.md)
  : Residual diagnostics: fitted values, normal quantiles and
  distribution.
- [`plot_correlation_heatmap()`](https://tolekeno.github.io/InterPlotComp/reference/plot_correlation_heatmap.md)
  : Genetic-correlation heatmap between environments.
- [`plot_environment_variances()`](https://tolekeno.github.io/InterPlotComp/reference/plot_environment_variances.md)
  : Direct, competitive and pure-stand genetic variance in each
  environment.
- [`plot_stability()`](https://tolekeno.github.io/InterPlotComp/reference/plot_stability.md)
  : Stability of pure-stand performance across environments.
- [`plot_met_scatter()`](https://tolekeno.github.io/InterPlotComp/reference/plot_met_scatter.md)
  : Direct against competitive effects, one panel per environment.
- [`save_figure()`](https://tolekeno.github.io/InterPlotComp/reference/save_figure.md)
  : Save a ggplot to file at a chosen size and resolution.

## Worked example data

Simulated from known parameters and from a fixed seed, so a fitted model
can be checked against the values that generated the data.

- [`sample_single_trial()`](https://tolekeno.github.io/InterPlotComp/reference/sample_single_trial.md)
  : Worked example: a single trial with inter-plot competition
- [`sample_met_trial()`](https://tolekeno.github.io/InterPlotComp/reference/sample_met_trial.md)
  : Worked example: a multi-environment trial series
- [`sample_pedigree()`](https://tolekeno.github.io/InterPlotComp/reference/sample_pedigree.md)
  : Worked example: a pedigree for the example trials

## Package

- [`InterPlotComp`](https://tolekeno.github.io/InterPlotComp/reference/InterPlotComp-package.md)
  [`InterPlotComp-package`](https://tolekeno.github.io/InterPlotComp/reference/InterPlotComp-package.md)
  : InterPlotComp: inter-plot competition analysis for plant breeding
  trials
