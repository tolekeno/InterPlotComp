# InterPlotComp: inter-plot competition analysis for plant breeding trials

Fits direct-competition mixed models to unbordered single-row-plot
breeding trials. For every genotype the model separates the effect
expressed in its own plot (the *direct* effect) from the effect it
imposes on neighbouring plots (the *competitive* effect), and combines
them into the value the genotype would express in a pure stand.

## Getting started

[`run_app()`](https://tolekeno.github.io/InterPlotComp/reference/run_app.md)
launches the interface. To script an analysis instead, the pipeline is
[`prepare_trial_data()`](https://tolekeno.github.io/InterPlotComp/reference/prepare_trial_data.md),
then
[`complete_field_grid()`](https://tolekeno.github.io/InterPlotComp/reference/complete_field_grid.md)
for a spatial model, then
[`add_neighbours()`](https://tolekeno.github.io/InterPlotComp/reference/add_neighbours.md),
then
[`fit_single_model()`](https://tolekeno.github.io/InterPlotComp/reference/fit_single_model.md)
or
[`fit_met_model()`](https://tolekeno.github.io/InterPlotComp/reference/fit_met_model.md).
[`build_relationship()`](https://tolekeno.github.io/InterPlotComp/reference/build_relationship.md)
supplies a pedigree or genomic relationship matrix.
[`sample_single_trial()`](https://tolekeno.github.io/InterPlotComp/reference/sample_single_trial.md),
[`sample_met_trial()`](https://tolekeno.github.io/InterPlotComp/reference/sample_met_trial.md)
and
[`sample_pedigree()`](https://tolekeno.github.io/InterPlotComp/reference/sample_pedigree.md)
generate worked examples from known parameters.

## ASReml-R licence

ASReml-R is commercial software licensed by VSNi. This package is a
front end: it uses whatever ASReml-R installation and already-activated
licence exist in the R process that runs it, and it never reads, writes,
embeds, stores or transmits a licence key. Without a valid licence no
model can be fitted, and there is no fallback engine.

## See also

Useful links:

- <https://github.com/tolekeno/InterPlotComp>

- <https://tolekeno.github.io/InterPlotComp/>

- Report bugs at <https://github.com/tolekeno/InterPlotComp/issues>

## Author

**Maintainer**: Tolera Keno <tolekeno@gmail.com> \[copyright holder\]

Authors:

- Tolera Keno <tolekeno@gmail.com> \[copyright holder\]
