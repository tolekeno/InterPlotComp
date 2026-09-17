# InterPlotComp 3.2.0

* Restructured as an installable R package. Install with
  `remotes::install_github("tolekeno/InterPlotComp")` and launch with
  `run_app()`.
* Exported an analysis API so a workflow can be scripted rather than clicked:
  `prepare_trial_data()`, `complete_field_grid()`, `add_neighbours()`,
  `build_relationship()`, `fit_single_model()`, `fit_met_model()`, and the
  worked-example generators `sample_single_trial()`, `sample_met_trial()` and
  `sample_pedigree()`.
* `app_ui()` and `app_server()` are exported for Shiny Server and shinyapps.io
  deployment; `inst/app/app.R` is the deployment entry point.
* The application version shown in the navigation bar is now read from
  DESCRIPTION, so the two cannot drift.

# InterPlotComp 3.1.0

* Pedigree and genomic relationship matrices via `vm()` and `ainverse()`, from
  a pedigree, a kinship matrix or a marker matrix (VanRaden). Non-phenotyped
  relatives receive predicted effects, flagged "Relative only", which is how
  parental breeding values are obtained from a progeny trial.
* The direct variance becomes additive when a relationship matrix is supplied,
  so the reported heritability is narrow-sense rather than entry-mean.

# InterPlotComp 3.0.0

* Replaced the `facv(DirectCompetition, 1)` genetic covariance with `us(2)`:
  three parameters rather than four for a 2 x 2 matrix, no synthetic factor,
  and no longer over-parameterised.
* Exact pure-stand standard errors, reliabilities and Cullis generalised
  heritability from the inverse coefficient matrix.
* Gaps in integer field coordinates are filled, so an absent field row no
  longer makes AR1 treat the plots either side of it as adjacent.
* Baseline no-competition model with AIC, BIC and a REML likelihood-ratio test.
* Ordered simplification ladder that advances only on recoverable numerical
  failures, and reports exactly which model was fitted.
* Rebuilt interface on bslib with publication-quality figure export to PNG,
  PDF, TIFF, SVG and EPS at a chosen canvas size and resolution.
