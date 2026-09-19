# InterPlotComp 3.7.1

## Figure text size

* **Exported figures were set too small.** The reference size is raised from
  11 pt to 14 pt at the 18 cm double-column width, which puts axis tick labels
  near 12 pt and captions near 11 pt - readable at full size, and still
  readable after the shrink a figure takes on its way into a manuscript or a
  slide. The secondary ratios in the shared theme were lifted with it, so tick
  labels, legends and captions no longer fall so far below the base size.

* **The size now grows with the square root of the canvas width, not linearly.**
  A figure is printed at the width it is exported at, so its type has to hold
  an absolute size on the page. Under the old linear rule a 9 cm single-column
  figure was given 7 pt type; every preset now sits in a 10-17 pt band.

* **Captions and subtitles wrap to the canvas, not to a fixed character
  count.** A fixed count is correct at one width only: at 110 characters a
  caption set for an 18 cm figure ran straight off a 9 cm one. The measure now
  follows the type size.

* Each panel of the 2 x 2 residual-diagnostics grid is half the width of the
  canvas and now takes the type size that width earns, instead of the
  full-canvas size that pushed two of its titles off the page.

* Guide titles sit above their key rather than beside it, so a horizontal
  colour bar plus a long title is no longer wider than a single-column canvas.

* **The on-screen preview is now an export at its own width**, computed with
  the same rule, so what is on screen is in the same proportion as the file
  that downloads. It was previously a fixed 12 pt at an unrelated resolution.

# InterPlotComp 3.7.0

## Interface

* The whole interface has been rebuilt on an explicit design system. Every
  colour is now emitted once as a CSS custom property, so the stylesheet, the
  Bootstrap theme and the figures cannot drift apart.

* **A dark mode.** The toggle sits at the right of the navigation bar. It is a
  second, deliberately chosen palette rather than an automatic inversion: each
  step was re-picked against the dark surface and re-checked for contrast
  (headings 14.6:1, body text 10.6:1, help text 6.4:1). Figures deliberately
  keep their light ground in both modes, because they are exported for print
  and the preview must match the exported file.

* **Typography.** A 15 px base on a 1.20 type scale, the platform UI face
  rather than a downloaded web font (the application must run offline), tighter
  optical letter-spacing on headings, and tabular lining figures in tables and
  model summaries so digits align column-wise. Metric values keep proportional
  figures, which read better at display size.

* **Layout and spacing** now come from one 4 px scale, applied consistently to
  cards, the sidebar, the accordion, tabs, buttons, inputs and tables. Metric
  strips reflow on a CSS grid, the interface degrades cleanly to phone width,
  keyboard focus is always visible, and `prefers-reduced-motion` and print
  stylesheets are honoured.

* Empty workspaces now show what to do next instead of a blank results card.

## Figures

* **The palette is now computed rather than chosen.** The three effect series,
  the sequential ramp and the diverging ramp were stepped in OKLab and checked
  against a lightness band, a chroma floor, a colour-vision-deficiency
  separation target after Machado-Oliveira-Fernandes (2009) protan/deutan
  simulation, a normal-vision floor and a contrast floor. The previous series
  green and blue read as grey (chroma 0.093, below the 0.10 floor); the
  replacements separate by dE 12.3 under deuteranopia.

* **Diverging scales are now centred on zero.** The direct-versus-competitive
  scatter, its multi-environment counterpart and the rank-change slope chart
  previously stretched the ramp over the observed range, which put the neutral
  colour at the middle of the data rather than at "no effect" - so a set of
  entirely positive effects came out half blue.

* The rank-change chart no longer uses red-to-green poles, the one pairing a
  deuteranope cannot resolve. Strokes take a higher-contrast variant of the
  ramp, so a line at "no change" no longer vanishes into the page.

* The stability chart no longer gives each of twelve genotypes its own hue.
  The highest-ranked few are drawn in the series colours and directly labelled;
  the rest form a recessive backdrop that still shows the spread and the
  crossovers.

* The ranking chart takes one series colour instead of a ramp that merely
  repeated its own x axis, and its rank axis no longer offers a rank 0.

* Correlation-heatmap labels take their colour from the luminance of the step
  they sit on, so a value can no longer be drawn as white text on a pale tile.
  Scatter markers gained a surface-coloured ring, which separates overlapping
  points far better than blanket transparency.

* Recessive grid and axis furniture, wider figure margins and consistent legend
  placement throughout.

* `viridisLite` is no longer a suggested dependency.

# InterPlotComp 3.6.0

* The fit now continues until ASReml reports convergence, restarting from the
  current estimates as many rounds as needed rather than stopping at the
  iteration limit. The number of extra rounds is reported. A round limit
  remains so a non-convergent model cannot run forever, and reaching it is
  stated rather than hidden.
* Fixed effects now carry a **Wald test**: a conditional F-test with computed
  denominator degrees of freedom, significance marks and a plain `Retain`
  column, so it is clear whether a covariate should stay in the model.
* "Competition adjustment trait" is renamed **covariate** throughout the
  interface, the outputs, the exported workbooks and the documentation.
* Genetic correlation heatmaps no longer draw the diagonal: it is 1 by
  definition and only anchored the colour scale on a value never in question.

# InterPlotComp 3.5.0

* Optional **covariate**. A measured proxy for interference
  (plant height, canopy width, root vigour) can be named in the column mapping;
  the neighbouring plots' centred values are fitted as a fixed covariate, so
  competition attributable to the covariate is removed before the genetic
  competitive effects are estimated. The focal plot's own value is available
  too, off by default. The default remains a single-trait model.
* The **Variance & heritability** tab now shows heritabilities: Cullis
  generalised heritability, accuracy, and how many genotypes clear a reliability
  of 0.5, for the direct and the pure-stand value, per environment in the MET.
* Genotypes are ranked on **predicted pure-stand performance** rather than on
  the pure-stand effect, and the ranking figure is drawn on the yield scale.
* With a relationship matrix, evaluated genotypes and relatives predicted from
  the matrix are reported in **separate tables**, with separate downloads and
  workbook sheets.
* The simulated MET environments are named Env01 to Env04, and both worked
  examples carry a simulated plant-height column so the trait adjustment can be
  tried immediately.

# InterPlotComp 3.4.0

* New MET genetic structure, *Separate fa() per effect*, fitting an ordinary
  `fa(Env, r)` term to the direct and competitive effects instead of one
  joint `facv()` covariance. `fa()` cannot be used inside the joint `str()`
  block because it augments the term with its own latent-factor levels, so
  the joint structure has to be given up to use it.
* The direct-competition covariance is a structural zero under that
  structure, not an estimate. The results banner, the variance table and the
  model description all say so, because the pure-stand variance then omits
  the 2k Cov(D, C) term and is overstated wherever the two effects are
  negatively correlated - by 1.3x to 8.7x on the worked example.
* The joint factor-analytic structure remains the default: on the worked
  example it fits 10.2 log-likelihood units better for the same number of
  parameters.

# InterPlotComp 3.3.0

* The separable spatial residual is written `ar1(Column):ar1(Row)` rather
  than `ar1v(Column):ar1(Row)`, so the residual variance is reported as
  ASReml's `sigma2` instead of a structure parameter with `sigma2` pinned at
  1. The fit is unchanged: identical log-likelihood and parameter count, for
  a single trial and for a `dsum()` MET alike, where each environment still
  receives its own residual scale.
* Variance components are now read from `summary()$varcomp`, which is on the
  variance scale, rather than from `fit$vparameters`, which holds ratios to
  `sigma2` whenever `sigma2` is estimated. Prediction error variances are read
  from `Cinv` without rescaling, which was already correct. Both were latent
  no-ops while the residual pinned `sigma2` at 1; without them the move to
  `ar1(Column)` would have silently rescaled every genetic variance,
  heritability and reliability.

* The residual process on each field axis is now selectable: AR1 (default),
  AR2, SAR, SAR2 or independent. A second-order process on the competition axis
  accommodates the negative lag-1 residual correlation that interference
  induces, which AR1 cannot represent and which otherwise leaks into the
  competitive effects.
* The simplification ladder drops a second-order process back to AR1 x AR1
  before giving up any other term, and every later step inherits the simplified
  process so the ladder stays a sequence of nested models.
* The single-trial workspace can filter one site out of a multi-site file: name
  the site column under Column mapping and choose the site.

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
