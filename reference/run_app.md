# Launch the inter-plot competition application

Starts the 'Shiny' interface for fitting direct-competition mixed models
to single-row-plot breeding trials.

## Usage

``` r
run_app(
  launch.browser = interactive(),
  port = NULL,
  host = "127.0.0.1",
  max_upload_mb = 250,
  quiet = FALSE,
  ...
)
```

## Arguments

- launch.browser:

  Open the application in the default browser.

- port:

  Port to listen on. \`NULL\` lets Shiny choose a free one.

- host:

  Interface to bind to. The default binds to the loopback address only,
  so the application is not exposed to the network.

- max_upload_mb:

  Largest file that may be uploaded, in megabytes.

- quiet:

  Suppress the start-up report of missing optional packages.

- ...:

  Further arguments passed to \[shiny::runApp()\].

## Value

Called for its side effect of running the application. Does not return
until the application is stopped.

## Details

Model fitting requires 'ASReml-R', which is commercial software licensed
by VSNi and is not supplied by this package. The interface opens and
data can be inspected without it, so a missing installation is visible
immediately rather than after a long upload; but no model can be fitted
until 'ASReml-R' is installed and licensed in the same R installation
that runs the application.

## Examples

``` r
# Launch the interface (interactive sessions only):
if (interactive()) {
  run_app()
}
```
