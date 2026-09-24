# User interface for the inter-plot competition application

Assembles the navigation bar, theme and the three workspaces. Exposed
mainly so that a Shiny Server or 'shinyapps.io' deployment can build the
interface directly; most users call \[run_app()\] instead.

## Usage

``` r
app_ui()
```

## Value

A \[shiny::tagList()\]-compatible UI definition.

## Examples

``` r
ui <- app_ui()
class(ui)
#> [1] "bslib_page"     "shiny.tag.list" "list"          
```
