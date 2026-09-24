test_that("app_ui assembles without a running Shiny session", {
  ui <- app_ui()
  expect_true(inherits(ui, c("shiny.tag", "shiny.tag.list", "bslib_page")))

  html <- paste(as.character(htmltools::renderTags(ui)$html), collapse = "\n")
  expect_match(html, "Inter-plot Competition")
  # The three workspaces must all reach the navigation bar.
  expect_match(html, "Single trial")
  expect_match(html, "Multi-environment")
  expect_match(html, "Guide")
})

test_that("the UI states the ASReml-R version in its navigation bar", {
  html <- paste(as.character(htmltools::renderTags(app_ui())$html), collapse = "\n")
  expect_match(html, "ASReml-R engine")
  expect_match(html, APP_VERSION, fixed = TRUE)
})

test_that("each module UI builds on its own", {
  for (f in list(single_ui, met_ui, guide_ui, relationship_ui)) {
    expect_silent(ui <- f("test_namespace"))
    html <- paste(as.character(htmltools::renderTags(ui)$html), collapse = "\n")
    # Inputs must be namespaced, or two workspaces would collide.
    expect_match(html, "test_namespace-")
  }
})

test_that("app_server is a well-formed Shiny server function", {
  expect_type(app_server, "closure")
  expect_named(formals(app_server), c("input", "output", "session"))
})

test_that("run_app exposes safe defaults", {
  f <- formals(run_app)
  expect_equal(eval(f$host), "127.0.0.1")      # loopback, not 0.0.0.0
  expect_equal(eval(f$max_upload_mb), 250)
  expect_null(eval(f$port))
  expect_false(eval(f$quiet))
})

test_that("the required/optional package split is coherent", {
  expect_true(all(REQUIRED_PACKAGES %in% rownames(utils::installed.packages())))
  expect_false(any(names(OPTIONAL_PACKAGES) %in% REQUIRED_PACKAGES))
  # ASReml belongs to neither list: it is loaded lazily, so the interface can
  # open and explain itself when ASReml is absent.
  expect_false("asreml" %in% REQUIRED_PACKAGES)
  expect_false("asreml" %in% names(OPTIONAL_PACKAGES))
})

test_that("check_required_packages passes in a working installation", {
  expect_true(check_required_packages())
})

test_that("optional_package_status reports one row per optional package", {
  s <- optional_package_status()
  expect_equal(nrow(s), length(OPTIONAL_PACKAGES))
  expect_named(s, c("Package", "Enables", "Installed"))
  expect_type(s$Installed, "logical")
  expect_false(anyNA(s$Enables))
})

test_that("asreml_installed does not itself trigger the licence check", {
  # It must answer "is it installed?", not "is it licensed?", so that a
  # licence failure is reported as a licence failure.
  expect_type(asreml_installed(), "logical")
  expect_equal(asreml_installed(), nzchar(system.file(package = "asreml")))
})

test_that("asreml_status describes the installation without fitting anything", {
  s <- asreml_status()
  expect_named(s, c("ok", "level", "title", "detail"))
  expect_type(s$ok, "logical")
  expect_match(s$detail, "licence|licenс|Install", ignore.case = TRUE)
})

test_that("load_asreml gives an actionable message when ASReml is absent", {
  skip_if(nzchar(system.file(package = "asreml")),
          "ASReml-R is installed here, so the absent-install path cannot run")
  expect_error(load_asreml(), "not installed")
})

test_that("dt_table renders a placeholder rather than failing on empty input", {
  expect_s3_class(dt_table(NULL), "datatables")
  expect_s3_class(dt_table(data.frame()), "datatables")
  expect_s3_class(dt_table(data.frame(a = numeric(0))), "datatables")
})

test_that("dt_table survives a numeric column with one distinct value", {
  # A degenerate range slider throws in noUiSlider and aborts the draw,
  # leaving a visible but empty table.
  df <- data.frame(Genotype = sprintf("G%02d", 1:20), Constant = 1,
                   Value = stats::rnorm(20))
  expect_s3_class(dt_table(df), "datatables")
})

test_that("dt_table replaces underscores in the displayed column names", {
  tab <- dt_table(data.frame(Direct_effect = 1:3, Pure_stand_effect = 4:6))
  expect_true("Direct effect" %in% names(tab$x$data))
})

test_that("stamped builds a timestamped file name with the right extension", {
  nm <- stamped("interplot_results", "xlsx")
  expect_match(nm, "^interplot_results_[0-9]{8}_[0-9]{4}\\.xlsx$")
})

test_that("save_figure writes each supported raster format", {
  p <- function(bs) {
    ggplot2::ggplot(data.frame(x = 1:5, y = 1:5), ggplot2::aes(x, y)) +
      ggplot2::geom_point() + theme_trial(bs)
  }
  for (fmt in c("png", "pdf")) {
    f <- withr::local_tempfile(fileext = paste0(".", fmt))
    save_figure(p, f, format = fmt, width = 12, height = 8, dpi = 100)
    expect_true(file.exists(f))
    expect_gt(file.size(f), 0)
  }
})

test_that("save_figure rejects a non-ggplot and an unknown format", {
  expect_error(save_figure(function(bs) 42, tempfile(), format = "png"),
               "Only ggplot figures")
  p <- function(bs) ggplot2::ggplot(data.frame(x = 1), ggplot2::aes(x)) +
    ggplot2::geom_bar()
  expect_error(save_figure(p, tempfile(), format = "bmp"),
               "Unsupported export format")
})

test_that("relationship_blocking_message only blocks a requested relationship", {
  # No relationship asked for: nothing to block.
  expect_null(relationship_blocking_message("none", NULL))
  # Asked for, but not yet built: the fit must not proceed silently.
  expect_type(relationship_blocking_message("pedigree", NULL), "character")
  # Asked for and built: clear to proceed.
  expect_null(relationship_blocking_message("pedigree", list(ids = letters)))
})

test_that("run_app validates its arguments before starting Shiny", {
  # shiny::runApp() blocks forever once it starts listening, so a regression in
  # the validation below would hang the whole suite rather than fail it. The
  # mock turns that into an immediate, legible failure.
  testthat::local_mocked_bindings(
    runApp = function(...) stop("run_app() reached shiny::runApp()"),
    .package = "shiny"
  )

  # A bad value must be named here, not surface from inside Shiny against an
  # internal argument the caller never set.
  expect_error(run_app(max_upload_mb = 0), "max_upload_mb")
  expect_error(run_app(max_upload_mb = -1), "max_upload_mb")
  expect_error(run_app(max_upload_mb = c(1, 2)), "max_upload_mb")
  expect_error(run_app(max_upload_mb = "big"), "max_upload_mb")

  expect_error(run_app(port = 0), "port")
  expect_error(run_app(port = 70000), "port")
  expect_error(run_app(port = "8080"), "port")

  expect_error(run_app(host = ""), "host")
  expect_error(run_app(host = c("a", "b")), "host")
})

test_that("figure_devices offers a fallback for every raster and vector format", {
  for (fmt in c("png", "tiff", "pdf", "eps")) {
    d <- figure_devices(fmt)
    expect_gt(length(d), 0L)
    expect_true(all(vapply(d, is.function, logical(1))), info = fmt)
    expect_false(anyDuplicated(names(d)) > 0)
  }
  # svg has no guaranteed device: both candidates are conditional.
  expect_type(figure_devices("svg"), "list")
  expect_error(figure_devices("bmp"), "Unsupported export format")
})

test_that("save_figure reports every device it tried when none can write", {
  # A device that opens nothing reproduces the macOS cairo_pdf failure, where
  # capabilities("cairo") is TRUE but no file ever appears.
  local_mocked_bindings(
    figure_devices = function(format) {
      list(`fake::silent` = function(filename, ...) {
        grDevices::pdf(file = nullfile())
      })
    }
  )
  p <- function(bs) ggplot2::ggplot(data.frame(x = 1, y = 1), ggplot2::aes(x, y)) +
    ggplot2::geom_point()
  f <- withr::local_tempfile(fileext = ".png")
  expect_error(save_figure(p, f, format = "png"), "Could not write a png figure")
  expect_error(save_figure(p, f, format = "png"), "fake::silent")
  expect_false(file.exists(f))
})
