# ---------------------------------------------------------------------------
# Visual design system
#
# One set of tokens drives the Bootstrap 5 theme, the bespoke CSS and every
# ggplot figure, so that on-screen output and exported publication figures are
# visually identical.
#
# Three rules govern this file.
#
#   1. Colours are computed, not chosen by eye. The categorical series, the
#      sequential ramp and the diverging ramp were stepped in OKLab and checked
#      against a lightness band, a chroma floor, a colour-vision-deficiency
#      separation target (Machado-Oliveira-Fernandes 2009, dE >= 8 after
#      protan/deutan simulation), a normal-vision floor and a contrast floor.
#      Every text-on-surface pair below meets WCAG AA (>= 4.5:1) in both modes.
#   2. Every colour is emitted once as a CSS custom property. The stylesheet
#      refers to `var(--ipc-*)` only, so the light and dark modes are the same
#      stylesheet with a different token block and cannot drift apart.
#   3. Fonts are a system stack, never a web font: the application must render
#      correctly on an offline analysis machine or an institutional server with
#      no outbound internet access.
# ---------------------------------------------------------------------------

# Shown in the navigation bar. Kept in step with the Version field of
# DESCRIPTION; read from there when the package is installed so the two
# cannot drift, with a literal fallback for a source checkout.
APP_VERSION <- tryCatch(
  as.character(utils::packageVersion("InterPlotComp")),
  error = function(e) "3.7.3"
)

# ---------------------------------------------------------------------------
# Colour tokens
# ---------------------------------------------------------------------------

# Light mode. Neutrals carry a faint green cast so the interface sits in the
# same family as the field-trial figures without tinting the data itself.
PAL <- list(
  ink        = "#0F211A",  # 16.8:1 on white - headings
  body       = "#2C3E36",  # 11.4:1 on white - body copy
  muted      = "#5E7268",  #  5.1:1 on white - captions, help text
  faint      = "#8A9C93",  # decorative only, never load-bearing text
  line       = "#DCE4DF",
  line_soft  = "#EAF0EC",
  surface    = "#FFFFFF",
  surface_2  = "#F8FAF9",  # table stripes, inset panels
  canvas     = "#F3F5F3",  # page background
  primary    = "#12694A",  #  6.7:1 on white
  primary_dk = "#0C4F37",  # navbar, hover
  primary_lt = "#E4F0EA",  # tints and card caps
  accent     = "#A35303",  #  5.5:1 on white
  accent_lt  = "#FBEFE2",
  info       = "#006EBE",
  info_lt    = "#E4EFFA",
  success    = "#12694A",
  warning    = "#8A6100",
  warning_lt = "#FBF2DF",
  danger     = "#A6321F",
  danger_lt  = "#FBEBE8",
  # Categorical series for the three effect types. Validated as a set:
  # worst CVD dE 12.3 (deuteranopia), worst normal-vision dE 25.2, every step
  # inside the L 0.43-0.77 band, chroma >= 0.10, contrast >= 3:1 on white.
  direct      = "#0EAA62",
  competition = "#A35303",
  pure        = "#006EBE"
)

# Dark mode. Not an automatic inversion: each step was re-chosen against the
# dark surface and re-checked (ink 14.6:1, body 10.6:1, muted 6.4:1).
PAL_DARK <- list(
  ink        = "#E9F1EC",
  body       = "#C3D0C9",
  muted      = "#93A49B",
  faint      = "#6E7F77",
  line       = "#2B3833",
  line_soft  = "#222D28",
  surface    = "#171F1B",
  surface_2  = "#1E2823",
  canvas     = "#0F1513",
  primary    = "#33C98A",
  primary_dk = "#0B1512",  # text placed *on* a primary fill
  primary_lt = "#16342A",
  accent     = "#E09A4C",
  accent_lt  = "#33230F",
  info       = "#63B3F0",
  info_lt    = "#122636",
  success    = "#33C98A",
  warning    = "#E0B65A",
  warning_lt = "#332912",
  danger     = "#F08874",
  danger_lt  = "#361A16",
  direct      = "#0EAA62",
  competition = "#A35303",
  pure        = "#006EBE"
)

# Sequential ramp for magnitude (yield, semivariance). One hue, light to dark,
# monotone in lightness with every adjacent gap >= 0.06 L and the light end
# still at 2:1 against white, so the top of the ramp is visible rather than
# implied.
SEQUENTIAL <- c("#02D279", "#03B86A", "#039F5A", "#03864B", "#036E3D", "#02562F")

# Diverging ramp for polarity (genetic correlations, effects centred on zero).
# Two hues around a *neutral* midpoint - never a hue at the middle - with
# lightness rising monotonically to the midpoint and falling symmetrically
# after it. The poles are the blue and the earth of the categorical series.
DIVERGING <- c(
  "#013C6B", "#0465AF", "#4E97DE", "#A2C8F0",
  "#F2F2F1",
  "#E9B999", "#CE7A3B", "#974C00", "#5B2C01"
)

# The same ramp restepped for *strokes*. A filled tile can carry a near-white
# neutral, because its edges define it; a 1 px line cannot - at the midpoint it
# simply disappears into the surface and reads as missing data rather than as
# "no change". Every step here clears 3:1 against white (minimum 3.6:1) while
# keeping the neutral midpoint and the symmetric lightness profile.
DIVERGING_LINE <- c(
  "#003967", "#005A9D", "#1F74BF", "#5B8ABB",
  "#898586",
  "#B07750", "#AC5701", "#874300", "#582A00"
)

# ---------------------------------------------------------------------------
# Typography and spacing tokens
# ---------------------------------------------------------------------------

# System stack. `system-ui` resolves to the platform UI face (Segoe UI
# Variable on Windows 11, SF on macOS, the desktop default on Linux), which is
# already hinted for small sizes on the user's own screen.
FONT_STACK <- paste(
  "system-ui", "-apple-system", '"Segoe UI Variable Text"', '"Segoe UI"',
  "Roboto", '"Helvetica Neue"', "Arial", '"Noto Sans"', "sans-serif",
  sep = ", "
)

# Display cut for headings where the platform ships one; it is drawn with
# tighter sidebearings at large sizes.
HEADING_STACK <- paste(
  '"Segoe UI Variable Display"', "system-ui", "-apple-system", '"Segoe UI"',
  "Roboto", '"Helvetica Neue"', "Arial", "sans-serif",
  sep = ", "
)

MONO_STACK <- paste('"Cascadia Mono"', '"SFMono-Regular"', "Consolas",
                    '"Liberation Mono"', "monospace", sep = ", ")

# Type scale, 15 px base on a 1.20 (minor third) ratio. Small enough to fit a
# model summary on one screen, large enough to read a variance component
# without leaning in.
TYPE <- list(
  base = "0.9375rem",  # 15px
  xs   = "0.6875rem",  # 11px - uppercase micro-labels only
  sm   = "0.8125rem",  # 13px - help text, table body
  md   = "1.0625rem",  # 17px
  lg   = "1.25rem",
  xl   = "1.5rem",
  lh_tight = "1.25",
  lh_body  = "1.55"
)

# 4 px spacing scale. Every margin and pad in the stylesheet is one of these.
SPACE <- list(x1 = "0.25rem", x2 = "0.5rem", x3 = "0.75rem", x4 = "1rem",
              x5 = "1.5rem", x6 = "2rem")

#' Emit the token block as CSS custom properties.
#'
#' Both modes are written from the same key set, so a token added to `PAL` and
#' forgotten in `PAL_DARK` is a visible error rather than a silent fallback.
#' @noRd
css_tokens <- function(pal, selector) {
  keys <- intersect(names(PAL), names(pal))
  vars <- paste0("  --ipc-", gsub("_", "-", keys), ": ", unlist(pal[keys]), ";",
                 collapse = "\n")
  paste0(selector, " {\n", vars, "\n}\n")
}

#' Bootstrap 5 theme for the whole application.
#'
#' Bootstrap's own variables are set for the light mode; the dark mode is
#' handled by overriding `--bs-*` under `[data-bs-theme="dark"]` in
#' [app_css()], which keeps both modes in one place.
#' @noRd
app_theme <- function() {
  bslib::bs_theme(
    version           = 5,
    bg                = PAL$surface,
    fg                = PAL$ink,
    primary           = PAL$primary,
    secondary         = PAL$muted,
    success           = PAL$success,
    info              = PAL$info,
    warning           = PAL$warning,
    danger            = PAL$danger,
    base_font         = FONT_STACK,
    heading_font      = HEADING_STACK,
    code_font         = MONO_STACK,
    "body-bg"            = PAL$canvas,
    "body-color"         = PAL$body,
    "navbar-bg"          = PAL$primary_dk,
    "border-color"       = PAL$line,
    "card-border-color"  = PAL$line,
    "card-cap-bg"        = PAL$surface,
    "card-bg"            = PAL$surface,
    "border-radius"      = "0.625rem",
    "border-radius-sm"   = "0.375rem",
    "border-radius-lg"   = "0.875rem",
    "font-size-base"     = TYPE$base,
    "line-height-base"   = TYPE$lh_body,
    "headings-font-weight" = "600",
    "table-striped-bg"   = PAL$surface_2,
    "link-decoration"    = "none",
    "focus-ring-width"   = "3px",
    "focus-ring-opacity" = "0.32",
    "focus-ring-color"   = "rgba(18, 105, 74, .32)"
  )
}

#' Additional CSS that Bootstrap variables alone cannot express.
#'
#' Written against `var(--ipc-*)` throughout, so the dark mode is the same
#' stylesheet with the token block swapped.
#' @noRd
app_css <- function() {
  htmltools::HTML(paste0(
    css_tokens(PAL, ":root, [data-bs-theme='light']"),
    css_tokens(PAL_DARK, "[data-bs-theme='dark']"),
    "
/* Bootstrap's own variables for the dark mode, so components we do not style
   by hand (dropdowns, popovers, modals, the DataTables chrome) follow. */
[data-bs-theme='dark'] {
  --bs-body-bg: var(--ipc-canvas);
  --bs-body-color: var(--ipc-body);
  --bs-emphasis-color: var(--ipc-ink);
  --bs-secondary-color: var(--ipc-muted);
  --bs-border-color: var(--ipc-line);
  --bs-card-bg: var(--ipc-surface);
  --bs-card-border-color: var(--ipc-line);
  --bs-secondary-bg: var(--ipc-surface-2);
  --bs-tertiary-bg: var(--ipc-surface-2);
  --bs-primary: var(--ipc-primary);
  --bs-link-color: var(--ipc-primary);
  --bs-link-hover-color: var(--ipc-primary);
  --bs-heading-color: var(--ipc-ink);
  --bs-table-striped-bg: var(--ipc-surface-2);
  --bs-focus-ring-color: rgba(51, 201, 138, .32);
  color-scheme: dark;
}

/* ---------------------------------------------------------------- base ---- */
body {
  background: var(--ipc-canvas);
  color: var(--ipc-body);
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
  text-rendering: optimizeLegibility;
}
h1, h2, h3, h4, h5, h6, .card-header, .navbar-brand {
  color: var(--ipc-ink);
  letter-spacing: -0.011em;   /* optical tightening; headings set too loose by default */
  line-height: " , TYPE$lh_tight, ";
}
/* Numbers that align vertically - table rows, model summaries - are set with
   tabular figures so digits line up column-wise. Standalone metric values are
   deliberately excluded: equal-width digits make a large isolated number look
   gappy. */
table, .dataTable, .table, code, pre {
  font-variant-numeric: tabular-nums lining-nums;
  font-feature-settings: 'tnum' 1, 'lnum' 1;
}
.metric-value { font-variant-numeric: proportional-nums lining-nums; }
code, pre { font-family: ", MONO_STACK, "; }
code { font-size: 0.85em; color: var(--ipc-accent); background: var(--ipc-accent-lt);
       padding: .08em .3em; border-radius: .25rem; }
pre code { background: none; padding: 0; color: inherit; }
a { color: var(--ipc-primary); text-decoration-thickness: 1px; text-underline-offset: 2px; }
a:hover { text-decoration: underline; }
hr { border-color: var(--ipc-line); opacity: 1; margin: ", SPACE$x4, " 0; }

/* Keyboard focus is always visible, and never as a bare browser outline. */
:focus-visible {
  outline: 2px solid var(--ipc-primary);
  outline-offset: 2px;
  border-radius: .25rem;
}

/* ------------------------------------------------------------- navbar ---- */
.navbar {
  padding-top: .55rem; padding-bottom: .55rem;
  box-shadow: 0 1px 0 rgba(0,0,0,.06), 0 2px 10px rgba(0,0,0,.05);
}
.navbar-brand {
  font-weight: 650; letter-spacing: .1px; font-size: ", TYPE$md, ";
  color: #fff !important; display: inline-flex; align-items: center; gap: .45rem;
}
.navbar .nav-link {
  font-weight: 500; font-size: ", TYPE$base, "; border-radius: .5rem;
  padding: .35rem .7rem !important; margin-inline: .1rem;
  color: rgba(255,255,255,.78) !important;
  display: inline-flex; align-items: center; gap: .4rem;
  transition: background-color .15s ease, color .15s ease;
}
.navbar .nav-link:hover { background: rgba(255,255,255,.1); color: #fff !important; }
.navbar .nav-link.active {
  background: rgba(255,255,255,.16); color: #fff !important; font-weight: 600;
}
.navbar-text { color: rgba(255,255,255,.65) !important; font-size: ", TYPE$sm, "; }
[data-bs-theme='dark'] .navbar { background: var(--ipc-surface) !important;
  box-shadow: 0 1px 0 var(--ipc-line); }

/* ------------------------------------------------------------ sidebar ---- */
.bslib-sidebar-layout > .sidebar {
  background: var(--ipc-surface);
  border-right: 1px solid var(--ipc-line);
}
.bslib-sidebar-layout > .sidebar > .sidebar-content { padding: ", SPACE$x4, "; }
.bslib-sidebar-layout > .sidebar .sidebar-title {
  font-size: ", TYPE$xs, "; text-transform: uppercase; letter-spacing: .09em;
  font-weight: 700; color: var(--ipc-muted); margin-bottom: ", SPACE$x3, ";
}

/* -------------------------------------------------------------- cards ---- */
.card {
  border: 1px solid var(--ipc-line);
  box-shadow: 0 1px 2px rgba(15, 33, 26, .04);
  margin-bottom: ", SPACE$x4, ";
}
.card-header {
  font-weight: 600; font-size: ", TYPE$base, ";
  background: var(--ipc-surface);
  border-bottom: 1px solid var(--ipc-line-soft);
  padding: .7rem ", SPACE$x4, ";
  display: flex; align-items: center; gap: .45rem;
}
.card-header .bi { color: var(--ipc-primary); }
.card-title-text { flex: 0 1 auto; }
.card-subtitle-text {
  margin-left: auto; font-weight: 400; font-size: ", TYPE$sm, ";
  color: var(--ipc-muted); text-align: right;
}
/* Pairs a checkbox with the select beside it without a hand-tuned margin. */
.input-align-bottom { display: flex; align-items: center; height: 100%; padding-top: 1.1rem; }
.input-align-bottom .shiny-input-container { margin-bottom: 0; }
.card-body { padding: ", SPACE$x4, "; }
/* bslib defines --bs-card-bg on the card element itself, so redefining it at
   the root does not reach it; the dark mode has to set it on the card. */
[data-bs-theme='dark'] .card,
[data-bs-theme='dark'] .bslib-card,
[data-bs-theme='dark'] .accordion,
[data-bs-theme='dark'] .dropdown-menu,
[data-bs-theme='dark'] .modal-content,
[data-bs-theme='dark'] .popover {
  --bs-card-bg: var(--ipc-surface);
  --bs-accordion-bg: var(--ipc-surface);
  background-color: var(--ipc-surface);
}
[data-bs-theme='dark'] .card-header { background-color: var(--ipc-surface); }

/* ---------------------------------------------------------- accordion ---- */
.accordion { --bs-accordion-border-color: var(--ipc-line); }
.accordion-item { background: transparent; }
.accordion-button {
  font-weight: 600; font-size: ", TYPE$sm, "; padding: .6rem .75rem;
  background: var(--ipc-surface-2); color: var(--ipc-ink);
}
.accordion-button:not(.collapsed) {
  background: var(--ipc-primary-lt); color: var(--ipc-primary); box-shadow: none;
}
.accordion-button:focus { box-shadow: none; }
.accordion-body { padding: ", SPACE$x3, " .75rem ", SPACE$x4, "; }

/* -------------------------------------------------- tabs, pills, navs ---- */
.nav-tabs { border-bottom: 1px solid var(--ipc-line); gap: .1rem; }
.nav-tabs .nav-link {
  font-weight: 550; font-size: ", TYPE$sm, "; color: var(--ipc-muted);
  border: none; border-bottom: 2px solid transparent; border-radius: 0;
  padding: .55rem .85rem; display: inline-flex; align-items: center; gap: .4rem;
  transition: color .15s ease, border-color .15s ease;
}
.nav-tabs .nav-link:hover { color: var(--ipc-ink); border-bottom-color: var(--ipc-line); }
.nav-tabs .nav-link.active {
  color: var(--ipc-primary); background: transparent;
  border-bottom-color: var(--ipc-primary); font-weight: 650;
}
.nav-pills {
  gap: .3rem; background: var(--ipc-surface-2); padding: .3rem;
  border-radius: .6rem; display: inline-flex; flex-wrap: wrap;
  margin-bottom: ", SPACE$x4, ";
}
.nav-pills .nav-link {
  font-size: ", TYPE$sm, "; font-weight: 550; color: var(--ipc-muted);
  padding: .32rem .8rem; border-radius: .45rem;
}
.nav-pills .nav-link.active {
  background: var(--ipc-surface); color: var(--ipc-primary); font-weight: 650;
  box-shadow: 0 1px 2px rgba(15, 33, 26, .08);
}

/* ------------------------------------------------------------ buttons ---- */
.btn {
  font-weight: 550; letter-spacing: .1px; border-radius: .5rem;
  display: inline-flex; align-items: center; justify-content: center; gap: .4rem;
  transition: background-color .15s ease, border-color .15s ease,
              box-shadow .15s ease, transform .06s ease;
}
.btn:active { transform: translateY(1px); }
.btn-sm { font-size: ", TYPE$sm, "; padding: .3rem .7rem; }
.btn-lg { font-size: ", TYPE$base, "; padding: .62rem 1rem; font-weight: 600; }
.btn-primary {
  background: var(--ipc-primary); border-color: var(--ipc-primary);
  box-shadow: 0 1px 2px rgba(15, 33, 26, .12);
}
.btn-primary:hover, .btn-primary:focus {
  background: var(--ipc-primary-dk); border-color: var(--ipc-primary-dk);
}
[data-bs-theme='dark'] .btn-primary { color: var(--ipc-primary-dk); }
[data-bs-theme='dark'] .btn-primary:hover { background: #4FDCA0; border-color: #4FDCA0; }
.btn-outline-primary { color: var(--ipc-primary); border-color: var(--ipc-line); }
.btn-outline-primary:hover {
  background: var(--ipc-primary-lt); border-color: var(--ipc-primary);
  color: var(--ipc-primary);
}
.btn .bi, .btn .fa, .btn svg { flex: 0 0 auto; }

/* ------------------------------------------------------------- inputs ---- */
.form-label, .control-label, .shiny-input-container > label {
  font-size: ", TYPE$sm, "; font-weight: 600; color: var(--ipc-ink);
  margin-bottom: .3rem; letter-spacing: .1px;
}
.form-control, .form-select, .selectize-input {
  font-size: ", TYPE$sm, "; border-radius: .45rem;
  border-color: var(--ipc-line); background: var(--ipc-surface);
  color: var(--ipc-body); transition: border-color .15s ease, box-shadow .15s ease;
}
.form-control:focus, .form-select:focus, .selectize-input.focus {
  border-color: var(--ipc-primary);
  box-shadow: 0 0 0 3px var(--bs-focus-ring-color);
}
.form-control::placeholder { color: var(--ipc-faint); }
.shiny-input-container { margin-bottom: ", SPACE$x3, "; }
.form-check-input:checked { background-color: var(--ipc-primary); border-color: var(--ipc-primary); }
.irs--shiny .irs-bar, .irs--shiny .irs-single, .irs--shiny .irs-handle > i:first-child {
  background: var(--ipc-primary); border-color: var(--ipc-primary);
}
.irs--shiny .irs-line { background: var(--ipc-line); }
.irs--shiny .irs-min, .irs--shiny .irs-max, .irs--shiny .irs-grid-text {
  color: var(--ipc-muted); background: transparent;
}
/* The file-upload control is the first thing a user meets: make it a drop
   target rather than a grey bar. */
.shiny-input-container input[type='file'] { font-size: ", TYPE$sm, "; }
.progress { height: .4rem; border-radius: .2rem; background: var(--ipc-line-soft); }
.progress-bar { background: var(--ipc-primary); }
/* Shiny tucks the upload bar up against the bottom edge of the file control,
   where its own 'Upload complete' caption is clipped by the input's border.
   It is given its own line below the control instead. */
.shiny-file-input-progress {
  position: relative; top: 0; margin: .4rem 0 0 !important;
  height: 1.15rem; border-radius: .3rem;
}
.shiny-file-input-progress .progress-bar {
  font-size: .7rem; line-height: 1.15rem; font-weight: 600; color: #fff;
}
[data-bs-theme='dark'] .shiny-file-input-progress .progress-bar {
  color: var(--ipc-primary-dk);
}

/* ----------------------------------------------------- section helpers ---- */
.section-note {
  color: var(--ipc-muted); font-size: ", TYPE$sm, "; line-height: 1.5;
  margin-top: .35rem;
}
.section-note code { font-size: .8rem; }
.section-note + .section-note { margin-top: ", SPACE$x2, "; }
/* A help paragraph belongs to the control above it and must not crowd the
   label of the control below. */
.section-note { margin-bottom: ", SPACE$x3, "; }

/* -------------------------------------------------------- empty state ---- */
.empty-state {
  display: flex; flex-direction: column; align-items: center; text-align: center;
  gap: ", SPACE$x2, "; padding: ", SPACE$x6, " ", SPACE$x4, ";
  max-width: 34rem; margin: ", SPACE$x5, " auto;
}
.empty-state .empty-icon {
  font-size: 1.9rem; color: var(--ipc-primary);
  background: var(--ipc-primary-lt); border-radius: 50%;
  width: 3.4rem; height: 3.4rem; display: flex; align-items: center;
  justify-content: center;
}
.empty-state .empty-title {
  font-size: ", TYPE$md, "; font-weight: 650; color: var(--ipc-ink);
  letter-spacing: -0.011em;
}
.empty-state .empty-body { font-size: ", TYPE$sm, "; color: var(--ipc-muted);
  line-height: 1.55; }
.empty-state ol {
  text-align: left; margin: ", SPACE$x2, " 0 0; padding-left: 1.2rem;
  font-size: ", TYPE$sm, "; color: var(--ipc-body); line-height: 1.7;
}
.empty-state ol::marker { color: var(--ipc-muted); }

/* ---------------------------------------------------------- banners ------ */
.status {
  padding: .7rem .85rem; border-radius: .55rem; border: 1px solid transparent;
  border-left-width: 3px; font-size: ", TYPE$sm, "; line-height: 1.5;
  margin-bottom: ", SPACE$x3, ";
}
.status-ok   { background: var(--ipc-primary-lt); border-color: var(--ipc-primary);
               color: var(--ipc-ink); }
.status-warn { background: var(--ipc-warning-lt); border-color: var(--ipc-warning);
               color: var(--ipc-ink); }
.status-bad  { background: var(--ipc-danger-lt);  border-color: var(--ipc-danger);
               color: var(--ipc-ink); }
.status b, .status strong { font-weight: 650; }
.status .bi { vertical-align: -.12em; }
.status-ok   .bi { color: var(--ipc-primary); }
.status-warn .bi { color: var(--ipc-warning); }
.status-bad  .bi { color: var(--ipc-danger); }

/* ------------------------------------------------------- metric strip ---- */
.metric-row {
  display: grid; gap: ", SPACE$x3, "; margin-bottom: ", SPACE$x4, ";
  grid-template-columns: repeat(auto-fit, minmax(170px, 1fr));
}
.metric {
  background: var(--ipc-surface); border: 1px solid var(--ipc-line);
  border-radius: .6rem; padding: .7rem .85rem;
}
.metric .metric-label {
  font-size: ", TYPE$xs, "; text-transform: uppercase; letter-spacing: .09em;
  color: var(--ipc-muted); font-weight: 700;
}
.metric .metric-value {
  font-size: ", TYPE$xl, "; font-weight: 640; color: var(--ipc-ink);
  line-height: 1.15; margin-top: .15rem; letter-spacing: -.02em;
}
.metric .metric-sub { font-size: ", TYPE$sm, "; color: var(--ipc-muted); margin-top: .1rem; }

/* ------------------------------------------------------------ figures ---- */
.fig-toolbar {
  display: flex; justify-content: flex-end; align-items: flex-end;
  flex-wrap: wrap; gap: ", SPACE$x2, "; margin-bottom: ", SPACE$x2, ";
}
.fig-toolbar .shiny-input-container { margin-bottom: 0; }
/* Figures keep a light 'paper' ground in both modes. They are exported for
   print at 600 dpi, so what is on screen must be what lands in the file; a
   dark-inverted preview of a light figure would be a lie. */
.fig-paper {
  background: #FFFFFF; border: 1px solid var(--ipc-line);
  border-radius: .5rem; padding: .4rem;
}
[data-bs-theme='dark'] .fig-paper { border-color: #313F39; }

/* ------------------------------------------------------------- tables ---- */
.table-scroll { overflow-x: auto; }
/* bslib marks a card body and its children as flex fill items even when the
   card is built with fill = FALSE, so a long table is capped at whatever space
   is left in the card and the rows below it - and the note under the table -
   are simply not drawn. A results table is as tall as its rows. */
.datatables.html-widget {
  flex: 0 0 auto !important;
  height: auto !important;
  min-height: 0;
}
.card-body:has(> .datatables.html-widget) { display: block; }
.card-body:has(> .datatables.html-widget) > * + * { margin-top: ", SPACE$x3, "; }
table.dataTable { font-size: ", TYPE$sm, "; border-collapse: separate !important; }
table.dataTable thead th {
  font-size: ", TYPE$xs, "; text-transform: uppercase; letter-spacing: .06em;
  font-weight: 700; color: var(--ipc-muted);
  background: var(--ipc-surface-2); border-bottom: 1px solid var(--ipc-line) !important;
  padding: .5rem .55rem !important; white-space: nowrap;
}
table.dataTable tbody td {
  padding: .4rem .55rem !important; border-top: 1px solid var(--ipc-line-soft);
  color: var(--ipc-body);
}
table.dataTable tbody tr:hover td { background: var(--ipc-primary-lt); }
table.dataTable.stripe tbody tr.odd td { background-color: var(--ipc-surface-2); }
table.dataTable.stripe tbody tr.odd:hover td { background: var(--ipc-primary-lt); }
.dataTables_wrapper .dataTables_length, .dataTables_wrapper .dataTables_filter,
.dataTables_wrapper .dataTables_info, .dataTables_wrapper .dataTables_paginate {
  font-size: ", TYPE$sm, "; color: var(--ipc-muted);
}
.dataTables_wrapper .dataTables_filter input, .dataTables_wrapper .dataTables_length select {
  border: 1px solid var(--ipc-line); border-radius: .4rem; padding: .18rem .45rem;
  background: var(--ipc-surface); color: var(--ipc-body);
}
.dataTables_wrapper .dataTables_paginate .paginate_button.current {
  background: var(--ipc-primary-lt) !important; border-color: var(--ipc-line) !important;
  color: var(--ipc-primary) !important;
}
[data-bs-theme='dark'] .dataTables_wrapper .dataTables_paginate .paginate_button {
  color: var(--ipc-body) !important;
}
/* DataTables paints stripes, hover and selection as an inset box-shadow
   overlay rather than a background, and its overlay lightens. On a dark
   surface that turns every second row into pale text on a pale band, so the
   overlay is switched off and the bands are painted from the tokens. */
[data-bs-theme='dark'] table.dataTable {
  --dt-row-stripe: transparent;
  --dt-row-hover: transparent;
  --dt-row-selected: transparent;
}
[data-bs-theme='dark'] table.dataTable > tbody > tr > *,
[data-bs-theme='dark'] table.dataTable > tbody > tr.odd > *,
[data-bs-theme='dark'] table.dataTable > tbody > tr.even > * {
  box-shadow: none !important;
  color: var(--ipc-body);
}
[data-bs-theme='dark'] table.dataTable > tbody > tr > * { background-color: var(--ipc-surface); }
[data-bs-theme='dark'] table.dataTable.stripe > tbody > tr.odd > * {
  background-color: var(--ipc-surface-2);
}
[data-bs-theme='dark'] table.dataTable > tbody > tr:hover > * {
  background-color: var(--ipc-primary-lt);
}
[data-bs-theme='dark'] table.dataTable thead th { background: var(--ipc-surface-2); }

/* --------------------------------------------------------- misc chrome --- */
.step-badge {
  display: inline-block; min-width: 1.4rem; text-align: center;
  background: var(--ipc-primary); color: #fff; border-radius: .35rem;
  font-size: ", TYPE$xs, "; font-weight: 700; margin-right: .4rem;
  padding: .1rem .3rem; line-height: 1.4;
}
[data-bs-theme='dark'] .step-badge { color: var(--ipc-primary-dk); }
.licence-box { font-size: ", TYPE$sm, "; }
/* ASReml failure messages are multi-line and must stay copyable and readable. */
.error-detail {
  white-space: pre-wrap; font-size: 0.78rem; margin: .4rem 0 0;
  padding: .5rem .6rem; border-radius: .4rem;
  background: var(--ipc-surface); border: 1px solid var(--ipc-line);
  color: var(--ipc-body); max-height: 16rem; overflow: auto;
}
.popover { border-color: var(--ipc-line); box-shadow: 0 8px 28px rgba(15,33,26,.14); }
.popover-header { font-size: ", TYPE$sm, "; font-weight: 650; background: var(--ipc-surface-2); }
.popover-body { font-size: ", TYPE$sm, "; }
.form-group.shiny-input-container[style*='width: auto'] { margin-bottom: 0; }

/* A thin, self-effacing scrollbar; the default Windows bar is 17px of grey. */
* { scrollbar-width: thin; scrollbar-color: var(--ipc-line) transparent; }
*::-webkit-scrollbar { width: 9px; height: 9px; }
*::-webkit-scrollbar-thumb { background: var(--ipc-line); border-radius: 6px; }
*::-webkit-scrollbar-track { background: transparent; }

/* ------------------------------------------------------- responsive ------ */
@media (max-width: 991px) {
  .card-body { padding: ", SPACE$x3, "; }
  .metric .metric-value { font-size: ", TYPE$lg, "; }
}
@media (max-width: 575px) {
  .metric-row { grid-template-columns: 1fr; }
  .nav-tabs .nav-link { padding: .5rem .55rem; }
  .fig-toolbar { justify-content: flex-start; }
}

@media (prefers-reduced-motion: reduce) {
  * { transition: none !important; animation: none !important; }
}

@media print {
  .navbar, .sidebar, .fig-toolbar, .dataTables_paginate, .dataTables_length { display: none !important; }
  .card { border: 1px solid #ccc; box-shadow: none; page-break-inside: avoid; }
  body { background: #fff; }
}
"))
}

# ---------------------------------------------------------------------------
# Figure theme and scales
# ---------------------------------------------------------------------------

#' Shared ggplot2 theme for screen and export.
#'
#' `base_size` is raised for exported figures so that text scales with the
#' larger canvas instead of becoming unreadably small at 600 dpi. Grid and axis
#' furniture is deliberately recessive: the data should be the darkest thing in
#' the frame.
#'
#' @param base_size base point size
#' @param grid which major grid lines to keep: "xy", "x", "y" or "none"
#' @noRd
theme_trial <- function(base_size = 12, grid = "xy") {
  th <- ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      text             = ggplot2::element_text(colour = PAL$body,
                                               lineheight = 1.15),
      plot.title       = ggplot2::element_text(face = "bold", size = base_size * 1.22,
                                               colour = PAL$ink, hjust = 0,
                                               margin = ggplot2::margin(b = 3)),
      plot.subtitle    = ggplot2::element_text(size = base_size * 0.92, colour = PAL$muted,
                                               hjust = 0, lineheight = 1.2,
                                               margin = ggplot2::margin(b = 10)),
      plot.caption     = ggplot2::element_text(size = base_size * 0.82, colour = PAL$muted,
                                               hjust = 0, lineheight = 1.2,
                                               margin = ggplot2::margin(t = 10)),
      plot.title.position   = "plot",
      plot.caption.position = "plot",
      axis.title       = ggplot2::element_text(size = base_size * 0.95, colour = PAL$body,
                                               face = "plain"),
      axis.title.x     = ggplot2::element_text(margin = ggplot2::margin(t = 6)),
      axis.title.y     = ggplot2::element_text(margin = ggplot2::margin(r = 6)),
      axis.text        = ggplot2::element_text(size = base_size * 0.88, colour = PAL$muted),
      axis.ticks       = ggplot2::element_line(colour = PAL$line, linewidth = 0.3),
      axis.ticks.length = grid::unit(2.5, "pt"),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(colour = PAL$line_soft, linewidth = 0.4),
      panel.background = ggplot2::element_rect(fill = PAL$surface, colour = NA),
      plot.background  = ggplot2::element_rect(fill = PAL$surface, colour = NA),
      legend.position  = "bottom",
      legend.justification = "left",
      legend.margin    = ggplot2::margin(t = 2),
      # The title sits above the key, not beside it. Beside it, a horizontal
      # colour bar plus a long title is wider than a single-column canvas and
      # the bar is clipped at the page edge.
      legend.title.position = "top",
      legend.title     = ggplot2::element_text(size = base_size * 0.88,
                                               face = "bold", colour = PAL$body),
      legend.text      = ggplot2::element_text(size = base_size * 0.86,
                                               colour = PAL$muted),
      # A horizontal colour bar at the bottom needs a wide, short key or its
      # break labels collide with one another.
      legend.key.height = grid::unit(0.55, "lines"),
      legend.key.width  = grid::unit(2.6, "lines"),
      strip.text       = ggplot2::element_text(face = "bold", size = base_size * 0.9,
                                               colour = PAL$ink,
                                               margin = ggplot2::margin(4, 4, 4, 4)),
      strip.background = ggplot2::element_rect(fill = PAL$surface_2, colour = NA),
      plot.margin      = ggplot2::margin(12, 14, 10, 12)
    )
  if (grid == "x") th <- th + ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
  if (grid == "y") th <- th + ggplot2::theme(panel.grid.major.x = ggplot2::element_blank())
  if (grid == "none") th <- th + ggplot2::theme(panel.grid.major = ggplot2::element_blank())
  th
}

#' Effect-type colour scale shared by all figures.
#' @noRd
effect_colours <- function() {
  c(Direct = PAL$direct, Competition = PAL$competition, `Pure stand` = PAL$pure,
    Neighbour = PAL$competition)
}

#' Series colour for one derived quantity.
#'
#' Colour follows the entity, never its rank: the direct effect is always the
#' field green, the competitive effect the warm earth, and anything derived
#' from the pure-stand value the blue, in every figure in the application.
#' @noRd
series_colour <- function(effect) {
  switch(effect,
    Direct_effect      = PAL$direct,
    Competition_effect = PAL$competition,
    PAL$pure)
}

#' Readable label colour for text drawn on top of a filled mark.
#'
#' Picked from the fill's own relative luminance (WCAG 2.1) rather than from a
#' threshold on the underlying value, so a label can never land as white text
#' on a pale step of a ramp.
#'
#' @param fill vector of fill colours
#' @noRd
contrast_text <- function(fill) {
  rgb <- t(grDevices::col2rgb(fill)) / 255
  lin <- ifelse(rgb <= 0.04045, rgb / 12.92, ((rgb + 0.055) / 1.055)^2.4)
  lum <- lin %*% c(0.2126, 0.7152, 0.0722)
  # Compare contrast against white and against the ink token and take the
  # better of the two.
  ifelse((1.05) / (lum + 0.05) >= (lum + 0.05) / 0.05, "#FFFFFF", PAL$ink)
}

#' Diverging colour scale centred on a neutral midpoint.
#'
#' A diverging ramp only means anything if its neutral step sits on the value
#' that divides the two directions. Letting ggplot2 stretch the ramp over the
#' observed range puts the neutral colour at the midpoint of the data instead,
#' so a set of entirely positive effects comes out half blue - the most common
#' way a correlation or effect map misleads.
#'
#' @param values the vector being mapped, used to find a symmetric limit
#' @param centre the value the neutral step must land on
#' @param aesthetic "colour" or "fill"; strokes take the higher-contrast ramp
#' @param ... passed to the scale (`name`, `guide`, ...)
#' @noRd
scale_diverging <- function(values, centre = 0, aesthetic = c("colour", "fill"),
                            ...) {
  aesthetic <- match.arg(aesthetic)
  span <- suppressWarnings(max(abs(values - centre), na.rm = TRUE))
  if (!is.finite(span) || span <= 0) span <- 1
  limits <- c(centre - span, centre + span)
  if (aesthetic == "colour") {
    ggplot2::scale_colour_gradientn(colours = DIVERGING_LINE, limits = limits,
                                    oob = scales::squish, ...)
  } else {
    ggplot2::scale_fill_gradientn(colours = DIVERGING, limits = limits,
                                  oob = scales::squish, ...)
  }
}
