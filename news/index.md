# Changelog

## graphicalMCP 0.1.0

- First release

## graphicalMCP 0.1.1

- Added compilation of vignettes
  ([\#73](https://github.com/openpharma/graphicalMCP/issues/73))
- Removed duplicated columns of “\*” in test values
  ([\#75](https://github.com/openpharma/graphicalMCP/issues/75))

## graphicalMCP 0.1.2

- Updated function documentation
- Updated references

## graphicalMCP 0.2.0

- Corrected typos
- Updated function documentation for CRAN release

## graphicalMCP 0.2.1

- Corrected typos
- First CRAN release

## graphicalMCP 0.2.2

- Updated citations
- Resubmission for first CRAN release

## graphicalMCP 0.2.3

- Included cran-comments.ms in .Rbuildignore
- Resubmission for first CRAN release

## graphicalMCP 0.2.4

- Updated documentation according to issue
  [\#84](https://github.com/openpharma/graphicalMCP/issues/84)
- Resubmission for first CRAN release

## graphicalMCP 0.2.5

CRAN release: 2024-07-13

- Updated adjust_weights_parametric_util.Rd
- Resubmission for first CRAN release

## graphicalMCP 0.2.6

CRAN release: 2024-11-08

- Github repo transferred to openpharma
- Submission for CRAN release

## graphicalMCP 0.2.7

- Added Hochberg-based procedures
- Added internal validations
- Expanded example graphs
- Updated vignettes
- Submission for CRAN release

## graphicalMCP 0.2.8

CRAN release: 2025-05-07

- Corrected urls for references
- Submission for CRAN release

## graphicalMCP 0.2.9

CRAN release: 2026-03-21

- Closed Issue
  [\#90](https://github.com/openpharma/graphicalMCP/issues/90) by
  changing the precision for parametric tests
- Submission for CRAN release

## graphicalMCP 0.3.0

- Added group sequential testing for graphical MCPs via
  [`graph_test_shortcut_gsd()`](https://openpharma.github.io/graphicalMCP/reference/graph_test_shortcut_gsd.md),
  which extends
  [`graph_test_shortcut()`](https://openpharma.github.io/graphicalMCP/reference/graph_test_shortcut.md)
  to multiple analyses with per-hypothesis spending functions,
  information fractions, and a `look_back` option to use repeated or
  sequential p-values
- Added spending functions
  [`spending_of()`](https://openpharma.github.io/graphicalMCP/reference/spending_functions.md),
  [`spending_pocock()`](https://openpharma.github.io/graphicalMCP/reference/spending_functions.md),
  [`spending_hsd()`](https://openpharma.github.io/graphicalMCP/reference/spending_functions.md),
  [`spending_linear()`](https://openpharma.github.io/graphicalMCP/reference/spending_functions.md),
  [`spending_wt()`](https://openpharma.github.io/graphicalMCP/reference/spending_wt.md)
  (Wang-Tsiatis), and
  [`spending_with_time()`](https://openpharma.github.io/graphicalMCP/reference/spending_with_time.md)
  for a custom spending time
- Added
  [`gs_boundaries()`](https://openpharma.github.io/graphicalMCP/reference/gs_boundaries.md),
  [`gs_corr()`](https://openpharma.github.io/graphicalMCP/reference/gs_corr.md),
  [`repeated_p()`](https://openpharma.github.io/graphicalMCP/reference/repeated_p.md),
  and
  [`sequential_p()`](https://openpharma.github.io/graphicalMCP/reference/sequential_p.md)
- Added vignettes on group sequential testing and on its validation
  against gsDesign and rpact
- Renamed the `Adj. P-value` column to `Adj.p` in
  [`print.graph_report()`](https://openpharma.github.io/graphicalMCP/reference/print.graph_report.md)
- Added a warning in
  [`graph_create()`](https://openpharma.github.io/graphicalMCP/reference/graph_create.md)
  for very small transition weights (Issue
  [\#97](https://github.com/openpharma/graphicalMCP/issues/97))
- Plotting examples, vignettes, and tests are skipped when `igraph` or
  `gMCP` is not installed
