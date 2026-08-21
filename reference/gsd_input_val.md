# Validate inputs for group sequential graphical MCP

Validate inputs for group sequential graphical MCP

## Usage

``` r
gsd_input_val(
  graph,
  p,
  alpha,
  info_frac,
  spending_fn,
  look_back,
  verbose,
  test_values
)
```

## Arguments

- graph:

  An initial graph as returned by
  [`graph_create()`](https://openpharma.github.io/graphicalMCP/reference/graph_create.md).

- p:

  A numeric matrix of p-values with \\m\\ rows (hypotheses) and \\K\\
  columns (analyses), where \\K\\ is the maximum number of analyses
  across all hypotheses. For hypotheses not tested at every analysis,
  use `NA` for the columns without data. Each hypothesis must have at
  least one non-`NA` value.

- alpha:

  A numeric scalar of the overall significance level, which should be
  between 0 & 1. The default is 0.025 for one-sided hypothesis testing
  problems; another common choice is 0.05 for two-sided hypothesis
  testing problems.

- info_frac:

  Information fractions at each analysis. Can be:

  - A numeric vector of length \\K\\ — same fractions for all
    hypotheses. Only allowed when `p` contains no `NA` values (i.e., all
    hypotheses have the same number of analyses).

  - A numeric matrix with \\m\\ rows (hypotheses) and \\K\\ columns
    (analyses) — different fractions per hypothesis. When `p` contains
    `NA` padding, `info_frac` must be a matrix with `NA` in the same
    positions as `p`.

  Non-`NA` values must be positive and monotonically non-decreasing per
  hypothesis. Values greater than 1 are allowed (e.g., when more
  information is collected than planned). The spending functions cap the
  cumulative spending at `alpha` for information fractions at or
  above 1. The last non-`NA` value does not need to be 1, allowing the
  procedure to be applied up to an interim analysis.

- spending_fn:

  Spending function(s) for computing group sequential boundaries. Can
  be:

  - A single function — applied to all hypotheses.

  - A list of \\m\\ functions — one per hypothesis.

  Each function must accept two arguments: `alpha` (significance level)
  and `info_frac` (information fraction), and return the cumulative
  alpha spent. Built-in options include
  [`spending_of()`](https://openpharma.github.io/graphicalMCP/reference/spending_functions.md),
  [`spending_pocock()`](https://openpharma.github.io/graphicalMCP/reference/spending_functions.md),
  [`spending_hsd()`](https://openpharma.github.io/graphicalMCP/reference/spending_functions.md),
  and
  [`spending_linear()`](https://openpharma.github.io/graphicalMCP/reference/spending_functions.md).

- look_back:

  A logical scalar or vector controlling the testing strategy. Can be:

  - A single logical — applied to all hypotheses.

  - A logical vector of length \\m\\ — one per hypothesis, allowing
    different strategies for different hypotheses.

  For hypotheses with `look_back = FALSE` (the default), rejection
  decisions at each analysis are based on repeated p-values at that
  analysis only. For hypotheses with `look_back = TRUE`, rejection
  decisions are based on sequential p-values which consider all analyses
  up to the current one. The `look_back = TRUE` option can lead to
  additional rejections because a hypothesis may have crossed its
  boundary at an earlier analysis but only becomes testable (via graph
  update) at a later analysis.

- verbose:

  A logical scalar specifying whether to include the boundary table in
  results. When `verbose = TRUE`, a table of nominal p-value boundaries
  is computed for each hypothesis at all possible weights from the
  graph's closure (via
  [`graph_generate_weights()`](https://openpharma.github.io/graphicalMCP/reference/graph_generate_weights.md)).
  This enables manual verification of rejection decisions. The default
  is `FALSE`.

- test_values:

  A logical scalar specifying whether to include the per-analysis
  rejection details in results. When `test_values = TRUE`, the rejection
  sequence, analysis at which each rejection occurred, and the nominal
  p-value boundaries are reported. The default is `test_values = FALSE`.

## Value

Invisibly returns `graph`.
