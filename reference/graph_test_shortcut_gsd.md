# Perform shortcut graphical multiple comparison procedures with group sequential designs

Extends
[`graph_test_shortcut()`](https://openpharma.github.io/graphicalMCP/reference/graph_test_shortcut.md)
to group sequential designs where hypotheses can be tested at multiple
analyses (interim and final). At each analysis, the significance level
available for each hypothesis is determined by a spending function
evaluated at the information fraction. The group sequential boundaries
(critical values) are computed from the spending using the joint
distribution of test statistics across analyses.

The procedure supports two modes controlled by the `look_back`
parameter:

- **`look_back = FALSE`** (default): At each analysis, rejection
  decisions are based on **repeated p-values** at the current analysis
  only. A repeated p-value at analysis \\k\\ is the minimum significance
  level at which the group sequential boundary at analysis \\k\\ would
  be crossed. The graphical shortcut procedure
  ([`graph_test_shortcut()`](https://openpharma.github.io/graphicalMCP/reference/graph_test_shortcut.md))
  is applied at each analysis using repeated p-values, and the graph is
  updated before proceeding to the next analysis.

- **`look_back = TRUE`**: Rejection decisions are based on **sequential
  p-values**, which consider all analyses up to the current one. A
  sequential p-value is the minimum of repeated p-values across all
  analyses conducted so far. Like the default mode, the procedure
  processes analyses sequentially, applying
  [`graph_test_shortcut()`](https://openpharma.github.io/graphicalMCP/reference/graph_test_shortcut.md)
  at each analysis using sequential p-values and updating the graph
  before proceeding. When a hypothesis becomes testable at a later
  analysis (via graph update), its `first_rejected_at` is set to the
  earliest analysis where its boundary was crossed, while `decision_at`
  records the analysis where the rejection was operationally processed.

## Usage

``` r
graph_test_shortcut_gsd(
  graph,
  p,
  alpha = 0.025,
  info_frac,
  spending_fn,
  look_back = FALSE,
  verbose = FALSE,
  test_values = FALSE
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

An S3 object of class `gsd_graph_report` with a list of elements:

- `inputs` - Input parameters, including the initial graph, p-values,
  alpha, information fractions, and spending functions.

- `outputs` - Output parameters:

  - `repeated_p` - An m x K matrix of repeated p-values at each
    analysis,

  - `sequential_p` - An m x K matrix of sequential p-values (cumulative
    minimum of repeated p-values),

  - `adjusted_p` - Adjusted p-values from the shortcut procedure
    (adjusted repeated p-values when `look_back = FALSE`, adjusted
    sequential p-values when `look_back = TRUE`),

  - `rejected` - Logical vector of rejection decisions,

  - `decision_at` - Integer vector indicating the analysis at which each
    hypothesis's decision was made. For rejected hypotheses, this is the
    analysis where the rejection was operationally processed. For
    non-rejected hypotheses, this is the last analysis where the
    hypothesis was tested,

  - `first_rejected_at` - Integer vector indicating the earliest
    analysis at which each hypothesis's boundary was crossed. For
    non-rejected hypotheses, this is `NA`. When `look_back = TRUE`, this
    may be earlier than `decision_at` if a hypothesis crossed its
    boundary at a prior analysis but only became testable at a later
    analysis,

  - `last_rejected_at` - Integer vector indicating the latest analysis
    at which each hypothesis's boundary was crossed. For non-rejected
    hypotheses, this is `NA`. Comparing `first_rejected_at` and
    `last_rejected_at` shows whether the rejection is supported by data
    at multiple analyses or only at a single analysis,

  - `rejection_sequence` - Character vector giving the order in which
    hypotheses were rejected across all analyses,

  - `graph` - Updated graph after removing all rejected hypotheses.

- `test_values` - Per-analysis details (if `test_values = TRUE`). A list
  of length \\K\\ (one entry per analysis). Each entry is a data frame
  containing the hypothesis name, current weight, observed p-value,
  nominal boundary, and rejection decision at that analysis. Entries are
  `NULL` for analyses where no hypotheses are active. When
  `look_back = TRUE` and a hypothesis is rejected at an earlier
  analysis, additional rows show the nominal p-value and boundary at
  each prior analysis, with a `Look_back` column indicating these rows.

- `boundary_table` - Boundary lookup table (if `verbose = TRUE`). A
  named list with one data frame per hypothesis. Each data frame
  contains the columns `Weight`, `Alpha.Allocated`, and `Boundary.k` for
  each analysis \\k\\, showing the nominal p-value boundary at each
  analysis for every possible weight from the graph's closure. This
  table is independent of observed p-values and can be used to manually
  verify rejection decisions.

## References

Maurer, W., and Bretz, F. (2013). Multiple testing in group sequential
trials using graphical approaches. *Statistics in Biopharmaceutical
Research*, 5(4), 311-320.

Zhao, Y., Liu, Q., Sun, L. Z., and Anderson, K. M. (2025). Adjusted
inference for multiple testing procedure in group-sequential designs.
*Biometrical Journal*, 67(1), e70020.
[doi:10.1002/bimj.70020](https://doi.org/10.1002/bimj.70020)

## See also

[`graph_test_shortcut()`](https://openpharma.github.io/graphicalMCP/reference/graph_test_shortcut.md)
for the fixed-sample (non-sequential) shortcut procedure,
[`sequential_p()`](https://openpharma.github.io/graphicalMCP/reference/sequential_p.md)
for computing sequential p-values,
[`repeated_p()`](https://openpharma.github.io/graphicalMCP/reference/repeated_p.md)
for computing repeated p-values,
[`spending_of()`](https://openpharma.github.io/graphicalMCP/reference/spending_functions.md),
[`spending_pocock()`](https://openpharma.github.io/graphicalMCP/reference/spending_functions.md),
[`spending_hsd()`](https://openpharma.github.io/graphicalMCP/reference/spending_functions.md),
[`spending_linear()`](https://openpharma.github.io/graphicalMCP/reference/spending_functions.md)
for spending functions.

## Examples

``` r
# A graphical procedure with two hypotheses tested at two analyses
hypotheses <- c(0.5, 0.5)
transitions <- rbind(c(0, 1), c(1, 0))
g <- graph_create(hypotheses, transitions)

# P-values at interim (50% info) and final (100% info) analyses
p <- rbind(
  H1 = c(0.024, 0.01),
  H2 = c(0.015, 0.005)
)

graph_test_shortcut_gsd(
  graph = g,
  p = p,
  alpha = 0.025,
  info_frac = c(0.5, 1),
  spending_fn = spending_of
)
#> 
#> Test parameters ($inputs) ------------------------------------------------------
#>   Initial graph
#> 
#>   --- Hypothesis weights ---
#>   H1: 0.5
#>   H2: 0.5
#> 
#>   --- Transition weights ---
#>      H1 H2
#>   H1  0  1
#>   H2  1  0
#> 
#>   Alpha = 0.025
#> 
#>   Information fractions
#>    Analysis_1 Analysis_2
#> H1        0.5          1
#> H2        0.5          1
#> 
#>   P-values
#>    Analysis_1 Analysis_2
#> H1   0.024000   0.010000
#> H2   0.015000   0.005000
#> 
#>   Spending functions
#>     H1: O'Brien-Fleming
#>     H2: O'Brien-Fleming
#> 
#>   Look back = FALSE
#> 
#> Test summary ($outputs) --------------------------------------------------------
#>   Hypothesis   Adj.p* Reject Tested.at First.Rej.at Last.Rej.at Look.back
#>           H1 0.010094   TRUE         2            2           2     FALSE
#>           H2 0.010051   TRUE         2            2           2     FALSE
#>   (*) Adjusted p-values account for both the group sequential design and the
#>       graphical multiple comparison procedure. Based on repeated p-values when
#>       look_back = FALSE, and sequential p-values when look_back = TRUE.
#> 
#>   Rejection sequence: H2 -> H1
#> 
#>   Final updated graph after removing rejected hypotheses
#> 
#>   --- Hypothesis weights ---
#>   H1: NA
#>   H2: NA
#> 
#>   --- Transition weights ---
#>      H1 H2
#>   H1 NA NA
#>   H2 NA NA
#> 

# With look_back = TRUE (sequential p-values)
graph_test_shortcut_gsd(
  graph = g,
  p = p,
  alpha = 0.025,
  info_frac = c(0.5, 1),
  spending_fn = spending_of,
  look_back = TRUE
)
#> 
#> Test parameters ($inputs) ------------------------------------------------------
#>   Initial graph
#> 
#>   --- Hypothesis weights ---
#>   H1: 0.5
#>   H2: 0.5
#> 
#>   --- Transition weights ---
#>      H1 H2
#>   H1  0  1
#>   H2  1  0
#> 
#>   Alpha = 0.025
#> 
#>   Information fractions
#>    Analysis_1 Analysis_2
#> H1        0.5          1
#> H2        0.5          1
#> 
#>   P-values
#>    Analysis_1 Analysis_2
#> H1   0.024000   0.010000
#> H2   0.015000   0.005000
#> 
#>   Spending functions
#>     H1: O'Brien-Fleming
#>     H2: O'Brien-Fleming
#> 
#>   Look back = TRUE
#> 
#> Test summary ($outputs) --------------------------------------------------------
#>   Hypothesis   Adj.p* Reject Tested.at First.Rej.at Last.Rej.at Look.back
#>           H1 0.010094   TRUE         2            2           2      TRUE
#>           H2 0.010051   TRUE         2            2           2      TRUE
#>   (*) Adjusted p-values account for both the group sequential design and the
#>       graphical multiple comparison procedure. Based on repeated p-values when
#>       look_back = FALSE, and sequential p-values when look_back = TRUE.
#> 
#>   Rejection sequence: H2 -> H1
#> 
#>   Final updated graph after removing rejected hypotheses
#> 
#>   --- Hypothesis weights ---
#>   H1: NA
#>   H2: NA
#> 
#>   --- Transition weights ---
#>      H1 H2
#>   H1 NA NA
#>   H2 NA NA
#> 

# Different spending functions per hypothesis
graph_test_shortcut_gsd(
  graph = g,
  p = p,
  alpha = 0.025,
  info_frac = c(0.5, 1),
  spending_fn = list(spending_of, spending_pocock)
)
#> 
#> Test parameters ($inputs) ------------------------------------------------------
#>   Initial graph
#> 
#>   --- Hypothesis weights ---
#>   H1: 0.5
#>   H2: 0.5
#> 
#>   --- Transition weights ---
#>      H1 H2
#>   H1  0  1
#>   H2  1  0
#> 
#>   Alpha = 0.025
#> 
#>   Information fractions
#>    Analysis_1 Analysis_2
#> H1        0.5          1
#> H2        0.5          1
#> 
#>   P-values
#>    Analysis_1 Analysis_2
#> H1   0.024000   0.010000
#> H2   0.015000   0.005000
#> 
#>   Spending functions
#>     H1: O'Brien-Fleming
#>     H2: Pocock
#> 
#>   Look back = FALSE
#> 
#> Test summary ($outputs) --------------------------------------------------------
#>   Hypothesis   Adj.p* Reject Tested.at First.Rej.at Last.Rej.at Look.back
#>           H1 0.019426   TRUE         2            2           2     FALSE
#>           H2 0.019426   TRUE         2            2           2     FALSE
#>   (*) Adjusted p-values account for both the group sequential design and the
#>       graphical multiple comparison procedure. Based on repeated p-values when
#>       look_back = FALSE, and sequential p-values when look_back = TRUE.
#> 
#>   Rejection sequence: H2 -> H1
#> 
#>   Final updated graph after removing rejected hypotheses
#> 
#>   --- Hypothesis weights ---
#>   H1: NA
#>   H2: NA
#> 
#>   --- Transition weights ---
#>      H1 H2
#>   H1 NA NA
#>   H2 NA NA
#> 

# User-defined spending functions can also be used, e.g., wrapping
# gsDesign::sfHSD(). See vignette("group-sequential-testing") for details.

# Different information fractions per hypothesis
graph_test_shortcut_gsd(
  graph = g,
  p = p,
  alpha = 0.025,
  info_frac = rbind(c(0.5, 1), c(0.6, 1)),
  spending_fn = spending_of
)
#> 
#> Test parameters ($inputs) ------------------------------------------------------
#>   Initial graph
#> 
#>   --- Hypothesis weights ---
#>   H1: 0.5
#>   H2: 0.5
#> 
#>   --- Transition weights ---
#>      H1 H2
#>   H1  0  1
#>   H2  1  0
#> 
#>   Alpha = 0.025
#> 
#>   Information fractions
#>    Analysis_1 Analysis_2
#> H1        0.5          1
#> H2        0.6          1
#> 
#>   P-values
#>    Analysis_1 Analysis_2
#> H1   0.024000   0.010000
#> H2   0.015000   0.005000
#> 
#>   Spending functions
#>     H1: O'Brien-Fleming
#>     H2: O'Brien-Fleming
#> 
#>   Look back = FALSE
#> 
#> Test summary ($outputs) --------------------------------------------------------
#>   Hypothesis   Adj.p* Reject Tested.at First.Rej.at Last.Rej.at Look.back
#>           H1 0.010202   TRUE         2            2           2     FALSE
#>           H2 0.010202   TRUE         2            2           2     FALSE
#>   (*) Adjusted p-values account for both the group sequential design and the
#>       graphical multiple comparison procedure. Based on repeated p-values when
#>       look_back = FALSE, and sequential p-values when look_back = TRUE.
#> 
#>   Rejection sequence: H2 -> H1
#> 
#>   Final updated graph after removing rejected hypotheses
#> 
#>   --- Hypothesis weights ---
#>   H1: NA
#>   H2: NA
#> 
#>   --- Transition weights ---
#>      H1 H2
#>   H1 NA NA
#>   H2 NA NA
#> 

# Different numbers of analyses per hypothesis (NA padding)
# H1 at analyses 1-2, H2 at 1-3, H3 at 2-3, H4 at 1 and 3
g4 <- graph_create(
  rep(0.25, 4),
  rbind(
    c(0, 1 / 3, 1 / 3, 1 / 3),
    c(1 / 3, 0, 1 / 3, 1 / 3),
    c(1 / 3, 1 / 3, 0, 1 / 3),
    c(1 / 3, 1 / 3, 1 / 3, 0)
  )
)
p4 <- rbind(
  H1 = c(0.024, 0.01, NA),
  H2 = c(0.015, 0.005, 0.001),
  H3 = c(NA, 0.012, 0.004),
  H4 = c(0.05, NA, 0.015)
)
# info_frac must be a matrix with NA matching p
info_frac4 <- rbind(
  H1 = c(0.5, 1, NA),
  H2 = c(1 / 3, 2 / 3, 1),
  H3 = c(NA, 0.5, 1),
  H4 = c(0.4, NA, 1)
)
graph_test_shortcut_gsd(
  graph = g4,
  p = p4,
  alpha = 0.025,
  info_frac = info_frac4,
  spending_fn = spending_of
)
#> 
#> Test parameters ($inputs) ------------------------------------------------------
#>   Initial graph
#> 
#>   --- Hypothesis weights ---
#>   H1: 0.25
#>   H2: 0.25
#>   H3: 0.25
#>   H4: 0.25
#> 
#>   --- Transition weights ---
#>            H1       H2       H3       H4
#>   H1 0.000000 0.333333 0.333333 0.333333
#>   H2 0.333333 0.000000 0.333333 0.333333
#>   H3 0.333333 0.333333 0.000000 0.333333
#>   H4 0.333333 0.333333 0.333333 0.000000
#> 
#>   Alpha = 0.025
#> 
#>   Information fractions
#>    Analysis_1 Analysis_2 Analysis_3
#> H1  0.5000000  1.0000000         NA
#> H2  0.3333333  0.6666667          1
#> H3         NA  0.5000000          1
#> H4  0.4000000         NA          1
#> 
#>   P-values
#>    Analysis_1 Analysis_2 Analysis_3
#> H1   0.024000   0.010000         NA
#> H2   0.015000   0.005000   0.001000
#> H3         NA   0.012000   0.004000
#> H4   0.050000         NA   0.015000
#> 
#>   Spending functions
#>     H1: O'Brien-Fleming
#>     H2: O'Brien-Fleming
#>     H3: O'Brien-Fleming
#>     H4: O'Brien-Fleming
#> 
#>   Look back = FALSE
#> 
#> Test summary ($outputs) --------------------------------------------------------
#>   Hypothesis   Adj.p* Reject Tested.at First.Rej.at Last.Rej.at Look.back
#>           H1 0.040376  FALSE         2           --          --     FALSE
#>           H2 0.004075   TRUE         3            3           3     FALSE
#>           H3 0.012051   TRUE         3            3           3     FALSE
#>           H4 0.030085  FALSE         3           --          --     FALSE
#>   (*) Adjusted p-values account for both the group sequential design and the
#>       graphical multiple comparison procedure. Based on repeated p-values when
#>       look_back = FALSE, and sequential p-values when look_back = TRUE.
#> 
#>   Rejection sequence: H2 -> H3
#> 
#>   Final updated graph after removing rejected hypotheses
#> 
#>   --- Hypothesis weights ---
#>   H1: 0.5
#>   H2:  NA
#>   H3:  NA
#>   H4: 0.5
#> 
#>   --- Transition weights ---
#>      H1 H2 H3 H4
#>   H1  0 NA NA  1
#>   H2 NA NA NA NA
#>   H3 NA NA NA NA
#>   H4  1 NA NA  0
#> 
```
