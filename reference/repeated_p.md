# Calculate the repeated p-value for a single hypothesis at a given analysis

A repeated p-value at analysis \\k\\ is the minimum significance level
at which the group sequential boundary at analysis \\k\\ would be
crossed. Unlike the sequential p-value, which considers all analyses up
to \\k\\, the repeated p-value only considers the boundary at analysis
\\k\\ itself.

The sequential p-value equals the minimum of repeated p-values across
analyses: \\\tilde{p}\_k = \min\_{l=1}^{k} \hat{p}\_l\\, where
\\\hat{p}\_l\\ is the repeated p-value at analysis \\l\\.

## Usage

``` r
repeated_p(
  p,
  info_frac,
  spending_fn,
  tol = 1e-06,
  maxpts = 25000,
  abseps = 1e-06
)
```

## Arguments

- p:

  A numeric vector of p-values at each analysis for a single hypothesis.
  The length must match the length of `info_frac`. All values must be
  non-missing and between 0 and 1.

- info_frac:

  A numeric vector of information fractions at each analysis. Values
  must be in (0, 1\] and monotonically non-decreasing. The length should
  match the length of `p`.

- spending_fn:

  A spending function. Must accept two arguments: `alpha` (total
  significance level) and `info_frac` (information fraction), and return
  the cumulative alpha spent. Built-in options include
  [`spending_of()`](https://openpharma.github.io/graphicalMCP/reference/spending_functions.md),
  [`spending_pocock()`](https://openpharma.github.io/graphicalMCP/reference/spending_functions.md),
  [`spending_hsd()`](https://openpharma.github.io/graphicalMCP/reference/spending_functions.md),
  and
  [`spending_linear()`](https://openpharma.github.io/graphicalMCP/reference/spending_functions.md).

- tol:

  A numeric scalar for the tolerance of the root-finding algorithm. The
  default is `1e-6`.

- maxpts:

  An integer scalar for the maximum number of function values for
  [`mvtnorm::GenzBretz`](https://rdrr.io/pkg/mvtnorm/man/algorithms.html).
  The default is 25000.

- abseps:

  A numeric scalar for the absolute error tolerance for
  [`mvtnorm::GenzBretz`](https://rdrr.io/pkg/mvtnorm/man/algorithms.html).
  The default is 1e-6.

## Value

A numeric scalar of the repeated p-value at the last analysis in the
input vectors.

## Details

For a hypothesis tested at analyses \\k = 1, \ldots, K\\ with p-values
\\p^{(k)}\\ and information fractions \\t^{(k)}\\, the repeated p-value
at analysis \\K\\ is the minimum \\\hat{p}\\ such that the observed
p-value \\p^{(K)}\\ crosses the group sequential boundary
\\c_K(\hat{p})\\ at that analysis: \$\$\hat{p}\_K = \min\\\alpha :
p^{(K)} \le c_K(\alpha)\\.\$\$

Note that computing the boundary \\c_K(\alpha)\\ requires knowledge of
all previous information fractions \\t^{(1)}, \ldots, t^{(K)}\\ because
the boundary at analysis \\K\\ depends on the cumulative spending and
the joint distribution of test statistics.

The repeated p-value is found using
[`stats::uniroot()`](https://rdrr.io/r/stats/uniroot.html) on the
function \\g(\alpha) = z_K - b_K(\alpha)\\, where \\z_K = \Phi^{-1}(1 -
p^{(K)})\\ is the observed Z-statistic at analysis \\K\\ and
\\b_K(\alpha)\\ is the Z-scale boundary.

## References

Maurer, W., and Bretz, F. (2013). Multiple testing in group sequential
trials using graphical approaches. *Statistics in Biopharmaceutical
Research*, 5(4), 311-320.

## See also

[`sequential_p()`](https://openpharma.github.io/graphicalMCP/reference/sequential_p.md)
for the sequential p-value (minimum repeated p-value),
[`gs_boundaries()`](https://openpharma.github.io/graphicalMCP/reference/gs_boundaries.md)
for computing group sequential boundaries,
[`graph_test_shortcut_gsd()`](https://openpharma.github.io/graphicalMCP/reference/graph_test_shortcut_gsd.md)
for graphical multiple comparison procedures with group sequential
designs.

## Examples

``` r
# Repeated p-value at the second analysis (interim at 50%, final at 100%)
repeated_p(
  p = c(0.024, 0.01),
  info_frac = c(0.5, 1),
  spending_fn = spending_of
)
#> [1] 0.01009393

# Compare with sequential p-value (which is the minimum repeated p-value)
sequential_p(
  p = c(0.024, 0.01),
  info_frac = c(0.5, 1),
  spending_fn = spending_of
)
#> [1] 0.01009409

# Repeated p-values at each analysis
# Analysis 1
repeated_p(
  p = 0.05,
  info_frac = 0.3,
  spending_fn = spending_of
)
#> [1] 0.2830392

# Analysis 2
repeated_p(
  p = c(0.05, 0.02),
  info_frac = c(0.3, 0.7),
  spending_fn = spending_of
)
#> [1] 0.05185878
```
