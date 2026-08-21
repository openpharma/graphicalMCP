# Calculate the sequential p-value for a single hypothesis

A sequential p-value is the minimum significance level at which a group
sequential boundary would be crossed at any analysis up to and including
the current one. It is computed using the group sequential boundaries
derived from the spending function and the joint distribution of test
statistics across analyses.

Sequential p-values are used in graphical multiple comparison procedures
for group sequential designs. They allow the separation of the group
sequential testing (handled by the spending function and boundaries)
from the multiplicity adjustment (handled by the graph). See Maurer and
Bretz (2013) for details.

## Usage

``` r
sequential_p(
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
  the cumulative alpha spent at information fraction `t`. Built-in
  options include
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

A numeric scalar of the sequential p-value.

## Details

For a hypothesis tested at analyses \\k = 1, \ldots, K\\ with p-values
\\p^{(k)}\\ and information fractions \\t^{(k)}\\, the sequential
p-value is the minimum \\\tilde{p}\\ such that for some analysis \\k\\,
the observed p-value \\p^{(k)}\\ crosses the group sequential boundary
\\c_k(\tilde{p})\\ derived from the spending function: \$\$\tilde{p} =
\min\\\alpha : p^{(k)} \le c_k(\alpha) \text{ for some } k\\,\$\$ where
\\c_k(\alpha)\\ is the nominal p-value boundary at analysis \\k\\ when
the total significance level is \\\alpha\\.

The boundary \\c_k(\alpha)\\ is computed from the spending function
\\f(\alpha, t)\\ using the joint distribution of test statistics.
Specifically, the Z-scale boundary \\b_k\\ satisfies \$\$P(Z_1 \< b_1,
\ldots, Z_k \< b_k) = 1 - f(\alpha, t_k),\$\$ and \\c_k = 1 -
\Phi(b_k)\\. Note that \\c_k \neq f(\alpha, t_k) - f(\alpha, t\_{k-1})\\
for \\k \> 1\\ due to the correlation between test statistics across
analyses.

The sequential p-value is found using
[`stats::uniroot()`](https://rdrr.io/r/stats/uniroot.html) on the
function \\g(\alpha) = \max_k (z_k - b_k(\alpha))\\, where \\z_k =
\Phi^{-1}(1 - p^{(k)})\\ is the observed Z-statistic and \\b_k(\alpha)\\
is the Z-scale boundary. This function is monotonically increasing in
\\\alpha\\ since boundaries become less stringent as \\\alpha\\
increases.

## References

Maurer, W., and Bretz, F. (2013). Multiple testing in group sequential
trials using graphical approaches. *Statistics in Biopharmaceutical
Research*, 5(4), 311-320.

Liu, Q., and Anderson, K. M. (2008). On adaptive extensions of group
sequential trials for clinical investigations. *Journal of the American
Statistical Association*, 103(484), 1621-1630.

## See also

[`gs_boundaries()`](https://openpharma.github.io/graphicalMCP/reference/gs_boundaries.md)
for computing group sequential boundaries,
[`graph_test_shortcut_gsd()`](https://openpharma.github.io/graphicalMCP/reference/graph_test_shortcut_gsd.md)
for graphical multiple comparison procedures with group sequential
designs,
[`spending_of()`](https://openpharma.github.io/graphicalMCP/reference/spending_functions.md),
[`spending_pocock()`](https://openpharma.github.io/graphicalMCP/reference/spending_functions.md),
[`spending_hsd()`](https://openpharma.github.io/graphicalMCP/reference/spending_functions.md),
[`spending_linear()`](https://openpharma.github.io/graphicalMCP/reference/spending_functions.md)
for spending functions.

## Examples

``` r
# A hypothesis tested at two analyses (interim at 50% and final at 100%)
sequential_p(
  p = c(0.024, 0.01),
  info_frac = c(0.5, 1),
  spending_fn = spending_of
)
#> [1] 0.01009409

# Sequential p-value with Pocock spending
sequential_p(
  p = c(0.024, 0.01),
  info_frac = c(0.5, 1),
  spending_fn = spending_pocock
)
#> [1] 0.01850038

# Sequential p-value updates as more analyses are conducted
# After analysis 1 only
sequential_p(
  p = 0.05,
  info_frac = 0.3,
  spending_fn = spending_of
)
#> [1] 0.2830392

# After analyses 1 and 2
sequential_p(
  p = c(0.05, 0.02),
  info_frac = c(0.3, 0.7),
  spending_fn = spending_of
)
#> [1] 0.05185881

# After all three analyses
sequential_p(
  p = c(0.05, 0.02, 0.01),
  info_frac = c(0.3, 0.7, 1),
  spending_fn = spending_of
)
#> [1] 0.01071828
```
