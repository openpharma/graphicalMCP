# Compute group sequential boundaries from an alpha spending function

Given a significance level, information fractions, and a spending
function, compute the group sequential boundaries at each analysis. The
boundaries are computed on the Z-scale using the recursive relationship
between cumulative spending and the joint distribution of test
statistics. The null hypothesis is rejected at analysis \\k\\ if the
test statistic \\Z_k \ge b_k\\.

At analysis \\k\\, the Z-scale boundary \\b_k\\ satisfies \$\$P(Z_1 \<
b_1, \ldots, Z_k \< b_k) = 1 - f(\alpha, t_k),\$\$ where \\f(\alpha,
t_k)\\ is the cumulative spending at information fraction \\t_k\\, and
\\(Z_1, \ldots, Z_k)\\ follows the canonical joint distribution with
mean zero and correlations given by
[`gs_corr()`](https://openpharma.github.io/graphicalMCP/reference/gs_corr.md).

## Usage

``` r
gs_boundaries(alpha, info_frac, spending_fn, maxpts = 25000, abseps = 1e-06)
```

## Arguments

- alpha:

  A numeric scalar of the significance level to be spent across
  analyses.

- info_frac:

  A numeric vector of information fractions at each analysis. Must be
  monotonically non-decreasing with values in (0, 1\].

- spending_fn:

  A spending function that takes two arguments: `alpha` (significance
  level) and `info_frac` (information fraction), and returns the
  cumulative alpha spent. See
  [`spending_of()`](https://openpharma.github.io/graphicalMCP/reference/spending_functions.md).

- maxpts:

  An integer scalar for the maximum number of function values for
  [`mvtnorm::GenzBretz`](https://rdrr.io/pkg/mvtnorm/man/algorithms.html).
  The default is 25000.

- abseps:

  A numeric scalar for the absolute error tolerance for
  [`mvtnorm::GenzBretz`](https://rdrr.io/pkg/mvtnorm/man/algorithms.html).
  The default is 1e-6.

## Value

A list with elements:

- `bounds_z` - A numeric vector of Z-scale boundaries at each analysis.

- `bounds_nominal` - A numeric vector of nominal p-value boundaries at
  each analysis, i.e., \\c_k = 1 - \Phi(b_k)\\.

## See also

[`spending_of()`](https://openpharma.github.io/graphicalMCP/reference/spending_functions.md),
[`spending_pocock()`](https://openpharma.github.io/graphicalMCP/reference/spending_functions.md),
[`spending_hsd()`](https://openpharma.github.io/graphicalMCP/reference/spending_functions.md),
[`spending_linear()`](https://openpharma.github.io/graphicalMCP/reference/spending_functions.md)
for spending functions,
[`sequential_p()`](https://openpharma.github.io/graphicalMCP/reference/sequential_p.md)
for sequential p-values,
[`graph_test_shortcut_gsd()`](https://openpharma.github.io/graphicalMCP/reference/graph_test_shortcut_gsd.md)
for graphical MCPs with group sequential designs,
[`gs_corr()`](https://openpharma.github.io/graphicalMCP/reference/gs_corr.md)
for the correlation matrix.
