# Calculate adjusted hypothesis weights for parametric tests

An intersection hypothesis can be rejected if its p-values are less than
or equal to their adjusted significance levels, which are their adjusted
hypothesis weights times \\\alpha\\. For Bonferroni tests, their
adjusted hypothesis weights are their hypothesis weights of the
intersection hypothesis. Additional adjustment is needed for parametric
tests:

- Parametric tests for
  [`adjust_weights_parametric()`](https://openpharma.github.io/graphicalMCP/reference/adjust_weights.md),

  - Note that one-sided tests are required for parametric tests.

## Usage

``` r
c_value_function(
  x,
  hypotheses,
  test_corr,
  alpha,
  maxpts = 25000,
  abseps = 1e-06,
  releps = 0
)

solve_c_parametric(
  hypotheses,
  test_corr,
  alpha,
  maxpts = 25000,
  abseps = 1e-06,
  releps = 0
)

adjust_weights_parametric_util(
  matrix_weights,
  matrix_intersections,
  test_corr,
  alpha,
  test_groups,
  maxpts = 25000,
  abseps = 1e-06,
  releps = 0
)
```

## Arguments

- x:

  The root to solve for with
  [`stats::uniroot()`](https://rdrr.io/r/stats/uniroot.html).

- hypotheses:

  A numeric vector of hypothesis weights. Must be a vector of values
  between 0 & 1 (inclusive). The sum of hypothesis weights should not
  exceed 1.

- test_corr:

  (Optional) A numeric matrix of correlations between test statistics,
  which is needed to perform parametric tests using
  [`adjust_weights_parametric()`](https://openpharma.github.io/graphicalMCP/reference/adjust_weights.md).
  The number of rows and columns of this correlation matrix should match
  the length of `p`.

- alpha:

  (Optional) A numeric value of the overall significance level, which
  should be between 0 & 1. The default is 0.025 for one-sided hypothesis
  testing problems; another common choice is 0.05 for two-sided
  hypothesis testing problems. Note when parametric tests are used, only
  one-sided tests are supported.

- maxpts:

  (Optional) An integer scalar for the maximum number of function
  values, which is needed to perform parametric tests using the
  [`mvtnorm::GenzBretz`](https://rdrr.io/pkg/mvtnorm/man/algorithms.html)
  algorithm. The default is 25000.

- abseps:

  (Optional) A numeric scalar for the absolute error tolerance, which is
  needed to perform parametric tests using the
  [`mvtnorm::GenzBretz`](https://rdrr.io/pkg/mvtnorm/man/algorithms.html)
  algorithm. The default is 1e-6.

- releps:

  (Optional) A numeric scalar for the relative error tolerance as
  double, which is needed to perform parametric tests using the
  [`mvtnorm::GenzBretz`](https://rdrr.io/pkg/mvtnorm/man/algorithms.html)
  algorithm. The default is 0.

## Value

- `c_value_function()` returns the difference between \\\alpha\\ and the
  Type I error of the parametric test with the \\c\\ value of `x`,
  adjusted for the correlation between test statistics using parametric
  tests based on equation (6) of Xi et al. (2017).

- `solve_c_parametric()` returns the c value adjusted for the
  correlation between test statistics using parametric tests based on
  equation (6) of Xi et al. (2017).

## References

Xi, D., Glimm, E., Maurer, W., and Bretz, F. (2017). A unified framework
for weighted parametric multiple test procedures. *Biometrical Journal*,
59(5), 918-931.

## See also

[`adjust_weights_parametric()`](https://openpharma.github.io/graphicalMCP/reference/adjust_weights.md)
for adjusted hypothesis weights using parametric tests.
