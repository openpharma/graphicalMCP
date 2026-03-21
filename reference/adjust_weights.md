# Calculate adjusted hypothesis weights

An intersection hypothesis can be rejected if its p-values are less than
or equal to their adjusted significance levels, which are their adjusted
hypothesis weights times \\\alpha\\. For Bonferroni tests, their
adjusted hypothesis weights are their hypothesis weights of the
intersection hypothesis. Additional adjustment is needed for parametric,
Simes, and Hochberg tests:

- Parametric tests for `adjust_weights_parametric()`,

  - Note that one-sided tests are required for parametric tests.

- Simes tests for `adjust_weights_simes()`,

- Hochberg tests for `adjust_weights_hochberg()`.

## Usage

``` r
adjust_weights_parametric(
  matrix_weights,
  matrix_intersections,
  test_corr,
  alpha,
  test_groups,
  ...
)

adjust_weights_simes(matrix_weights, p, test_groups)

adjust_weights_hochberg(matrix_weights, matrix_intersections, p, test_groups)
```

## Arguments

- matrix_weights:

  (Optional) A matrix of hypothesis weights of all intersection
  hypotheses. This can be obtained as the second half of columns from
  the output of
  [`graph_generate_weights()`](https://openpharma.github.io/graphicalMCP/reference/graph_generate_weights.md).

- matrix_intersections:

  (Optional) A matrix of hypothesis indicators of all intersection
  hypotheses. This can be obtained as the first half of columns from the
  output of
  [`graph_generate_weights()`](https://openpharma.github.io/graphicalMCP/reference/graph_generate_weights.md).

- test_corr:

  (Optional) A numeric matrix of correlations between test statistics,
  which is needed to perform parametric tests using
  `adjust_weights_parametric()`. The number of rows and columns of this
  correlation matrix should match the length of `p`.

- alpha:

  (Optional) A numeric value of the overall significance level, which
  should be between 0 & 1. The default is 0.025 for one-sided hypothesis
  testing problems; another common choice is 0.05 for two-sided
  hypothesis testing problems. Note when parametric tests are used, only
  one-sided tests are supported.

- test_groups:

  (Optional) A list of numeric vectors specifying hypotheses to test
  together. Grouping is needed to correctly perform Simes and parametric
  tests.

- ...:

  Additional arguments to perform parametric tests using the
  [`mvtnorm::GenzBretz`](https://rdrr.io/pkg/mvtnorm/man/algorithms.html)
  algorithm. `maxpts` is an integer scalar for the maximum number of
  function values, whose default value is 25000. `abseps` is a numeric
  scalar for the absolute error tolerance, whose default value is 1e-6.
  `releps` is a numeric scalar for the relative error tolerance as
  double, whose default value is 0.

- p:

  (Optional) A numeric vector of p-values (unadjusted, raw), whose
  values should be between 0 & 1. The length should match the number of
  columns of `matrix_weights`.

## Value

- `adjust_weights_parametric()` returns a matrix with the same
  dimensions as `matrix_weights`, whose hypothesis weights have been
  adjusted according to parametric tests.

- `adjust_weights_simes()` returns a matrix with the same dimensions as
  `matrix_weights`, whose hypothesis weights have been adjusted
  according to Simes tests.

- `adjust_weights_hochberg()` returns a matrix with the same dimensions
  as `matrix_weights`, whose hypothesis weights have been adjusted
  according to Hochberg tests.

## References

Lu, K. (2016). Graphical approaches using a Bonferroni mixture of
weighted Simes tests. *Statistics in Medicine*, 35(22), 4041-4055.

Xi, D., Glimm, E., Maurer, W., and Bretz, F. (2017). A unified framework
for weighted parametric multiple test procedures. *Biometrical Journal*,
59(5), 918-931.

Xi, D., and Bretz, F. (2019). Symmetric graphs for equally weighted
tests, with application to the Hochberg procedure. *Statistics in
Medicine*, 38(27), 5268-5282.

## See also

[`adjust_p_parametric()`](https://openpharma.github.io/graphicalMCP/reference/adjust_p.md)
for adjusted p-values using parametric tests,
[`adjust_p_simes()`](https://openpharma.github.io/graphicalMCP/reference/adjust_p.md)
for adjusted p-values using Simes tests,
[`adjust_p_hochberg()`](https://openpharma.github.io/graphicalMCP/reference/adjust_p.md)
for adjusted p-values using Hochberg tests.

## Examples

``` r
alpha <- 0.025
num_hyps <- 4
g <- bonferroni_holm(num_hyps)
weighting_strategy <- graph_generate_weights(g)
matrix_intersections <- weighting_strategy[, seq_len(num_hyps)]
matrix_weights <- weighting_strategy[, -seq_len(num_hyps)]

set.seed(1234)
adjust_weights_parametric(
  matrix_weights = matrix_weights,
  matrix_intersections = matrix_intersections,
  test_corr = list(diag(2), diag(2)),
  alpha = alpha,
  test_groups = list(1:2, 3:4)
)
#>           H1        H2        H3        H4
#> 1  0.2507911 0.2507911 0.2507911 0.2507911
#> 2  0.3347254 0.3347254 0.3333333 0.0000000
#> 3  0.3347254 0.3347254 0.0000000 0.3333333
#> 4  0.5031647 0.5031647 0.0000000 0.0000000
#> 5  0.3333333 0.0000000 0.3347254 0.3347254
#> 6  0.5000000 0.0000000 0.5000000 0.0000000
#> 7  0.5000000 0.0000000 0.0000000 0.5000000
#> 8  1.0000000 0.0000000 0.0000000 0.0000000
#> 9  0.0000000 0.3333333 0.3347254 0.3347254
#> 10 0.0000000 0.5000000 0.5000000 0.0000000
#> 11 0.0000000 0.5000000 0.0000000 0.5000000
#> 12 0.0000000 1.0000000 0.0000000 0.0000000
#> 13 0.0000000 0.0000000 0.5031647 0.5031647
#> 14 0.0000000 0.0000000 1.0000000 0.0000000
#> 15 0.0000000 0.0000000 0.0000000 1.0000000
alpha <- 0.025
p <- c(0.018, 0.01, 0.105, 0.006)
num_hyps <- length(p)
g <- bonferroni_holm(num_hyps)
weighting_strategy <- graph_generate_weights(g)
matrix_intersections <- weighting_strategy[, seq_len(num_hyps)]
matrix_weights <- weighting_strategy[, -seq_len(num_hyps)]

adjust_weights_simes(
  matrix_weights = matrix_weights,
  p = p,
  test_groups = list(1:2, 3:4)
)
#>           H4        H2        H1        H3
#> 1  0.2500000 0.2500000 0.5000000 0.5000000
#> 2  0.0000000 0.3333333 0.6666667 0.3333333
#> 3  0.3333333 0.3333333 0.6666667 0.3333333
#> 4  0.0000000 0.5000000 1.0000000 0.0000000
#> 5  0.3333333 0.0000000 0.3333333 0.6666667
#> 6  0.0000000 0.0000000 0.5000000 0.5000000
#> 7  0.5000000 0.0000000 0.5000000 0.5000000
#> 8  0.0000000 0.0000000 1.0000000 0.0000000
#> 9  0.3333333 0.3333333 0.3333333 0.6666667
#> 10 0.0000000 0.5000000 0.5000000 0.5000000
#> 11 0.5000000 0.5000000 0.5000000 0.5000000
#> 12 0.0000000 1.0000000 1.0000000 0.0000000
#> 13 0.5000000 0.0000000 0.0000000 1.0000000
#> 14 0.0000000 0.0000000 0.0000000 1.0000000
#> 15 1.0000000 0.0000000 0.0000000 1.0000000
alpha <- 0.025
p <- c(0.018, 0.01, 0.105, 0.006)
num_hyps <- length(p)
g <- bonferroni_holm(num_hyps)
weighting_strategy <- graph_generate_weights(g)
matrix_intersections <- weighting_strategy[, seq_len(num_hyps)]
matrix_weights <- weighting_strategy[, -seq_len(num_hyps)]

adjust_weights_hochberg(
  matrix_weights = matrix_weights,
  matrix_intersections = matrix_intersections,
  p = p,
  test_groups = list(1:2, 3:4)
)
#>           H1        H2        H3        H4
#> 1  0.5000000 0.2500000 0.5000000 0.2500000
#> 2  0.6666667 0.3333333 0.3333333 0.0000000
#> 3  0.6666667 0.3333333 0.0000000 0.3333333
#> 4  1.0000000 0.5000000 0.0000000 0.0000000
#> 5  0.3333333 0.0000000 0.6666667 0.3333333
#> 6  0.5000000 0.0000000 0.5000000 0.0000000
#> 7  0.5000000 0.0000000 0.0000000 0.5000000
#> 8  1.0000000 0.0000000 0.0000000 0.0000000
#> 9  0.0000000 0.3333333 0.6666667 0.3333333
#> 10 0.0000000 0.5000000 0.5000000 0.0000000
#> 11 0.0000000 0.5000000 0.0000000 0.5000000
#> 12 0.0000000 1.0000000 0.0000000 0.0000000
#> 13 0.0000000 0.0000000 1.0000000 0.5000000
#> 14 0.0000000 0.0000000 1.0000000 0.0000000
#> 15 0.0000000 0.0000000 0.0000000 1.0000000
```
