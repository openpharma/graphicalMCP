# Calculate adjusted p-values

For an intersection hypothesis, an adjusted p-value is the smallest
significance level at which the intersection hypothesis can be rejected.
The intersection hypothesis can be rejected if its adjusted p-value is
less than or equal to \\\alpha\\. Currently, there are three test types
supported:

- Bonferroni tests for `adjust_p_bonferroni()`,

- Parametric tests for `adjust_p_parametric()`,

  - Note that one-sided tests are required for parametric tests.

- Simes tests for `adjust_p_simes()`,

- Hochberg tests for `adjust_p_hochberg()`.

## Usage

``` r
adjust_p_bonferroni(p, hypotheses)

adjust_p_parametric(
  p,
  hypotheses,
  test_corr = NULL,
  maxpts = 25000,
  abseps = 1e-06,
  releps = 0
)

adjust_p_simes(p, hypotheses)

adjust_p_hochberg(p, hypotheses)
```

## Arguments

- p:

  A numeric vector of p-values (unadjusted, raw), whose values should be
  between 0 & 1. The length should match the length of `hypotheses`.

- hypotheses:

  A numeric vector of hypothesis weights. Must be a vector of values
  between 0 & 1 (inclusive). The length should match the length of `p`.
  The sum of hypothesis weights should not exceed 1.

- test_corr:

  (Optional) A numeric matrix of correlations between test statistics,
  which is needed to perform parametric tests using
  `adjust_p_parametric()`. The number of rows and columns of this
  correlation matrix should match the length of `p`.

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

A single adjusted p-value for the intersection hypothesis.

## References

Bretz, F., Maurer, W., Brannath, W., and Posch, M. (2009). A graphical
approach to sequentially rejective multiple test procedures. *Statistics
in Medicine*, 28(4), 586-604.

Lu, K. (2016). Graphical approaches using a Bonferroni mixture of
weighted Simes tests. *Statistics in Medicine*, 35(22), 4041-4055.

Xi, D., Glimm, E., Maurer, W., and Bretz, F. (2017). A unified framework
for weighted parametric multiple test procedures. *Biometrical Journal*,
59(5), 918-931.

Xi, D., and Bretz, F. (2019). Symmetric graphs for equally weighted
tests, with application to the Hochberg procedure. *Statistics in
Medicine*, 38(27), 5268-5282.

## See also

[`adjust_weights_parametric()`](https://openpharma.github.io/graphicalMCP/reference/adjust_weights.md)
for adjusted hypothesis weights using parametric tests,
[`adjust_weights_simes()`](https://openpharma.github.io/graphicalMCP/reference/adjust_weights.md)
for adjusted hypothesis weights using Simes tests,
[`adjust_weights_hochberg()`](https://openpharma.github.io/graphicalMCP/reference/adjust_weights.md)
for adjusted hypothesis weights using Hochberg tests.

## Examples

``` r
hypotheses <- c(H1 = 0.5, H2 = 0.25, H3 = 0.25)
p <- c(0.019, 0.025, 0.05)
adjust_p_bonferroni(p, hypotheses)
#> [1] 0.038
set.seed(1234)
hypotheses <- c(H1 = 0.5, H2 = 0.25, H3 = 0.25)
p <- c(0.019, 0.025, 0.05)
# Using the `mvtnorm::GenzBretz` algorithm
corr <- matrix(0.5, nrow = 3, ncol = 3)
diag(corr) <- 1
adjust_p_parametric(p, hypotheses, corr)
#> [1] 0.03343516
hypotheses <- c(H1 = 0.5, H2 = 0.25, H3 = 0.25)
p <- c(0.019, 0.025, 0.05)
adjust_p_simes(p, hypotheses)
#> [1] 0.03333333
hypotheses <- c(H1 = .25, H2 = .25, H3 = 0.25, H4 = 0.25)
p <- c(0.019, 0.025, 0.05, .05)
adjust_p_hochberg(p, hypotheses)
#> [1] 0.05
```
