# Perform graphical multiple comparison procedures efficiently for power calculation

These functions performs similarly to
[`graph_test_closure()`](https://openpharma.github.io/graphicalMCP/reference/graph_test_closure.md)
or
[`graph_test_shortcut()`](https://openpharma.github.io/graphicalMCP/reference/graph_test_shortcut.md)
but are optimized for efficiently calculating power. For example,
generating weights and calculating adjusted weights can be done only
once. Vectorization has been applied where possible.

## Usage

``` r
graph_test_closure_fast(p, alpha, adjusted_weights, matrix_intersections)

graph_test_shortcut_fast(p, alpha, adjusted_weights)
```

## Arguments

- p:

  A numeric vector of one-sided p-values (unadjusted, raw), whose values
  should be between 0 & 1. The length should match the number of
  hypotheses in `graph`.

- alpha:

  A numeric value of the one-sided overall significance level, which
  should be between 0 & 1. The default is 0.025 for one-sided hypothesis
  testing. Note that only one-sided tests are supported.

- adjusted_weights:

  The adjusted hypothesis weights, which are the second half of columns
  from
  [`graph_generate_weights()`](https://openpharma.github.io/graphicalMCP/reference/graph_generate_weights.md)
  output, adjusted by the appropriate test types (Bonferroni, Simes, or
  parametric).

- matrix_intersections:

  A matrix of hypothesis indicators in a weighting strategy, which are
  the first half the
  [`graph_generate_weights()`](https://openpharma.github.io/graphicalMCP/reference/graph_generate_weights.md)
  output.

## Value

A logical or integer vector indicating whether each hypothesis can be
rejected or not.

## See also

- [`graph_test_closure()`](https://openpharma.github.io/graphicalMCP/reference/graph_test_closure.md)
  for closed graphical multiple comparison procedures.

- [`graph_test_shortcut()`](https://openpharma.github.io/graphicalMCP/reference/graph_test_shortcut.md)
  for shortcut graphical multiple comparison procedures.
