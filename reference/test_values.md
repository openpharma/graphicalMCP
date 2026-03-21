# Organize outputs for testing an intersection hypothesis

An intersection hypothesis can be tested by a mixture of test types
including Bonferroni, parametric and Simes tests. This function organize
outputs of testing and prepare them for `graph_report`.

## Usage

``` r
test_values_bonferroni(p, hypotheses, alpha, intersection = NA)

test_values_parametric(p, hypotheses, alpha, intersection = NA, test_corr)

test_values_simes(p, hypotheses, alpha, intersection = NA)

test_values_hochberg(p, hypotheses, alpha, intersection = NA)
```

## Arguments

- p:

  A numeric vector of p-values (unadjusted, raw), whose values should be
  between 0 & 1. The length should match the number of hypotheses in
  `graph`.

- hypotheses:

  A numeric vector of hypothesis weights in a graphical multiple
  comparison procedure. Must be a vector of values between 0 & 1
  (inclusive). The length should match the row and column lengths of
  `transitions`. The sum of hypothesis weights should not exceed 1.

- alpha:

  A numeric value of the overall significance level, which should be
  between 0 & 1. The default is 0.025 for one-sided hypothesis testing
  problems; another common choice is 0.05 for two-sided hypothesis
  testing problems. Note when parametric tests are used, only one-sided
  tests are supported.

- intersection:

  (optional) A numeric scalar used to name the intersection hypothesis
  in a weighting strategy.

- test_corr:

  (Optional) A list of numeric correlation matrices. Each entry in the
  list should correspond to each test group. For a test group using
  Bonferroni or Simes tests, its corresponding entry in `test_corr`
  should be `NA`. For a test group using parametric tests, its
  corresponding entry in `test_corr` should be a numeric correlation
  matrix specifying the correlation between test statistics for
  hypotheses in this test group. The length should match the number of
  elements in `test_groups`.

## Value

A data frame with rows corresponding to individual hypotheses involved
in the intersection hypothesis with hypothesis weights `hypotheses`.
There are following columns:

- `Intersection` - Name of this intersection hypothesis,

- `Hypothesis` - Name of an individual hypothesis,

- `Test` - Test type for an individual hypothesis,

- `p` - (Unadjusted or raw) p-values for a individual hypothesis,

- `c_value`- C value for parametric tests,

- `Weight` - Hypothesis weight for an individual hypothesis,

- `Alpha` - Overall significance level \\\alpha\\,

- `Inequality_holds` - Indicator to show if the p-value is less than or
  equal to its significance level.

  - For Bonferroni and Simes tests, the significance level is the
    hypothesis weight times \\\alpha\\.

  - For parametric tests, the significance level is the c value times
    the hypothesis weight times \\\alpha\\.

## References

Bretz, F., Maurer, W., Brannath, W., and Posch, M. (2009). A graphical
approach to sequentially rejective multiple test procedures. *Statistics
in Medicine*, 28(4), 586-604.

Lu, K. (2016). Graphical approaches using a Bonferroni mixture of
weighted Simes tests. *Statistics in Medicine*, 35(22), 4041-4055.

Xi, D., Glimm, E., Maurer, W., and Bretz, F. (2017). A unified framework
for weighted parametric multiple test procedures. *Biometrical Journal*,
59(5), 918-931.
