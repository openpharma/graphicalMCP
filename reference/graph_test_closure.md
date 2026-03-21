# Perform closed graphical multiple comparison procedures

Closed graphical multiple comparison procedures, or graphical multiple
comparison procedures based on the closure, generate the closure based
on a graph consisting of all intersection hypotheses. It tests each
intersection hypothesis and rejects an individual hypothesis if all
intersection hypotheses involving it have been rejected. An intersection
hypothesis represents the parameter space where individual null
hypotheses involved are true simultaneously.

For a graphical multiple comparison procedure with \$m\$ hypotheses,
there are \\2^m-1\\ intersection hypotheses. For each intersection
hypothesis, a test type could be chosen to determine how to reject the
intersection hypothesis. Current choices of test types include
Bonferroni, Simes and parametric. This implementation offers a more
general framework covering Bretz et al. (2011), Lu (2016), and Xi et al.
(2017). See
[`vignette("closed-testing")`](https://openpharma.github.io/graphicalMCP/articles/closed-testing.md)
for more illustration of closed test procedures and interpretation of
their outputs.

## Usage

``` r
graph_test_closure(
  graph,
  p,
  alpha = 0.025,
  test_groups = list(seq_along(graph$hypotheses)),
  test_types = c("bonferroni"),
  test_corr = rep(list(NA), length(test_types)),
  verbose = FALSE,
  test_values = FALSE
)
```

## Arguments

- graph:

  An initial graph as returned by
  [`graph_create()`](https://openpharma.github.io/graphicalMCP/reference/graph_create.md).

- p:

  A numeric vector of p-values (unadjusted, raw), whose values should be
  between 0 & 1. The length should match the number of hypotheses in
  `graph`.

- alpha:

  A numeric value of the overall significance level, which should be
  between 0 & 1. The default is 0.025 for one-sided hypothesis testing
  problems; another common choice is 0.05 for two-sided hypothesis
  testing problems. Note when parametric tests are used, only one-sided
  tests are supported.

- test_groups:

  A list of numeric vectors specifying hypotheses to test together.
  Grouping is needed to correctly perform Simes and parametric tests.

- test_types:

  A character vector of test types to apply to each test group. This is
  needed to correctly perform Simes and parametric tests. The length
  should match the number of elements in `test_groups`.

- test_corr:

  (Optional) A list of numeric correlation matrices. Each entry in the
  list should correspond to each test group. For a test group using
  Bonferroni or Simes tests, its corresponding entry in `test_corr`
  should be `NA`. For a test group using parametric tests, its
  corresponding entry in `test_corr` should be a numeric correlation
  matrix specifying the correlation between test statistics for
  hypotheses in this test group. The length should match the number of
  elements in `test_groups`.

- verbose:

  A logical scalar specifying whether the details of the adjusted
  p-value calculations should be included in results. When
  `verbose = TRUE`, adjusted p-values are provided for each intersection
  hypothesis. The default is `verbose = FALSE`.

- test_values:

  A logical scalar specifying whether adjusted significance levels
  should be provided for each hypothesis. When `test_values = TRUE`, it
  provides an equivalent way of performing graphical multiple comparison
  procedures by comparing each p-value with its significance level. If
  the p-value of a hypothesis is less than or equal to its significance
  level, the hypothesis is rejected. The default is
  `test_values = FALSE`.

## Value

A `graph_report` object with a list of 4 elements:

- `inputs` - Input parameters, which is a list of:

  - `graph` - Initial graph,

  - `p` - (Unadjusted or raw) p-values,

  - `alpha` - Overall significance level,

  - `test_groups` - Groups of hypotheses for different types of tests,

  - `test_types` - Different types of tests,

  - `test_corr` - Correlation matrices for parametric tests.

- `outputs` - Output parameters, which is a list of:

  - `adjusted_p` - Adjusted p-values,

  - `rejected` - Rejected hypotheses,

  - `graph` - Updated graph after deleting all rejected hypotheses.

- `details` - Verbose outputs with adjusted p-values for intersection
  hypotheses, if `verbose = TRUE`.

- `test_values` - Adjusted significance levels, if `test_values = TRUE`.

## Details for test specification

Test specification includes three components: `test_groups`,
`test_types`, and `test_corr`. Alignment among entries in these
components is important for correct implementation. There are two ways
to provide test specification. The first approach is the "unnamed"
approach, which assumes that all 3 components are ordered the same way,
i.e., the \$n\$-th element of `test_types` and `test_corr` should apply
to the \$n\$-th group in `test_groups`. The second "named" approach uses
the name of each element of each component to connect the element of
`test_types` and `test_corr` with the correct element of `test_groups`.
Consistency should be ensured for correct implementation.

## References

Bretz, F., Posch, M., Glimm, E., Klinglmueller, F., Maurer, W., and
Rohmeyer, K. (2011). Graphical approaches for multiple comparison
procedures using weighted Bonferroni, Simes, or parametric tests.
*Biometrical Journal*, 53(6), 894-913.

Lu, K. (2016). Graphical approaches using a Bonferroni mixture of
weighted Simes tests. *Statistics in Medicine*, 35(22), 4041-4055.

Xi, D., Glimm, E., Maurer, W., and Bretz, F. (2017). A unified framework
for weighted parametric multiple test procedures. *Biometrical Journal*,
59(5), 918-931.

## See also

[`graph_test_shortcut()`](https://openpharma.github.io/graphicalMCP/reference/graph_test_shortcut.md)
for shortcut graphical multiple comparison procedures.

## Examples

``` r
# A graphical multiple comparison procedure with two primary hypotheses
# (H1 and H2) and two secondary hypotheses (H3 and H4)
# See Figure 4 in Bretz et al. (2011).
hypotheses <- c(0.5, 0.5, 0, 0)
delta <- 0.5
transitions <- rbind(
  c(0, delta, 1 - delta, 0),
  c(delta, 0, 0, 1 - delta),
  c(0, 1, 0, 0),
  c(1, 0, 0, 0)
)
g <- graph_create(hypotheses, transitions)

p <- c(0.018, 0.01, 0.105, 0.006)
alpha <- 0.025

# Closed graphical multiple comparison procedure using Bonferroni tests
# Same results as `graph_test_shortcut(g, p, alpha)`
graph_test_closure(g, p, alpha)
#> 
#> Test parameters ($inputs) ------------------------------------------------------
#>   Initial graph
#> 
#>   --- Hypothesis weights ---
#>   H1: 0.5
#>   H2: 0.5
#>   H3: 0.0
#>   H4: 0.0
#> 
#>   --- Transition weights ---
#>       H1  H2  H3  H4
#>   H1 0.0 0.5 0.5 0.0
#>   H2 0.5 0.0 0.0 0.5
#>   H3 0.0 1.0 0.0 0.0
#>   H4 1.0 0.0 0.0 0.0
#> 
#>   Alpha = 0.025
#> 
#>                           H1    H2    H3    H4
#>   Unadjusted p-values: 0.018 0.010 0.105 0.006
#> 
#>   Test types
#>   bonferroni: (H1, H2, H3, H4)
#> 
#> Test summary ($outputs) --------------------------------------------------------
#>   Hypothesis Adj. P-value Reject
#>           H1        0.024   TRUE
#>           H2        0.020   TRUE
#>           H3        0.105  FALSE
#>           H4        0.024   TRUE
#> 
#>   Final updated graph after removing rejected hypotheses
#> 
#>   --- Hypothesis weights ---
#>   H1: NA
#>   H2: NA
#>   H3:  1
#>   H4: NA
#> 
#>   --- Transition weights ---
#>      H1 H2 H3 H4
#>   H1 NA NA NA NA
#>   H2 NA NA NA NA
#>   H3 NA NA  0 NA
#>   H4 NA NA NA NA
#> 

# Closed graphical multiple comparison procedure using parametric tests for
# H1 and H2, and Bonferroni tests for H3 and H4
set.seed(1234)
corr_list <- list(matrix(c(1, 0.5, 0.5, 1), nrow = 2), NA)
graph_test_closure(
  graph = g,
  p = p,
  alpha = alpha,
  test_groups = list(1:2, 3:4),
  test_types = c("parametric", "bonferroni"),
  test_corr = corr_list
)
#> 
#> Test parameters ($inputs) ------------------------------------------------------
#>   Initial graph
#> 
#>   --- Hypothesis weights ---
#>   H1: 0.5
#>   H2: 0.5
#>   H3: 0.0
#>   H4: 0.0
#> 
#>   --- Transition weights ---
#>       H1  H2  H3  H4
#>   H1 0.0 0.5 0.5 0.0
#>   H2 0.5 0.0 0.0 0.5
#>   H3 0.0 1.0 0.0 0.0
#>   H4 1.0 0.0 0.0 0.0
#> 
#>   Alpha = 0.025
#> 
#>                           H1    H2    H3    H4
#>   Unadjusted p-values: 0.018 0.010 0.105 0.006
#> 
#>   Correlation matrix:     H1  H2
#>                       H1 1.0 0.5
#>                       H2 0.5 1.0
#> 
#>   Test types
#>   parametric: (H1, H2)
#>   bonferroni: (H3, H4)
#> 
#> Test summary ($outputs) --------------------------------------------------------
#>   Hypothesis Adj. P-value Reject
#>           H1      0.02400   TRUE
#>           H2      0.01871   TRUE
#>           H3      0.10500  FALSE
#>           H4      0.02400   TRUE
#> 
#>   Final updated graph after removing rejected hypotheses
#> 
#>   --- Hypothesis weights ---
#>   H1: NA
#>   H2: NA
#>   H3:  1
#>   H4: NA
#> 
#>   --- Transition weights ---
#>      H1 H2 H3 H4
#>   H1 NA NA NA NA
#>   H2 NA NA NA NA
#>   H3 NA NA  0 NA
#>   H4 NA NA NA NA
#> 
# The "named" approach to obtain the same results
# Note that "group2" appears before "group1" in `test_groups`
set.seed(1234)
corr_list <- list(group1 = matrix(c(1, 0.5, 0.5, 1), nrow = 2), group2 = NA)
graph_test_closure(
  graph = g,
  p = p,
  alpha = alpha,
  test_groups = list(group1 = 1:2, group2 = 3:4),
  test_types = c(group2 = "bonferroni", group1 = "parametric"),
  test_corr = corr_list
)
#> 
#> Test parameters ($inputs) ------------------------------------------------------
#>   Initial graph
#> 
#>   --- Hypothesis weights ---
#>   H1: 0.5
#>   H2: 0.5
#>   H3: 0.0
#>   H4: 0.0
#> 
#>   --- Transition weights ---
#>       H1  H2  H3  H4
#>   H1 0.0 0.5 0.5 0.0
#>   H2 0.5 0.0 0.0 0.5
#>   H3 0.0 1.0 0.0 0.0
#>   H4 1.0 0.0 0.0 0.0
#> 
#>   Alpha = 0.025
#> 
#>                           H1    H2    H3    H4
#>   Unadjusted p-values: 0.018 0.010 0.105 0.006
#> 
#>   Correlation matrix:     H1  H2
#>                       H1 1.0 0.5
#>                       H2 0.5 1.0
#> 
#>   Test types
#>   parametric: (H1, H2)
#>   bonferroni: (H3, H4)
#> 
#> Test summary ($outputs) --------------------------------------------------------
#>   Hypothesis Adj. P-value Reject
#>           H1      0.02400   TRUE
#>           H2      0.01871   TRUE
#>           H3      0.10500  FALSE
#>           H4      0.02400   TRUE
#> 
#>   Final updated graph after removing rejected hypotheses
#> 
#>   --- Hypothesis weights ---
#>   H1: NA
#>   H2: NA
#>   H3:  1
#>   H4: NA
#> 
#>   --- Transition weights ---
#>      H1 H2 H3 H4
#>   H1 NA NA NA NA
#>   H2 NA NA NA NA
#>   H3 NA NA  0 NA
#>   H4 NA NA NA NA
#> 

# Closed graphical multiple comparison procedure using parametric tests for
# H1 and H2, and Simes tests for H3 and H4
set.seed(1234)
graph_test_closure(
  graph = g,
  p = p,
  alpha = alpha,
  test_groups = list(group1 = 1:2, group2 = 3:4),
  test_types = c(group1 = "parametric", group2 = "simes"),
  test_corr = corr_list
)
#> 
#> Test parameters ($inputs) ------------------------------------------------------
#>   Initial graph
#> 
#>   --- Hypothesis weights ---
#>   H1: 0.5
#>   H2: 0.5
#>   H3: 0.0
#>   H4: 0.0
#> 
#>   --- Transition weights ---
#>       H1  H2  H3  H4
#>   H1 0.0 0.5 0.5 0.0
#>   H2 0.5 0.0 0.0 0.5
#>   H3 0.0 1.0 0.0 0.0
#>   H4 1.0 0.0 0.0 0.0
#> 
#>   Alpha = 0.025
#> 
#>                           H1    H2    H3    H4
#>   Unadjusted p-values: 0.018 0.010 0.105 0.006
#> 
#>   Correlation matrix:     H1  H2
#>                       H1 1.0 0.5
#>                       H2 0.5 1.0
#> 
#>   Test types
#>   parametric: (H1, H2)
#>        simes: (H3, H4)
#> 
#> Test summary ($outputs) --------------------------------------------------------
#>   Hypothesis Adj. P-value Reject
#>           H1      0.02400   TRUE
#>           H2      0.01871   TRUE
#>           H3      0.10500  FALSE
#>           H4      0.02400   TRUE
#> 
#>   Final updated graph after removing rejected hypotheses
#> 
#>   --- Hypothesis weights ---
#>   H1: NA
#>   H2: NA
#>   H3:  1
#>   H4: NA
#> 
#>   --- Transition weights ---
#>      H1 H2 H3 H4
#>   H1 NA NA NA NA
#>   H2 NA NA NA NA
#>   H3 NA NA  0 NA
#>   H4 NA NA NA NA
#> 
```
