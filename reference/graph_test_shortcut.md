# Perform shortcut (sequentially rejective) graphical multiple comparison procedures

Shortcut graphical multiple comparison procedures are sequentially
rejective procedure based on Bretz et al. (2009). With \$m\$ hypotheses,
there are at most \$m\$ steps to obtain all rejection decisions. These
procedure are equivalent to closed graphical multiple comparison
procedures using Bonferroni tests for intersection hypotheses, but
shortcut procedures are faster to perform. See
[`vignette("shortcut-testing")`](https://openpharma.github.io/graphicalMCP/articles/shortcut-testing.md)
for more illustration of shortcut procedures and interpretation of their
outputs.

## Usage

``` r
graph_test_shortcut(
  graph,
  p,
  alpha = 0.025,
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

  A numeric scalar of the overall significance level, which should be
  between 0 & 1. The default is 0.025 for one-sided hypothesis testing
  problems; another common choice is 0.05 for two-sided hypothesis
  testing problems.

- verbose:

  A logical scalar specifying whether the details of intermediate update
  graphs should be included in results. When `verbose = TRUE`,
  intermediate update graphs are provided after deleting each
  hypothesis, which has been rejected. The default is `verbose = FALSE`.

- test_values:

  A logical scalar specifying whether adjusted significance levels
  should be provided for each hypothesis. When `test_values = TRUE`, it
  provides an equivalent way of performing graphical multiple comparison
  procedures by comparing each p-value with its significance level. If
  the p-value of a hypothesis is less than or equal to its significance
  level, the hypothesis is rejected. The order of rejection is based on
  the order of adjusted p-values from the smallest to the largest. The
  default is `test_values = FALSE`.

## Value

An S3 object of class `graph_report` with a list of 4 elements:

- `inputs` - Input parameters, which is a list of:

  - `graph` - Initial graph, \*`p` - (Unadjusted or raw) p-values,

  - `alpha` - Overall significance level,

  - `test_groups` - Groups of hypotheses for different types of tests,
    which are the list of all hypotheses for `graph_test_shortcut()`,

  - `test_types` - Different types of tests, which are "bonferroni" for
    `graph_test_shortcut()`.

- Output parameters `outputs`, which is a list of:

  - `adjusted_p` - Adjusted p-values,

  - `rejected` - Rejected hypotheses,

  - `graph` - Updated graph after deleting all rejected hypotheses.

- `details` - Verbose outputs with intermediate updated graphs, if
  `verbose = TRUE`.

- `test_values` - Adjusted significance levels, if `test_values = TRUE`.

## References

Bretz, F., Maurer, W., Brannath, W., and Posch, M. (2009). A graphical
approach to sequentially rejective multiple test procedures. *Statistics
in Medicine*, 28(4), 586-604.

Bretz, F., Posch, M., Glimm, E., Klinglmueller, F., Maurer, W., and
Rohmeyer, K. (2011). Graphical approaches for multiple comparison
procedures using weighted Bonferroni, Simes, or parametric tests.
*Biometrical Journal*, 53(6), 894-913.

## See also

- [`graph_test_closure()`](https://openpharma.github.io/graphicalMCP/reference/graph_test_closure.md)
  for graphical multiple comparison procedures using the closed test,

- [`graph_rejection_orderings()`](https://openpharma.github.io/graphicalMCP/reference/graph_rejection_orderings.md)
  for all possible rejection orderings.

## Examples

``` r
# A graphical multiple comparison procedure with two primary hypotheses (H1
# and H2) and two secondary hypotheses (H3 and H4)
# See Figure 1 in Bretz et al. (2011).
hypotheses <- c(0.5, 0.5, 0, 0)
transitions <- rbind(
  c(0, 0, 1, 0),
  c(0, 0, 0, 1),
  c(0, 1, 0, 0),
  c(1, 0, 0, 0)
)
g <- graph_create(hypotheses, transitions)

p <- c(0.018, 0.01, 0.105, 0.006)
alpha <- 0.025
graph_test_shortcut(g, p, alpha)
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
#>      H1 H2 H3 H4
#>   H1  0  0  1  0
#>   H2  0  0  0  1
#>   H3  0  1  0  0
#>   H4  1  0  0  0
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
#>           H1        0.020   TRUE
#>           H2        0.020   TRUE
#>           H3        0.105  FALSE
#>           H4        0.020   TRUE
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
