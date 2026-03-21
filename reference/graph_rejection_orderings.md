# Find alternate rejection orderings (sequences) for shortcut tests

When multiple hypotheses are rejected by using
[`graph_test_shortcut()`](https://openpharma.github.io/graphicalMCP/reference/graph_test_shortcut.md),
there may be multiple orderings or sequences in which hypotheses are
rejected one by one. The default order in
[`graph_test_shortcut()`](https://openpharma.github.io/graphicalMCP/reference/graph_test_shortcut.md)
is based on the adjusted p-values, from the smallest to the largest.
This function `graph_rejection_orderings()` provides all possible and
valid orders (or sequences) of rejections. Although the order of
rejection does not affect the final rejection decisions Bretz et al.
(2009), different sequences could offer different ways to explain the
step-by-step process of shortcut graphical multiple comparison
procedures.

## Usage

``` r
graph_rejection_orderings(shortcut_test_result)
```

## Arguments

- shortcut_test_result:

  A `graph_report` object as returned by
  [`graph_test_shortcut()`](https://openpharma.github.io/graphicalMCP/reference/graph_test_shortcut.md).

## Value

A modified `graph_report` object containing all valid orderings of
rejections of hypotheses

## References

Bretz, F., Maurer, W., Brannath, W., and Posch, M. (2009). A graphical
approach to sequentially rejective multiple test procedures. *Statistics
in Medicine*, 28(4), 586-604.

Bretz, F., Posch, M., Glimm, E., Klinglmueller, F., Maurer, W., and
Rohmeyer, K. (2011). Graphical approaches for multiple comparison
procedures using weighted Bonferroni, Simes, or parametric tests.
*Biometrical Journal*, 53(6), 894-913.

## See also

[`graph_test_shortcut()`](https://openpharma.github.io/graphicalMCP/reference/graph_test_shortcut.md)
for shortcut graphical multiple comparison procedures.

## Examples

``` r
# A graphical multiple comparison procedure with two primary hypotheses (H1
# and H2) and two secondary hypotheses (H3 and H4)
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

shortcut_testing <- graph_test_shortcut(g, p, alpha, verbose = TRUE)

# Reject H1, H2, and H4
shortcut_testing$outputs$rejected
#>    H1    H2    H3    H4 
#>  TRUE  TRUE FALSE  TRUE 

# Default order of rejections: H2, H1, H4
shortcut_testing$details$del_seq
#> [1] "H2" "H1" "H4"

# There is another valid sequence of rejection: H2, H4, H1
graph_rejection_orderings(shortcut_testing)$valid_orderings
#> [[1]]
#> H2 H1 H4 
#>  2  1  4 
#> 
#> [[2]]
#> H2 H4 H1 
#>  2  4  1 
#> 

# Finally, intermediate updated graphs can be obtained by providing the order
# of rejections into `[graph_update()]`
graph_update(g, delete = c(2, 4, 1))
#> Initial and final graphs -------------------------------------------------------
#> 
#> Initial graph
#> 
#> --- Hypothesis weights ---
#> H1: 0.5
#> H2: 0.5
#> H3: 0.0
#> H4: 0.0
#> 
#> --- Transition weights ---
#>      H1  H2  H3  H4
#>  H1 0.0 0.5 0.5 0.0
#>  H2 0.5 0.0 0.0 0.5
#>  H3 0.0 1.0 0.0 0.0
#>  H4 1.0 0.0 0.0 0.0
#> 
#> Updated graph after deleting hypotheses 2, 4, 1
#> 
#> --- Hypothesis weights ---
#> H1: NA
#> H2: NA
#> H3:  1
#> H4: NA
#> 
#> --- Transition weights ---
#>     H1 H2 H3 H4
#>  H1 NA NA NA NA
#>  H2 NA NA NA NA
#>  H3 NA NA  0 NA
#>  H4 NA NA NA NA
#> 
#> Deletion sequence ($intermediate_graphs) ---------------------------------------
#> 
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
#>     Step 1: Updated graph after removing hypothesis 2
#> 
#>     --- Hypothesis weights ---
#>     H1: 0.75
#>     H2:   NA
#>     H3: 0.00
#>     H4: 0.25
#> 
#>     --- Transition weights ---
#>              H1       H2       H3       H4
#>     H1 0.000000       NA 0.666667 0.333333
#>     H2       NA       NA       NA       NA
#>     H3 0.500000       NA 0.000000 0.500000
#>     H4 1.000000       NA 0.000000 0.000000
#> 
#>       Step 2: Updated graph after removing hypotheses 2, 4
#> 
#>       --- Hypothesis weights ---
#>       H1:  1
#>       H2: NA
#>       H3:  0
#>       H4: NA
#> 
#>       --- Transition weights ---
#>          H1 H2 H3 H4
#>       H1  0 NA  1 NA
#>       H2 NA NA NA NA
#>       H3  1 NA  0 NA
#>       H4 NA NA NA NA
#> 
#>         Step 3: Updated graph after removing hypotheses 2, 4, 1
#> 
#>         --- Hypothesis weights ---
#>         H1: NA
#>         H2: NA
#>         H3:  1
#>         H4: NA
#> 
#>         --- Transition weights ---
#>            H1 H2 H3 H4
#>         H1 NA NA NA NA
#>         H2 NA NA NA NA
#>         H3 NA NA  0 NA
#>         H4 NA NA NA NA
#> 
#>   Final updated graph after removing deleted hypotheses
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
