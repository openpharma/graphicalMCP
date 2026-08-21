# S3 print method for the class `graph_report`

A printed `graph_report` displays the initial graph, p-values and
significance levels, rejection decisions, and optional detailed test
results.

## Usage

``` r
# S3 method for class 'graph_report'
print(x, ..., precision = 4, indent = 2, rows = 10)
```

## Arguments

- x:

  An object of class `graph_report` to print.

- ...:

  Other values passed on to other methods (currently unused)

- precision:

  An integer scalar indicating the number of decimal places to to
  display.

- indent:

  An integer scalar indicating how many spaces to indent results.

- rows:

  An integer scalar indicating how many rows of detailed test results to
  print.

## Value

An object x of class `graph_report`, after printing the report of
conducting a graphical multiple comparison procedure.

## References

Bretz, F., Posch, M., Glimm, E., Klinglmueller, F., Maurer, W., and
Rohmeyer, K. (2011). Graphical approaches for multiple comparison
procedures using weighted Bonferroni, Simes, or parametric tests.
*Biometrical Journal*, 53(6), 894-913.

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
#>   Hypothesis Adj.p Reject
#>           H1 0.020   TRUE
#>           H2 0.020   TRUE
#>           H3 0.105  FALSE
#>           H4 0.020   TRUE
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
