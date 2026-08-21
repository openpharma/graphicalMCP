# S3 print method for the class `gsd_graph_report`

A printed `gsd_graph_report` displays:

- **Test parameters**: the initial graph, alpha, information fractions,
  p-values, spending functions, and per-hypothesis look_back settings.

- **Test summary**: adjusted p-values, rejection decisions, the analysis
  at which each decision was made (`Decision.at`), the earliest analysis
  at which the boundary was crossed (`First.Rej.at`), look_back status,
  and the rejection sequence.

- **Per-analysis details** (if `test_values = TRUE`): nominal p-values,
  boundaries, and rejection decisions at each analysis. For hypotheses
  rejected via look_back, additional rows show the boundary crossing at
  earlier analyses, marked with `*` and a footnote.

- **Boundary table** (if `verbose = TRUE`): nominal p-value boundaries
  for all possible hypothesis weights from the graph's closure, enabling
  manual verification of rejection decisions.

## Usage

``` r
# S3 method for class 'gsd_graph_report'
print(x, ..., precision = 6, indent = 2)
```

## Arguments

- x:

  An object of class `gsd_graph_report` to print.

- ...:

  Other values passed on to other methods (currently unused).

- precision:

  An integer scalar indicating the number of decimal places to display.

- indent:

  An integer scalar indicating how many spaces to indent results.

## Value

An object x of class `gsd_graph_report`, invisibly.

## References

Maurer, W., and Bretz, F. (2013). Multiple testing in group sequential
trials using graphical approaches. *Statistics in Biopharmaceutical
Research*, 5(4), 311-320.

## Examples

``` r
hypotheses <- c(0.5, 0.5)
transitions <- rbind(c(0, 1), c(1, 0))
g <- graph_create(hypotheses, transitions)

p <- rbind(
  H1 = c(0.024, 0.01),
  H2 = c(0.015, 0.005)
)

graph_test_shortcut_gsd(
  graph = g,
  p = p,
  alpha = 0.025,
  info_frac = c(0.5, 1),
  spending_fn = spending_of
)
#> 
#> Test parameters ($inputs) ------------------------------------------------------
#>   Initial graph
#> 
#>   --- Hypothesis weights ---
#>   H1: 0.5
#>   H2: 0.5
#> 
#>   --- Transition weights ---
#>      H1 H2
#>   H1  0  1
#>   H2  1  0
#> 
#>   Alpha = 0.025
#> 
#>   Information fractions
#>    Analysis_1 Analysis_2
#> H1        0.5          1
#> H2        0.5          1
#> 
#>   P-values
#>    Analysis_1 Analysis_2
#> H1   0.024000   0.010000
#> H2   0.015000   0.005000
#> 
#>   Spending functions
#>     H1: O'Brien-Fleming
#>     H2: O'Brien-Fleming
#> 
#>   Look back = FALSE
#> 
#> Test summary ($outputs) --------------------------------------------------------
#>   Hypothesis   Adj.p* Reject Tested.at First.Rej.at Last.Rej.at Look.back
#>           H1 0.010094   TRUE         2            2           2     FALSE
#>           H2 0.010051   TRUE         2            2           2     FALSE
#>   (*) Adjusted p-values account for both the group sequential design and the
#>       graphical multiple comparison procedure. Based on repeated p-values when
#>       look_back = FALSE, and sequential p-values when look_back = TRUE.
#> 
#>   Rejection sequence: H2 -> H1
#> 
#>   Final updated graph after removing rejected hypotheses
#> 
#>   --- Hypothesis weights ---
#>   H1: NA
#>   H2: NA
#> 
#>   --- Transition weights ---
#>      H1 H2
#>   H1 NA NA
#>   H2 NA NA
#> 
```
