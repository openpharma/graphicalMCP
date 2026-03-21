# S3 print method for the class `initial_graph`

A printed `initial_graph` displays a header stating "Initial graph",
hypothesis weights, and transition weights.

## Usage

``` r
# S3 method for class 'initial_graph'
print(x, ..., precision = 4, indent = 0)
```

## Arguments

- x:

  An object of class `initial_graph` to print.

- ...:

  Other values passed on to other methods (currently unused).

- precision:

  An integer scalar indicating the number of decimal places to to
  display.

- indent:

  An integer scalar indicating how many spaces to indent results.

## Value

An object x of class `initial_graph`, after printing the initial graph.

## References

Bretz, F., Posch, M., Glimm, E., Klinglmueller, F., Maurer, W., and
Rohmeyer, K. (2011). Graphical approaches for multiple comparison
procedures using weighted Bonferroni, Simes, or parametric tests.
*Biometrical Journal*, 53(6), 894-913.

## See also

[`print.updated_graph()`](https://openpharma.github.io/graphicalMCP/reference/print.updated_graph.md)
for the print method for the updated graph after hypotheses being
deleted from the initial graph.

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
hyp_names <- c("H11", "H12", "H21", "H22")
g <- graph_create(hypotheses, transitions, hyp_names)
g
#> Initial graph
#> 
#> --- Hypothesis weights ---
#> H11: 0.5
#> H12: 0.5
#> H21: 0.0
#> H22: 0.0
#> 
#> --- Transition weights ---
#>      H11 H12 H21 H22
#>  H11   0   0   1   0
#>  H12   0   0   0   1
#>  H21   0   1   0   0
#>  H22   1   0   0   0
```
