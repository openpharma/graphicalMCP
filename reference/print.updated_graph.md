# S3 print method for the class `updated_graph`

A printed `updated_graph` displays the initial graph, the (final)
updated graph, and the sequence of intermediate updated graphs after
hypotheses are deleted (if available).

## Usage

``` r
# S3 method for class 'updated_graph'
print(x, ..., precision = 6, indent = 2)
```

## Arguments

- x:

  An object of the class `updated_graph` to print.

- ...:

  Other values passed on to other methods (currently unused).

- precision:

  An integer scalar indicating the number of decimal places to to
  display.

- indent:

  An integer scalar indicating how many spaces to indent results.

## Value

An object x of the class `updated_graph`, after printing the updated
graph.

## References

Bretz, F., Posch, M., Glimm, E., Klinglmueller, F., Maurer, W., and
Rohmeyer, K. (2011a). Graphical approaches for multiple comparison
procedures using weighted Bonferroni, Simes, or parametric tests.
*Biometrical Journal*, 53(6), 894-913.

## See also

[`print.initial_graph()`](https://openpharma.github.io/graphicalMCP/reference/print.initial_graph.md)
for the print method for the initial graph.

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

# Delete the second and third hypotheses in the "unordered mode"
graph_update(g, delete = c(FALSE, TRUE, TRUE, FALSE))
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
#>     H1 H2 H3 H4
#>  H1  0  0  1  0
#>  H2  0  0  0  1
#>  H3  0  1  0  0
#>  H4  1  0  0  0
#> 
#> Updated graph after deleting hypotheses 2, 3
#> 
#> --- Hypothesis weights ---
#> H1: 0.5
#> H2:  NA
#> H3:  NA
#> H4: 0.5
#> 
#> --- Transition weights ---
#>     H1 H2 H3 H4
#>  H1  0 NA NA  1
#>  H2 NA NA NA NA
#>  H3 NA NA NA NA
#>  H4  1 NA NA  0

# Equivalent way in the "ordered mode" to obtain the updated graph after
# deleting the second and third hypotheses
# Additional intermediate updated graphs are also provided
graph_update(g, delete = 2:3)
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
#>     H1 H2 H3 H4
#>  H1  0  0  1  0
#>  H2  0  0  0  1
#>  H3  0  1  0  0
#>  H4  1  0  0  0
#> 
#> Updated graph after deleting hypotheses 2, 3
#> 
#> --- Hypothesis weights ---
#> H1: 0.5
#> H2:  NA
#> H3:  NA
#> H4: 0.5
#> 
#> --- Transition weights ---
#>     H1 H2 H3 H4
#>  H1  0 NA NA  1
#>  H2 NA NA NA NA
#>  H3 NA NA NA NA
#>  H4  1 NA NA  0
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
#>      H1 H2 H3 H4
#>   H1  0  0  1  0
#>   H2  0  0  0  1
#>   H3  0  1  0  0
#>   H4  1  0  0  0
#> 
#>     Step 1: Updated graph after removing hypothesis 2
#> 
#>     --- Hypothesis weights ---
#>     H1: 0.5
#>     H2:  NA
#>     H3: 0.0
#>     H4: 0.5
#> 
#>     --- Transition weights ---
#>        H1 H2 H3 H4
#>     H1  0 NA  1  0
#>     H2 NA NA NA NA
#>     H3  0 NA  0  1
#>     H4  1 NA  0  0
#> 
#>       Step 2: Updated graph after removing hypotheses 2, 3
#> 
#>       --- Hypothesis weights ---
#>       H1: 0.5
#>       H2:  NA
#>       H3:  NA
#>       H4: 0.5
#> 
#>       --- Transition weights ---
#>          H1 H2 H3 H4
#>       H1  0 NA NA  1
#>       H2 NA NA NA NA
#>       H3 NA NA NA NA
#>       H4  1 NA NA  0
#> 
#>   Final updated graph after removing deleted hypotheses
#> 
#>   --- Hypothesis weights ---
#>   H1: 0.5
#>   H2:  NA
#>   H3:  NA
#>   H4: 0.5
#> 
#>   --- Transition weights ---
#>      H1 H2 H3 H4
#>   H1  0 NA NA  1
#>   H2 NA NA NA NA
#>   H3 NA NA NA NA
#>   H4  1 NA NA  0
#> 
```
