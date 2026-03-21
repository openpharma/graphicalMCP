# Obtain an updated graph by updating an initial graphical after deleting hypotheses

After a hypothesis is deleted, an initial graph will be updated. The
deleted hypothesis will have the hypothesis weight of 0 and the
transition weight of 0. Remaining hypotheses will have updated
hypothesis weights and transition weights according to Algorithm 1 of
Bretz et al. (2009).

## Usage

``` r
graph_update(graph, delete)
```

## Arguments

- graph:

  An initial graph as returned by
  [`graph_create()`](https://openpharma.github.io/graphicalMCP/reference/graph_create.md).

- delete:

  A logical or integer vector, denoting which hypotheses to delete. A
  logical vector results in the "unordered mode", which means that
  hypotheses corresponding to `TRUE` in `delete` will be deleted. The
  sequence of deletion will follow the sequence of `TRUE`'s in `delete`.
  In this case, the length of the logical vector must match the number
  of hypotheses in `graph`. An integer vector results in the "ordered
  mode", which means that `delete` specifies the sequence in which
  hypotheses should be deleted by indicating the location of deleted
  hypotheses, e.g., 1st, 2nd, etc. In this case, the integer vector can
  have any length, but must only contain valid hypothesis numbers
  (greater than 0, and less than or equal to he number of hypotheses in
  `graph`).

## Value

An S3 object of class `updated_graph` with a list of 4 elements:

- `initial_graph`: The initial graph object.

- `updated_graph`: The updated graph object with specified hypotheses
  deleted.

- `deleted`: A numeric vector indicating which hypotheses were deleted.

- `intermediate_graphs`: When using the ordered mode, a list of
  intermediate updated graphs after each hypothesis is deleted according
  to the sequence specified by `delete`.

## Sequence of deletion

When there are multiple hypotheses to be deleted from a graph, there are
many sequences of deletion in which an initial graph is updated to an
updated graph. If the interest is in the updated graph after all
hypotheses specified by `delete` are deleted, this updated graph is the
same no matter which sequence of deletion is used. This property has
been proved by Bretz et al. (2009). If the interest is in the
intermediate updated graph after each hypothesis is deleted according to
the sequence specified by `delete`, an integer vector of `delete` should
be specified and these detailed outputs will be provided.

## References

Bretz, F., Maurer, W., Brannath, W., and Posch, M. (2009). A graphical
approach to sequentially rejective multiple test procedures. *Statistics
in Medicine*, 28(4), 586-604.

Bretz, F., Posch, M., Glimm, E., Klinglmueller, F., Maurer, W., and
Rohmeyer, K. (2011). Graphical approaches for multiple comparison
procedures using weighted Bonferroni, Simes, or parametric tests.
*Biometrical Journal*, 53(6), 894-913.

## See also

- [`graph_create()`](https://openpharma.github.io/graphicalMCP/reference/graph_create.md)
  for the initial graph.

- [`graph_rejection_orderings()`](https://openpharma.github.io/graphicalMCP/reference/graph_rejection_orderings.md)
  for possible sequences of rejections for a graphical multiple
  comparison procedure using shortcut testing.

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
