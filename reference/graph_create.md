# Create the initial graph for a multiple comparison procedure

A graphical multiple comparison procedure is represented by 1) a vector
of initial hypothesis weights `hypotheses`, and 2) a matrix of initial
transition weights `transitions`. This function creates the initial
graph object using hypothesis weights and transition weights.

## Usage

``` r
graph_create(hypotheses, transitions, hyp_names = NULL)
```

## Arguments

- hypotheses:

  A numeric vector of hypothesis weights in a graphical multiple
  comparison procedure. Must be a vector of values between 0 & 1
  (inclusive). The length should match the row and column lengths of
  `transitions`. The sum of hypothesis weights should not exceed 1.

- transitions:

  A numeric matrix of transition weights between hypotheses in a
  graphical multiple comparison procedure. Must be a square matrix of
  values between 0 & 1 (inclusive). The row and column lengths should
  match the length of `hypotheses`. Each row (Transition weights leaving
  a hypothesis) can sum to no more than 1. The diagonal entries
  (Transition weights from a hypothesis to itself) must be all 0s.

- hyp_names:

  (Optional) A character vector of hypothesis names. If not provided,
  names from `hypotheses` and `transitions` will be used. If names are
  not specified, hypotheses will be named sequentially as H1, H2,
  .......

## Value

An S3 object of class `initial_graph` with a list of 2 elements:

- Hypothesis weights `hypotheses`.

- Transition weights `transitions`.

## Validation of inputs

Inputs are also validated to make sure of the validity of the graph:

- Hypothesis weights `hypotheses` are numeric.

- Transition weights `transitions` are numeric.

- Length of `hypotheses` and dimensions of `transitions` match.

- Hypothesis weights `hypotheses` must be non-negative and sum to no
  more than 1.

- Transition weights `transitions`:

  - Values must be non-negative.

  - Rows must sum to no more than 1.

  - Diagonal entries must be all 0.

- Hypothesis names `hyp_names` override names in `hypotheses` or
  `transitions`.

## References

Bretz, F., Maurer, W., Brannath, W., and Posch, M. (2009). A graphical
approach to sequentially rejective multiple test procedures. *Statistics
in Medicine*, 28(4), 586-604.

Bretz, F., Posch, M., Glimm, E., Klinglmueller, F., Maurer, W., and
Rohmeyer, K. (2011). Graphical approaches for multiple comparison
procedures using weighted Bonferroni, Simes, or parametric tests.
*Biometrical Journal*, 53(6), 894-913.

## See also

[`graph_update()`](https://openpharma.github.io/graphicalMCP/reference/graph_update.md)
for the updated graph after hypotheses being deleted from the initial
graph.

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

# Explicit names override names in `hypotheses` (with a warning)
hypotheses <- c(h1 = 0.5, h2 = 0.5, h3 = 0, h4 = 0)
transitions <- rbind(
  c(0, 0, 1, 0),
  c(0, 0, 0, 1),
  c(0, 1, 0, 0),
  c(1, 0, 0, 0)
)
g <- graph_create(hypotheses, transitions, hyp_names)
#> Warning: Hypothesis names specified - overriding names in
#>                     `hypotheses` and `transitions`
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

# Use names in `transitions`
hypotheses <- c(0.5, 0.5, 0, 0)
transitions <- rbind(
  H1 = c(0, 0, 1, 0),
  H2 = c(0, 0, 0, 1),
  H3 = c(0, 1, 0, 0),
  H4 = c(1, 0, 0, 0)
)
g <- graph_create(hypotheses, transitions)
g
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

# Unmatched names in `hypotheses` and `transitions` (with an error)
hypotheses <- c(h1 = 0.5, h2 = 0.5, h3 = 0, h4 = 0)
transitions <- rbind(
  H1 = c(0, 0, 1, 0),
  H2 = c(0, 0, 0, 1),
  H3 = c(0, 1, 0, 0),
  H4 = c(1, 0, 0, 0)
)
try(
  g <- graph_create(hypotheses, transitions)
)
#> Error in graph_create(hypotheses, transitions) : 
#>   Names provided in `hypotheses` and `transitions` should match

# When names are not specified, hypotheses are numbered sequentially as
# H1, H2, ...
hypotheses <- c(0.5, 0.5, 0, 0)
transitions <- rbind(
  c(0, 0, 1, 0),
  c(0, 0, 0, 1),
  c(0, 1, 0, 0),
  c(1, 0, 0, 0)
)
g <- graph_create(hypotheses, transitions)
g
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
```
