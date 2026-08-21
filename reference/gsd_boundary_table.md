# Compute boundary table for all possible hypothesis weights

For each hypothesis, enumerates all unique weights from the graph's
closure (via
[`graph_generate_weights()`](https://openpharma.github.io/graphicalMCP/reference/graph_generate_weights.md))
and computes the group sequential boundaries at each analysis for each
weight. This provides a lookup table for manual verification: given a
hypothesis's weight (from graph propagation), the nominal boundary at
each analysis can be read directly.

## Usage

``` r
gsd_boundary_table(graph, alpha, info_frac, spending_fn, num_hyps, hyp_names)
```

## Arguments

- graph:

  An `initial_graph` object.

- alpha:

  Overall significance level.

- info_frac:

  Information fraction matrix (m x K).

- spending_fn:

  List of spending functions.

- num_hyps:

  Number of hypotheses.

- hyp_names:

  Character vector of hypothesis names.

## Value

A named list of data frames, one per hypothesis. Each data frame has
columns: `Weight`, `Alpha.Allocated`, and one `Boundary.k` column per
analysis, showing the nominal p-value boundary at each analysis for each
possible weight.
