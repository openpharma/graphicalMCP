# Find pairs of vertices that are connected in both directions

For an initial graph, find pairs of hypotheses that are connected in
both directions. This is used to plot graphs using
[`plot.initial_graph()`](https://openpharma.github.io/graphicalMCP/reference/plot.initial_graph.md).

## Usage

``` r
edge_pairs(graph)
```

## Arguments

- graph:

  An initial graph as returned by
  [`graph_create()`](https://openpharma.github.io/graphicalMCP/reference/graph_create.md).

## Value

A list of vertex pairs which are connected in both directions. NULL if
no such pairs are found.
