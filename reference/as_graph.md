# Convert between graphicalMCP, gMCP, and igraph graph classes

Graph objects have different structures and attributes in
`graphicalMCP`, `gMCP`, and `igraph` R packages. These functions convert
between different classes to increase compatibility.

Note that `igraph` and `gMCP` have additional attributes for vertices,
edges, or a graph itself. These conversion functions only handle
attributes related to hypothesis names, hypothesis weights and
transition weights. Other attributes will be dropped when converting.

## Usage

``` r
as_initial_graph(graph)

# S3 method for class 'graphMCP'
as_initial_graph(graph)

# S3 method for class 'igraph'
as_initial_graph(graph)

as_graphMCP(graph)

# S3 method for class 'initial_graph'
as_graphMCP(graph)

as_igraph(graph)

# S3 method for class 'initial_graph'
as_igraph(graph)
```

## Arguments

- graph:

  An `initial_graph` object from the `graphicalMCP` package, a
  `graphMCP` object from the `gMCP` package, or an `igraph` object from
  the `igraph` package, depending on the conversion type.

## Value

- `as_graphMCP()` returns a `graphMCP` object for the `gMCP` package.

- `as_igraph()` returns an `igraph` object for the `igraph` package.

- `as_initial_graph()` returns an `initial_graph` object for the
  `graphicalMCP` package.

## References

Csardi, G., Nepusz, T., Traag, V., Horvat, S., Zanini, F., Noom, D., and
Mueller, K. (2024). *igraph*: Network analysis and visualization in R. R
package version 2.0.3. <https://CRAN.R-project.org/package=igraph>.

Rohmeyer, K., and Klinglmueller, K. (2024). *gMCP*: Graph based multiple
test procedures. R package version 0.8-17.
<https://cran.r-project.org/package=gMCP>.

## See also

[`graph_create()`](https://openpharma.github.io/graphicalMCP/reference/graph_create.md)
for the initial graph used in the `graphicalMCP` package.

## Examples

``` r
g_graphicalMCP <- random_graph(5)

if (requireNamespace("gMCP", quietly = TRUE)) {
  g_gMCP <- as_graphMCP(g_graphicalMCP)

  all.equal(g_graphicalMCP, as_initial_graph(g_gMCP))
}
#> [1] TRUE

if (requireNamespace("igraph", quietly = TRUE)) {
  g_igraph <- as_igraph(g_graphicalMCP)

  all.equal(g_graphicalMCP, as_initial_graph(g_igraph))
}
#> [1] TRUE
```
