# Generate the weighting strategy based on a graphical multiple comparison procedure

A graphical multiple comparison procedure defines a closed test
procedure, which tests each intersection hypothesis and reject an
individual hypothesis if all intersection hypotheses involving it have
been rejected. An intersection hypothesis represents the parameter space
where individual null hypotheses involved are true simultaneously.

The closure based on a graph consists of all updated graphs
(corresponding to intersection hypotheses) after all combinations of
hypotheses are deleted. For a graphical multiple comparison procedure
with \\m\\ hypotheses, there are \\2^{m}-1\\ updated graphs
(intersection hypotheses), including the initial graph (the overall
intersection hypothesis). The weighting strategy of this graph consists
of hypothesis weights from all \\2^{m}-1\\ updated graphs (intersection
hypotheses). The algorithm to derive the weighting strategy is based on
Algorithm 1 in Bretz et al. (2011).

## Usage

``` r
graph_generate_weights(graph)
```

## Arguments

- graph:

  An initial graph as returned by
  [`graph_create()`](https://openpharma.github.io/graphicalMCP/reference/graph_create.md).

## Value

A numeric matrix of all intersection hypotheses and their hypothesis
weights. For a graphical multiple comparison procedure with \\m\\
hypotheses, the number of rows is \\2^{m}-1\\, each of which corresponds
to an intersection hypothesis. The number of columns is \\2\cdot m\\.
The first \\m\\ columns indicate which individual hypotheses are
included in a given intersection hypothesis and the second half of
columns provide hypothesis weights for each individual hypothesis for a
given intersection hypothesis.

## Performance

Generation of intersection hypotheses is closely related to the power
set of a given set of indices. As the number of hypotheses increases,
the memory and time usage can grow quickly (e.g., at a rate of
\\O(2^n)\\). There are also multiple ways to implement Algorithm 1 in
Bretz et al. (2011). See
[`vignette("generate-closure")`](https://openpharma.github.io/graphicalMCP/articles/generate-closure.md)
for more information about generating intersection hypotheses and
comparisons of different approaches to calculate weighting strategies.

## References

Bretz, F., Posch, M., Glimm, E., Klinglmueller, F., Maurer, W., and
Rohmeyer, K. (2011). Graphical approaches for multiple comparison
procedures using weighted Bonferroni, Simes, or parametric tests.
*Biometrical Journal*, 53(6), 894-913.

## See also

[`graph_test_closure()`](https://openpharma.github.io/graphicalMCP/reference/graph_test_closure.md)
for graphical multiple comparison procedures using the closed test.

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

graph_generate_weights(g)
#>    H1 H2 H3 H4  H1  H2  H3  H4
#> 1   1  1  1  1 0.5 0.5 0.0 0.0
#> 2   1  1  1  0 0.5 0.5 0.0 0.0
#> 3   1  1  0  1 0.5 0.5 0.0 0.0
#> 4   1  1  0  0 0.5 0.5 0.0 0.0
#> 5   1  0  1  1 0.5 0.0 0.0 0.5
#> 6   1  0  1  0 1.0 0.0 0.0 0.0
#> 7   1  0  0  1 0.5 0.0 0.0 0.5
#> 8   1  0  0  0 1.0 0.0 0.0 0.0
#> 9   0  1  1  1 0.0 0.5 0.5 0.0
#> 10  0  1  1  0 0.0 0.5 0.5 0.0
#> 11  0  1  0  1 0.0 1.0 0.0 0.0
#> 12  0  1  0  0 0.0 1.0 0.0 0.0
#> 13  0  0  1  1 0.0 0.0 0.5 0.5
#> 14  0  0  1  0 0.0 0.0 1.0 0.0
#> 15  0  0  0  1 0.0 0.0 0.0 1.0
```
