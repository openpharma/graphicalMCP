# Unified GSD procedure supporting per-hypothesis look_back

Computes repeated and sequential p-values, then processes analyses
sequentially. At each analysis k, applies
[`graph_test_shortcut()`](https://openpharma.github.io/graphicalMCP/reference/graph_test_shortcut.md)
using the appropriate p-values for each hypothesis: sequential p-values
for hypotheses with `look_back = TRUE`, repeated p-values for those with
`look_back = FALSE`. The graph is updated after each analysis before
proceeding to the next.

## Usage

``` r
gsd_test(
  graph,
  p,
  alpha,
  info_frac,
  spending_fn,
  look_back,
  num_analyses,
  num_hyps,
  hyp_names,
  analysis_names,
  test_values,
  verbose
)
```
