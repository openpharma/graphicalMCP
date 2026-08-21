# Compute test_values for look_back = FALSE (analysis-by-analysis)

For each analysis, computes the nominal boundaries and records the
rejection sequence with boundaries at which rejections occurred. Walks
through the rejection sequence within each analysis, updating the graph
and recomputing boundaries after each rejection.

## Usage

``` r
gsd_test_values_details(
  step_graph,
  p,
  k,
  alpha,
  info_frac,
  spending_fn,
  rejection_seq_k,
  hyp_names,
  rejected_after,
  has_data_k = rep(TRUE, length(hyp_names))
)
```
