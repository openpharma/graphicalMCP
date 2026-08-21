# Compute look_back rows for test_values

When a hypothesis is rejected via look_back at an earlier analysis than
the operational analysis, this function generates rows showing the
nominal p-value and boundary at each prior analysis (in decreasing order
from the operational analysis down to the attributed analysis). The
weight and boundaries are computed using the hypothesis's weight at the
point of rejection. The `Reject` column indicates whether the nominal
p-value crosses the boundary at each analysis.

## Usage

``` r
gsd_test_values_look_back(
  hyp_name,
  k,
  attributed_to,
  alpha,
  p,
  info_frac,
  spending_fn,
  hyp_names,
  w_at_rejection
)
```

## Arguments

- hyp_name:

  Name of the hypothesis.

- k:

  The operational analysis where the rejection occurred.

- attributed_to:

  The analysis to which the rejection is attributed.

- alpha:

  Overall significance level.

- p:

  P-value matrix.

- info_frac:

  Information fraction matrix.

- spending_fn:

  List of spending functions.

- hyp_names:

  Character vector of hypothesis names.

- w_at_rejection:

  The hypothesis weight at the point of rejection (from the shortcut's
  internal graph sequence).

## Value

A data frame with look_back rows for analyses k-1, k-2, ...,
attributed_to.
