# Wang-Tsiatis spending function

Computes the implied cumulative alpha spending from the Wang-Tsiatis
family of group sequential boundaries. The Wang-Tsiatis boundaries at
analysis \\k\\ with information fraction \\t_k\\ are defined as: \$\$c_k
= C \cdot t_k^{\Delta - 0.5},\$\$ where \\\Delta\\ is the shape
parameter and \\C\\ is a constant calibrated so that the overall Type I
error equals \\\alpha\\.

Special cases:

- \\\Delta = 0.5\\: Pocock boundaries (equal Z-scale boundaries across
  analyses).

- \\\Delta = 0\\: O'Brien-Fleming boundaries (very conservative at early
  analyses).

- \\0 \< \Delta \< 0.5\\: intermediate between O'Brien-Fleming and
  Pocock.

Unlike the Lan-DeMets approximations
([`spending_of()`](https://openpharma.github.io/graphicalMCP/reference/spending_functions.md),
[`spending_pocock()`](https://openpharma.github.io/graphicalMCP/reference/spending_functions.md)),
this function computes the **exact** boundaries from the Wang-Tsiatis
family and derives the implied spending. It is computationally more
expensive because it requires root-finding and multivariate normal
integration at each call.

## Usage

``` r
spending_wt(alpha, info_frac, delta = 0.5, maxpts = 25000, abseps = 1e-06)
```

## Arguments

- alpha:

  A numeric scalar of the total significance level.

- info_frac:

  A numeric vector of information fractions at each analysis. Must be
  non-negative, with at most one value \\\geq 1\\. The last value must
  be \\\geq 1\\ (i.e., the final analysis must be included), because the
  Wang-Tsiatis constant \\C\\ is calibrated over the full set of
  analyses.

- delta:

  A numeric scalar for the shape parameter \\\Delta\\. The default is
  `0.5` (Pocock). Use `0` for O'Brien-Fleming.

- maxpts:

  An integer scalar for the maximum number of function values for
  [`mvtnorm::GenzBretz()`](https://rdrr.io/pkg/mvtnorm/man/algorithms.html).
  The default is 25000.

- abseps:

  A numeric scalar for the absolute error tolerance for
  [`mvtnorm::GenzBretz()`](https://rdrr.io/pkg/mvtnorm/man/algorithms.html).
  The default is 1e-6.

## Value

A numeric vector the same length as `info_frac` of cumulative alpha
spent at each information fraction.

## References

Wang, S. K., and Tsiatis, A. A. (1987). Approximately optimal
one-parameter boundaries for group sequential trials. *Biometrics*,
43(1), 193-199.

## See also

[`spending_of()`](https://openpharma.github.io/graphicalMCP/reference/spending_functions.md)
and
[`spending_pocock()`](https://openpharma.github.io/graphicalMCP/reference/spending_functions.md)
for the Lan-DeMets approximations,
[`gs_boundaries()`](https://openpharma.github.io/graphicalMCP/reference/gs_boundaries.md)
for computing boundaries from spending functions,
[`graph_test_shortcut_gsd()`](https://openpharma.github.io/graphicalMCP/reference/graph_test_shortcut_gsd.md)
for the graphical procedure.

## Examples

``` r
# Exact O'Brien-Fleming (delta = 0)
spending_wt(0.025, c(0.5, 1), delta = 0)
#> [1] 0.002582893 0.025000000

# Exact Pocock (delta = 0.5)
spending_wt(0.025, c(0.5, 1), delta = 0.5)
#> [1] 0.01469289 0.02500000

# Intermediate (delta = 0.25)
spending_wt(0.025, c(1 / 3, 2 / 3, 1), delta = 0.25)
#> [1] 0.003058763 0.012364427 0.024988431

# Compare with Lan-DeMets approximations
spending_of(0.025, c(1 / 3, 2 / 3, 1)) # Lan-DeMets OBF approximation
#> [1] 0.0001035057 0.0060483891 0.0250000000
spending_wt(0.025, c(1 / 3, 2 / 3, 1), 0) # Exact OBF
#> [1] 0.0002594917 0.0071647556 0.0250000000

# Use in graph_test_shortcut_gsd (wrap to fix delta)
# \donttest{
g <- graph_create(c(0.5, 0.5), rbind(c(0, 1), c(1, 0)))
p <- rbind(H1 = c(0.024, 0.01), H2 = c(0.015, 0.005))
graph_test_shortcut_gsd(
  graph = g, p = p, alpha = 0.025,
  info_frac = c(0.5, 1),
  spending_fn = function(a, t) spending_wt(a, t, delta = 0.25)
)
#> 
#> Test parameters ($inputs) ------------------------------------------------------
#>   Initial graph
#> 
#>   --- Hypothesis weights ---
#>   H1: 0.5
#>   H2: 0.5
#> 
#>   --- Transition weights ---
#>      H1 H2
#>   H1  0  1
#>   H2  1  0
#> 
#>   Alpha = 0.025
#> 
#>   Information fractions
#>    Analysis_1 Analysis_2
#> H1        0.5          1
#> H2        0.5          1
#> 
#>   P-values
#>    Analysis_1 Analysis_2
#> H1   0.024000   0.010000
#> H2   0.015000   0.005000
#> 
#>   Spending functions
#>     H1: spending_wt(a, t, delta = 0.25)
#>     H2: spending_wt(a, t, delta = 0.25)
#> 
#>   Look back = FALSE
#> 
#> Test summary ($outputs) --------------------------------------------------------
#>   Hypothesis   Adj.p* Reject Tested.at First.Rej.at Last.Rej.at Look.back
#>           H1 1.00000+  FALSE         2           --          --     FALSE
#>           H2 1.00000+  FALSE         2           --          --     FALSE
#>   (*) Adjusted p-values account for both the group sequential design and the
#>       graphical multiple comparison procedure. Based on repeated p-values when
#>       look_back = FALSE, and sequential p-values when look_back = TRUE.
#> 
#>   Final updated graph after removing rejected hypotheses
#> 
#>   --- Hypothesis weights ---
#>   H1: 0.5
#>   H2: 0.5
#> 
#>   --- Transition weights ---
#>      H1 H2
#>   H1  0  1
#>   H2  1  0
#> 
# }
```
