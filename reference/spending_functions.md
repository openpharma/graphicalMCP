# Alpha spending functions for group sequential designs

Alpha spending functions determine how the total significance level
(alpha) is allocated across interim and final analyses in a group
sequential design. Given the total alpha and the information fraction(s)
at one or more analyses, a spending function returns the cumulative
alpha spent at each information fraction.

Four commonly used spending functions are provided:

- `spending_of()` for the Lan-DeMets O'Brien-Fleming approximation,

- `spending_pocock()` for the Lan-DeMets Pocock approximation,

- `spending_hsd()` for the Hwang-Shih-DeCani family,

- `spending_linear()` for linear (uniform) spending.

## Usage

``` r
spending_of(alpha, info_frac)

spending_pocock(alpha, info_frac)

spending_hsd(alpha, info_frac, gamma = -4)

spending_linear(alpha, info_frac)
```

## Arguments

- alpha:

  A numeric scalar of the total significance level to be spent. Must be
  between 0 and 1.

- info_frac:

  A numeric scalar or vector of information fractions. Values must be
  non-negative. When `info_frac = 0`, the spending is 0. When
  `info_frac >= 1`, the spending is capped at `alpha`.

- gamma:

  A numeric scalar for the gamma parameter of the Hwang-Shih-DeCani
  spending function. Common choices are `gamma = -4` (approximates
  O'Brien-Fleming), `gamma = 1` (approximates Pocock), and `gamma = 0`
  (linear spending). The default is `gamma = -4`.

## Value

A numeric vector the same length as `info_frac` of cumulative alpha
spent at each information fraction.

## Details

All spending functions satisfy the following properties:

- \\f(\alpha, 0) = 0\\,

- \\f(\alpha, 1) = \alpha\\,

- \\f(\alpha, t)\\ is non-decreasing in \\t\\.

The cumulative alpha spent at analysis \\k\\ is \\f(\alpha, t_k)\\, and
the incremental spending is \$\$\Delta\alpha_k = f(\alpha, t_k) -
f(\alpha, t\_{k-1}).\$\$

Note that the incremental spending is *not* the nominal significance
level (boundary) at analysis \\k\\. The boundary must be derived from
the spending using the joint distribution of test statistics across
analyses. See
[`sequential_p()`](https://openpharma.github.io/graphicalMCP/reference/sequential_p.md)
and
[`graph_test_shortcut_gsd()`](https://openpharma.github.io/graphicalMCP/reference/graph_test_shortcut_gsd.md)
for details.

## Spending function formulas

- **O'Brien-Fleming** (`spending_of`): \$\$f(\alpha, t) = 2\left(1 -
  \Phi\left(\frac{\Phi^{-1}(1 - \alpha/2)} {\sqrt{t}}\right)\right).\$\$
  This is the Lan-DeMets approximation to O'Brien-Fleming boundaries. It
  is very conservative at early analyses and spends most of the alpha at
  the final analysis.

- **Pocock** (`spending_pocock`): \$\$f(\alpha, t) = \alpha \cdot
  \ln(1 + (e - 1) \cdot t).\$\$ This spends alpha more evenly across
  analyses compared to O'Brien-Fleming.

- **Hwang-Shih-DeCani** (`spending_hsd`): \$\$f(\alpha, t) = \alpha
  \cdot \frac{1 - e^{-\gamma t}}{1 - e^{-\gamma}}, \quad \gamma \neq
  0,\$\$ \$\$f(\alpha, t) = \alpha \cdot t, \quad \gamma = 0.\$\$ With
  `gamma = -4`, it approximates O'Brien-Fleming; with `gamma = 1`, it
  approximates Pocock.

- **Linear** (`spending_linear`): \$\$f(\alpha, t) = \alpha \cdot t.\$\$

## References

Lan, K. K. G., and DeMets, D. L. (1983). Discrete sequential boundaries
for clinical trials. *Biometrika*, 70(3), 659-663.

Hwang, I. K., Shih, W. J., and De Cani, J. S. (1990). Group sequential
designs using a family of type I error probability spending functions.
*Statistics in Medicine*, 9(12), 1439-1445.

## Examples

``` r
# O'Brien-Fleming spending at 50% information
spending_of(0.025, 0.5)
#> [1] 0.001525323

# Cumulative spending across analyses (vectorized)
spending_of(0.025, c(0, 0.5, 1))
#> [1] 0.000000000 0.001525323 0.025000000

# Compare spending functions at information fractions (1/3, 2/3, 1)
spending_of(0.025, c(1 / 3, 2 / 3, 1))
#> [1] 0.0001035057 0.0060483891 0.0250000000
spending_pocock(0.025, c(1 / 3, 2 / 3, 1))
#> [1] 0.01132081 0.01908456 0.02500000
spending_hsd(0.025, c(1 / 3, 2 / 3, 1), gamma = -4)
#> [1] 0.001303062 0.006246445 0.025000000
spending_linear(0.025, c(1 / 3, 2 / 3, 1))
#> [1] 0.008333333 0.016666667 0.025000000

# User-defined spending function: piecewise combination.
# Use O'Brien-Fleming for the first half of alpha (conservative at
# early analyses), and Pocock for the second half (more aggressive).
# This can be useful when a hypothesis starts with a small weight
# (OBF spending) and later receives additional weight via graph
# propagation (Pocock spending for the increment).
spending_piecewise <- function(alpha, info_frac, threshold = 0.0125) {
  spending_of(pmin(alpha, threshold), info_frac) +
    spending_pocock(pmax(alpha - threshold, 0), info_frac)
}
spending_piecewise(0.025, c(1 / 3, 2 / 3, 1))
#> [1] 0.005675579 0.011762668 0.025000000
# Compare: alpha = 0.0125 uses only OBF
spending_piecewise(0.0125, c(1 / 3, 2 / 3, 1))
#> [1] 1.517362e-05 2.220386e-03 1.250000e-02
spending_of(0.0125, c(1 / 3, 2 / 3, 1))
#> [1] 1.517362e-05 2.220386e-03 1.250000e-02
# Pocock spending at 50% information
spending_pocock(0.025, 0.5)
#> [1] 0.01550286
# Hwang-Shih-DeCani spending at 50% information
spending_hsd(0.025, 0.5, gamma = -4)
#> [1] 0.002980073
spending_hsd(0.025, 0.5, gamma = 1)
#> [1] 0.01556148
spending_hsd(0.025, 0.5, gamma = 0)
#> [1] 0.0125
# Linear spending at 50% information
spending_linear(0.025, 0.5)
#> [1] 0.0125
```
