# Create a spending function with a custom spending time

Wraps an existing spending function to use a fixed **spending time**
instead of the information fractions passed to it at runtime. This
controls only the alpha allocation schedule. The correlation structure
of the test statistics is determined separately by the `info_frac`
argument in
[`graph_test_shortcut_gsd()`](https://openpharma.github.io/graphicalMCP/reference/graph_test_shortcut_gsd.md)
(via
[`gs_corr()`](https://openpharma.github.io/graphicalMCP/reference/gs_corr.md)),
not by the spending function.

This is useful in two common scenarios:

- **Subgroup analyses**: all-subjects hypotheses use subgroup event
  fractions as spending time (controlling how alpha is allocated across
  analyses), while `info_frac` in
  [`graph_test_shortcut_gsd()`](https://openpharma.github.io/graphicalMCP/reference/graph_test_shortcut_gsd.md)
  uses all-subjects event fractions (controlling the correlation
  structure).

- **Monitoring with changed final information**: when the actual total
  information at the final analysis differs from the planned total, the
  planned information fractions are used as spending time to preserve
  the alpha allocation at earlier analyses, while `info_frac` in
  [`graph_test_shortcut_gsd()`](https://openpharma.github.io/graphicalMCP/reference/graph_test_shortcut_gsd.md)
  uses the actual information fractions for the correlation structure.

## Usage

``` r
spending_with_time(spending_fn, spending_time, info_frac = NULL)
```

## Arguments

- spending_fn:

  A spending function to wrap. Must accept two arguments: `alpha`
  (significance level) and `info_frac` (information fraction), and
  return the cumulative alpha spent.

- spending_time:

  A numeric vector of spending time values. These replace the
  `info_frac` argument when the wrapped function is called. May contain
  `NA` for analyses that are skipped (e.g., a hypothesis not tested at a
  particular analysis). The last non-`NA` value should be 1 if the final
  analysis has been specified.

- info_frac:

  An optional numeric vector of information fractions with the same
  length as `spending_time`. If provided, the `NA` positions are
  validated to match those in `spending_time`. This ensures that the
  spending time and information fraction structures are consistent.

## Value

A function with the same signature as `spending_fn` —
`function(alpha, info_frac)` — that internally uses `spending_time`
instead of `info_frac` for alpha allocation.

## See also

[`spending_of()`](https://openpharma.github.io/graphicalMCP/reference/spending_functions.md),
[`spending_pocock()`](https://openpharma.github.io/graphicalMCP/reference/spending_functions.md),
[`spending_hsd()`](https://openpharma.github.io/graphicalMCP/reference/spending_functions.md),
[`spending_linear()`](https://openpharma.github.io/graphicalMCP/reference/spending_functions.md)
for built-in spending functions,
[`graph_test_shortcut_gsd()`](https://openpharma.github.io/graphicalMCP/reference/graph_test_shortcut_gsd.md)
for the graphical procedure with group sequential designs.

## Examples

``` r
# --- Subgroup spending time ---
# Without spending_with_time, spending_of() uses info_frac for spending:
info_frac_all <- c(529 / 800, 700 / 800, 1) # all-subjects fractions
spending_of(0.01, info_frac_all)
#> [1] 0.001536878 0.005892983 0.010000000

# With spending_with_time, spending uses subgroup fractions instead.
# The info_frac passed at runtime is ignored by the spending function;
# it is only used by gs_boundaries()/graph_test_shortcut_gsd() for
# the correlation structure.
spending_time_sub <- c(185 / 295, 245 / 295, 1) # subgroup fractions
spending_with_time(spending_of, spending_time_sub)
#> function (alpha, info_frac_runtime) 
#> {
#>     non_na <- !is.na(info_frac_runtime)
#>     n_non_na <- sum(non_na)
#>     st <- st_non_na[seq_len(n_non_na)]
#>     spent <- spending_fn(alpha, st)
#>     result <- rep(NA_real_, length(info_frac_runtime))
#>     result[non_na] <- spent
#>     result
#> }
#> <bytecode: 0x5591590a5ef0>
#> <environment: 0x5591590a6f20>

# --- Monitoring with changed final information ---
# Planned: 295 OS events at 3 analyses (185, 245, 295 events).
# spending_time uses planned fractions for interim analyses and 1
# for the final analysis.
spending_monitor <- spending_with_time(
  spending_of,
  spending_time = c(185 / 295, 245 / 295, 1)
)

# Overrunning (310 events) or underrunning (280 events):
# spending_time is the same in both cases — it uses planned fractions
# for interim analyses and 1 for the final analysis, because alpha
# spent has been fixed for interim analyses. The actual info_frac
# (which differs between overrunning and underrunning) only affects
# the correlation structure in gs_boundaries()/graph_test_shortcut_gsd().
spending_monitor(0.01, c(185 / 295, 245 / 295, 1))
#> [1] 0.001143195 0.004706352 0.010000000

# --- Skipped analyses (NA in spending_time) ---
# If a hypothesis is not tested at analysis 2, both spending_time and
# info_frac have NA at that position. The output also has NA there.
spending_skip <- spending_with_time(
  spending_of,
  spending_time = c(185 / 295, NA, 1),
  info_frac = c(185 / 295, NA, 1)
)
spending_skip(0.01, c(185 / 295, NA, 1))
#> [1] 0.001143195          NA 0.010000000
```
