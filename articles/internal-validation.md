# Power simulations using multiple approaches for internal validation

``` r

library(graphicalMCP)
```

## Introduction

Multiple approaches are implemented in `graphicalMCP` to reject a
hypothesis for different purposes and/or considerations. One approach is
to calculate the adjusted p-value of this hypothesis and compare it with
`alpha`. This approach is implemented in `adjusted_p` functions and
[`graph_test_closure()`](https://openpharma.github.io/graphicalMCP/reference/graph_test_closure.md)
(when `test_values = FALSE`). Another approach is to calculate the
adjusted significance level of this hypothesis and compare it with its
p-value. This approach is implemented in `adjusted_weights` functions,
[`graph_test_closure()`](https://openpharma.github.io/graphicalMCP/reference/graph_test_closure.md)
(when `test_values = TRUE`), and
[`graph_calculate_power()`](https://openpharma.github.io/graphicalMCP/reference/graph_calculate_power.md).
To further tailor this approach for different outputs, a different way
of coding are used for
[`graph_test_closure()`](https://openpharma.github.io/graphicalMCP/reference/graph_test_closure.md)
(when `test_values = TRUE`). When implementing these approaches in
[`graph_calculate_power()`](https://openpharma.github.io/graphicalMCP/reference/graph_calculate_power.md),
variations are added to optimize computing speed. Thus, these approaches
could be compared with each other for internal validation.

## Power simulations

A random graph will be generated and used for the comparison. A set of
marginal power (without multiplicity adjustment) is randomly generated.
Local power (with multiplicity adjustment) is calculated using
[`graph_calculate_power()`](https://openpharma.github.io/graphicalMCP/reference/graph_calculate_power.md).
In addition, p-values simulated from
[`graph_calculate_power()`](https://openpharma.github.io/graphicalMCP/reference/graph_calculate_power.md)
are saved. These p-values are used to generate local power via
[`graph_test_shortcut()`](https://openpharma.github.io/graphicalMCP/reference/graph_test_shortcut.md)
and
[`graph_test_closure()`](https://openpharma.github.io/graphicalMCP/reference/graph_test_closure.md)
as the proportion of times every hypothesis can be rejected. We expect
to observe matching results for 1000 random graphs.

### Bonferroni tests

We compare power simulations from
[`graph_calculate_power()`](https://openpharma.github.io/graphicalMCP/reference/graph_calculate_power.md)
and those using
[`graph_test_shortcut()`](https://openpharma.github.io/graphicalMCP/reference/graph_test_shortcut.md)
via respectively the adjusted p-value approach and the adjusted
significance level approach.

``` r

out <- read.csv(here::here("vignettes/internal-validation_bonferroni.csv"))
# Matching power using the adjusted p-value approach
all.equal(out$adjusted_p, rep(TRUE, nrow(out)))
#> [1] TRUE
# Matching power using the adjusted significance level approach
all.equal(out$adjusted_significance_level, rep(TRUE, nrow(out)))
#> [1] TRUE
```

### Hochberg tests

We compare power simulations from
[`graph_calculate_power()`](https://openpharma.github.io/graphicalMCP/reference/graph_calculate_power.md)
and those using
[`graph_test_closure()`](https://openpharma.github.io/graphicalMCP/reference/graph_test_closure.md)
via respectively the adjusted p-value approach and the adjusted
significance level approach. Two test groups are used with randomly
assigned hypotheses.

``` r

out <- read.csv(here::here("vignettes/internal-validation_hochberg.csv"))
# Matching power using the adjusted p-value approach
all.equal(out$adjusted_p, rep(TRUE, nrow(out)))
#> [1] TRUE
# Matching power using the adjusted significance level approach
all.equal(out$adjusted_significance_level, rep(TRUE, nrow(out)))
#> [1] TRUE
```

### Simes tests

We compare power simulations from
[`graph_calculate_power()`](https://openpharma.github.io/graphicalMCP/reference/graph_calculate_power.md)
and those using
[`graph_test_closure()`](https://openpharma.github.io/graphicalMCP/reference/graph_test_closure.md)
via respectively the adjusted p-value approach and the adjusted
significance level approach. Two test groups are used with randomly
assigned hypotheses.

``` r

out <- read.csv(here::here("vignettes/internal-validation_simes.csv"))
# Matching power using the adjusted p-value approach
all.equal(out$adjusted_p, rep(TRUE, nrow(out)))
#> [1] TRUE
# Matching power using the adjusted significance level approach
all.equal(out$adjusted_significance_level, rep(TRUE, nrow(out)))
#> [1] TRUE
```

### Parametric tests

We compare power simulations from
[`graph_calculate_power()`](https://openpharma.github.io/graphicalMCP/reference/graph_calculate_power.md)
and those using
[`graph_test_closure()`](https://openpharma.github.io/graphicalMCP/reference/graph_test_closure.md)
via respectively the adjusted p-value approach and the adjusted
significance level approach. Two test groups are used with randomly
assigned hypotheses.

``` r

out <- read.csv(here::here("vignettes/internal-validation_parametric.csv"))
# Matching power using the adjusted p-value approach
all.equal(out$adjusted_p, rep(TRUE, nrow(out)))
#> [1] TRUE
# Matching power using the adjusted significance level approach
all.equal(out$adjusted_significance_level, rep(TRUE, nrow(out)))
#> [1] TRUE
```

### Mixed tests of Bonferroni, Hochberg and Simes

We compare power simulations from
[`graph_calculate_power()`](https://openpharma.github.io/graphicalMCP/reference/graph_calculate_power.md)
and those using
[`graph_test_closure()`](https://openpharma.github.io/graphicalMCP/reference/graph_test_closure.md)
via respectively the adjusted p-value approach and the adjusted
significance level approach. Two test groups are used with randomly
assigned hypotheses. Two test types are randomly picked among
Bonferroni, Hochberg and Simes tests.

``` r

out <- read.csv(here::here("vignettes/internal-validation_mixed.csv"))
# Matching power using the adjusted p-value approach
all.equal(out$adjusted_p, rep(TRUE, nrow(out)))
#> [1] TRUE
# Matching power using the adjusted significance level approach
all.equal(out$adjusted_significance_level, rep(TRUE, nrow(out)))
#> [1] TRUE
```

### Mixed tests of parametric and one of Bonferroni, Hochberg and Simes

We compare power simulations from
[`graph_calculate_power()`](https://openpharma.github.io/graphicalMCP/reference/graph_calculate_power.md)
and those using
[`graph_test_closure()`](https://openpharma.github.io/graphicalMCP/reference/graph_test_closure.md)
via respectively the adjusted p-value approach and the adjusted
significance level approach. Two test groups are used with randomly
assigned hypotheses. Parametric test type is assigned to the first test
group and the test type for the second test group is randomly picked
among Bonferroni, Hochberg and Simes tests.

``` r

out <- read.csv(here::here("vignettes/internal-validation_parametric-mixed.csv"))
# Matching power using the adjusted p-value approach
all.equal(out$adjusted_p, rep(TRUE, nrow(out)))
#> [1] TRUE
# Matching power using the adjusted significance level approach
all.equal(out$adjusted_significance_level, rep(TRUE, nrow(out)))
#> [1] TRUE
```

## Conclusions

Multiple approaches are implemented in `graphicalMCP` to reject a
hypothesis for different purposes and/or considerations. One approach is
to calculate the adjusted p-value of this hypothesis and compare it with
`alpha`. Another approach is to calculate the adjusted significance
level of this hypothesis and compare it with its p-value. Based on 1000
random graphs, these two approaches produce matching power for all types
of tests. Therefore, the internal validation is considered to be
complete.
