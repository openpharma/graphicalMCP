# Comparisons with other packages

``` r

library(graphicalMCP)
library(lrstat)
library(gMCP)
```

## Introduction

There are two R packages that cover graphical multiple comparison
procedures (MCPs): `gMCP` (Rohmeyer and Klinglmueller 2024) and `lrstat`
(Lu 2023). The development of `graphicalMCP` benefited from these two
packages. Here we provide some comparisons between `graphicalMCP` and
other packages with respect to key functions.

## List of comparisons

- Weighting strategy for the closure
  - [`graphicalMCP::graph_generate_weights()`](https://openpharma.github.io/graphicalMCP/reference/graph_generate_weights.md)
  - [`gMCP::generateWeights()`](https://rdrr.io/pkg/gMCP/man/generateWeights.html)
- Sequentially rejective procedures based on Bonferroni tests
  - [`graphicalMCP::graph_test_shortcut()`](https://openpharma.github.io/graphicalMCP/reference/graph_test_shortcut.md)
  - [`gMCP::gMCP()`](https://rdrr.io/pkg/gMCP/man/gMCP.html)
- Power simulation for sequentially rejective procedures
  - [`graphicalMCP::graph_calculate_power()`](https://openpharma.github.io/graphicalMCP/reference/graph_calculate_power.md)
  - [`gMCP::calcPower()`](https://rdrr.io/pkg/gMCP/man/calcPower.html)
- Closed procedures with parametric tests
  - [`graphicalMCP::graph_test_closure()`](https://openpharma.github.io/graphicalMCP/reference/graph_test_closure.md)
  - [`gMCP::gMCP()`](https://rdrr.io/pkg/gMCP/man/gMCP.html)
- Power simulation for closed procedures with parametric tests
  - [`graphicalMCP::graph_calculate_power()`](https://openpharma.github.io/graphicalMCP/reference/graph_calculate_power.md)
  - [`gMCP::calcPower()`](https://rdrr.io/pkg/gMCP/man/calcPower.html)
- Closed procedures with Simes tests
  - [`graphicalMCP::graph_test_closure()`](https://openpharma.github.io/graphicalMCP/reference/graph_test_closure.md)
  - [`gMCP::gMCP()`](https://rdrr.io/pkg/gMCP/man/gMCP.html)
- Power simulation for closed procedures with Simes tests
  - [`graphicalMCP::graph_calculate_power()`](https://openpharma.github.io/graphicalMCP/reference/graph_calculate_power.md)
  - [`gMCP::calcPower()`](https://rdrr.io/pkg/gMCP/man/calcPower.html)

## Comparisons of weighting strategies

A random graph for five hypotheses will be generated and used for the
comparison. Weighting strategies from the following two functions will
be compared:
[`graphicalMCP::graph_generate_weights()`](https://openpharma.github.io/graphicalMCP/reference/graph_generate_weights.md)
and
[`gMCP::generateWeights()`](https://rdrr.io/pkg/gMCP/man/generateWeights.html).
This process is repeated 1000 times. Weighting strategies are matched
for all 1000 cases.

``` r

set.seed(1234)
identical <- NULL
for (i in 1:1000) {
  graph <- random_graph(5)
  graphicalmcp_weights <- graphicalMCP::graph_generate_weights(graph)
  dimnames(graphicalmcp_weights) <- list(NULL, NULL)
  gmcp_weights <-
    gMCP::generateWeights(graph$transitions, graph$hypotheses)
  gmcp_weights <- gmcp_weights[nrow(gmcp_weights):1, ] # Reorder rows
  identical <- c(
    identical,
    all.equal(gmcp_weights, graphicalmcp_weights, tolerance = 1e-7)
  )
}
all(identical)
#> [1] TRUE
```

## Comparisons of sequentially rejective procedures based on Bonferroni tests

### Adjusted p-values for testing

A random graph for five hypotheses will be generated and used for the
comparison. A set of p-values is randomly generated to be used for the
graphical MCP. Adjusted p-values are calculated and compared using the
following functions:
[`graphicalMCP::graph_test_shortcut()`](https://openpharma.github.io/graphicalMCP/reference/graph_test_shortcut.md)
and [`gMCP::gMCP()`](https://rdrr.io/pkg/gMCP/man/gMCP.html). This
process is repeated 10000 times. Adjusted p-values are matched for all
10000 cases.

``` r

set.seed(1234)
alpha <- 0.025
identical <- NULL
for (i in 1:10000) {
  graph <- random_graph(5)
  p <- runif(5, 0, alpha)
  graphicalmcp_test_shortcut <-
    graph_test_shortcut(graph, p, alpha = alpha)$outputs$adjusted_p
  gmcp_test_shortcut <-
    gMCP(as_graphMCP(graph), p, alpha = alpha)@adjPValues
  identical <- c(
    identical,
    all.equal(graphicalmcp_test_shortcut, gmcp_test_shortcut, tolerance = 1e-7)
  )
}
all(identical)
#> [1] TRUE
```

### Power simulations

A random graph for five hypotheses will be generated and used for the
comparison. A set of marginal power (without multiplicity adjustment) is
randomly generated. Local power (with multiplicity adjustment) is
calculated and compared using the following functions:
[`graphicalMCP::graph_calculate_power()`](https://openpharma.github.io/graphicalMCP/reference/graph_calculate_power.md)
and [`gMCP::calcPower()`](https://rdrr.io/pkg/gMCP/man/calcPower.html).
Since different simulation methods are used, results are slightly
different. The maximum absolute difference in local power is 0.0051
(0.51%) among 1000 cases, which is relatively small.

``` r

diff <- read.csv(here::here("vignettes/comparisons_power_shortcut.csv"))
graphicalmcp_power <- subset(diff, procedure == "graphicalMCP")
gmcp_power <- subset(diff, procedure == "gMCP")
round(
  max(
    abs(
      graphicalmcp_power[, -ncol(graphicalmcp_power)] -
        gmcp_power[, -ncol(gmcp_power)]
    )
  ),
  4
) # Maximum difference in local power among 1000 cases
#> [1] 0.0051
```

## Comparisons of closed test procedures with parametric tests

### Adjusted p-values for testing

A successive graph with two primary and two secondary hypotheses will be
generated and used for the comparison. A set of p-values is randomly
generated to be used for the graphical MCP. Adjusted p-values are
calculated and compared using the following functions:
[`graphicalMCP::graph_test_closure()`](https://openpharma.github.io/graphicalMCP/reference/graph_test_closure.md)
and [`gMCP::gMCP()`](https://rdrr.io/pkg/gMCP/man/gMCP.html). Parametric
tests are used for two primary hypotheses. This process is repeated
10000 times. Adjusted p-values are matched for all 10000 cases.

``` r

hypotheses <- c(0.5, 0.5, 0, 0)
transitions <- rbind(
  c(0, 0.5, 0.5, 0),
  c(0.5, 0, 0, 0.5),
  c(0, 1, 0, 0),
  c(1, 0, 0, 0)
)
graph <- graph_create(hypotheses, transitions)

set.seed(1234)
alpha <- 0.025
identical <- NULL
test_corr <- rbind(
  c(1, 0.5, NA, NA),
  c(0.5, 1, NA, NA),
  c(NA, NA, 1, NA),
  c(NA, NA, NA, 1)
)
for (i in 1:10000) {
  p <- runif(4, 0, alpha)
  graphicalmcp_test_parametric <- graph_test_closure(
    graph,
    p,
    alpha = alpha,
    test_groups = list(1:2, 3:4),
    test_types = c("parametric", "bonferroni"),
    test_corr = list(test_corr[1:2, 1:2], NA)
  )$outputs$adjusted_p
  gmcp_test_parametric <- gMCP(
    as_graphMCP(graph),
    p,
    alpha = 0.025,
    correlation = test_corr
  )@adjPValues
  identical <- c(
    identical,
    all.equal(graphicalmcp_test_parametric, gmcp_test_parametric, tolerance = 1e-7)
  )
}
all(identical)
#> [1] TRUE
```

### Power simulations

A successive graph with two primary and two secondary hypotheses will be
generated and used for the comparison. A set of marginal power (without
multiplicity adjustment) is randomly generated. Local power (with
multiplicity adjustment) is calculated and compared using the following
functions:
[`graphicalMCP::graph_calculate_power()`](https://openpharma.github.io/graphicalMCP/reference/graph_calculate_power.md)
and [`gMCP::calcPower()`](https://rdrr.io/pkg/gMCP/man/calcPower.html).
Parametric tests are used for two primary hypotheses. This process is
repeated 100 times. Since different simulation methods are used, results
are slightly different. The maximum absolute difference in local power
is 0.0142 (1.42%) among 100 cases, which is small.

``` r

diff <- read.csv(here::here("vignettes/comparisons_power_parametric.csv"))
graphicalmcp_power <- subset(diff, procedure == "graphicalMCP")
gmcp_power <- subset(diff, procedure == "gMCP")
round(
  max(
    abs(
      graphicalmcp_power[, -ncol(graphicalmcp_power)] -
        gmcp_power[, -ncol(gmcp_power)]
    )
  ),
  4
) # Maximum difference in local power among 100 cases
#> [1] 0.0142
```

## Comparisons of closed test procedures with Simes tests

### Adjusted p-values for testing

A successive graph with two primary and two secondary hypotheses will be
generated and used for the comparison. A set of p-values is randomly
generated to be used for the graphical MCP. Adjusted p-values are
calculated and compared using the following functions:
[`graphicalMCP::graph_test_closure()`](https://openpharma.github.io/graphicalMCP/reference/graph_test_closure.md)
and
[`lrstat::fadjpsim()`](https://kaifenglu.github.io/lrstat/reference/fadjpsim.html).
Simes tests are used for two primary hypotheses. This process is
repeated 10000 times. Adjusted p-values are matched for all 10000 cases.

``` r

hypotheses <- c(0.5, 0.5, 0, 0)
eps <- 0.0001
transitions <- rbind(
  c(0, 1 - eps, eps, 0),
  c(1 - eps, 0, 0, eps),
  c(0, 1, 0, 0),
  c(1, 0, 0, 0)
)
graph <- graph_create(hypotheses, transitions)

set.seed(1234)
alpha <- 0.025
identical <- NULL
family <- rbind(
  c(1, 1, 0, 0),
  c(0, 0, 1, 0),
  c(0, 0, 0, 1)
)
for (i in 1:10000) {
  p <- runif(4, 0, alpha)
  graphicalmcp_test_simes <- graph_test_closure(
    graph,
    p,
    alpha = alpha,
    test_groups = list(1:2, 3:4),
    test_types = c("simes", "bonferroni")
  )$outputs$adjusted_p
  names(graphicalmcp_test_simes) <- NULL
  lrstat_test_simes <-
    fadjpsim(
      fwgtmat(graph$hypotheses, graph$transitions),
      p,
      family
    )

  identical <- c(
    identical,
    all.equal(graphicalmcp_test_simes, lrstat_test_simes, tolerance = 1e-7)
  )
}
all(identical)
#> [1] TRUE
```

### Power simulations

Power simulations are not available in `lrstat` for Simes tests. Thus a
comparison could be done to compare
[`graphicalMCP::graph_calculate_power()`](https://openpharma.github.io/graphicalMCP/reference/graph_calculate_power.md)
and a manual repetition of
[`lrstat::fadjpsim()`](https://kaifenglu.github.io/lrstat/reference/fadjpsim.html)
for many sets of marginal power. This process is the same as the above
comparison of adjusted p-values, and thus omitted.

## Comparisons of closed test procedures with Hochberg tests

### Adjusted p-values for testing

A symmetric graph with five hypotheses will be generated and used for
the comparison. A set of p-values is randomly generated to be used for
the graphical MCP. Adjusted p-values are calculated and compared using
the following functions:
[`graphicalMCP::graph_test_closure()`](https://openpharma.github.io/graphicalMCP/reference/graph_test_closure.md)
and `p.adjust`. This process is repeated 10000 times. Adjusted p-values
are matched for all 10000 cases.

``` r

graph <- hochberg(5)

set.seed(1234)
alpha <- 0.025
identical <- NULL
for (i in 1:10000) {
  p <- runif(5, 0, alpha)
  graphicalmcp_test_hochberg <- graph_test_closure(
    graph,
    p,
    alpha = alpha,
    test_types = c("hochberg")
  )$outputs$adjusted_p
  names(graphicalmcp_test_hochberg) <- NULL
  p.adjust_hochberg <- p.adjust(p, method = "hochberg")

  identical <- c(
    identical,
    all.equal(graphicalmcp_test_hochberg, p.adjust_hochberg, tolerance = 1e-7)
  )
}
all(identical)
#> [1] TRUE
```

### Power simulations

Power simulations are not available in `R` for Hochberg tests. Thus a
comparison could be done to compare
[`graphicalMCP::graph_calculate_power()`](https://openpharma.github.io/graphicalMCP/reference/graph_calculate_power.md)
and a manual repetition of `p.adjust` for many sets of marginal power.
This process is the same as the above comparison of adjusted p-values,
and thus omitted.

## Reference

Lu, Kaifeng. 2023. *lrstat: Power and Sample Size Calculation for
Non-Proportional Hazards*. <https://cran.r-project.org/package=lrstat>.

Rohmeyer, Kornelius, and Florian Klinglmueller. 2024. *gMCP: Graph Based
Multiple Test Procedures*. <https://cran.r-project.org/package=gMCP>.
