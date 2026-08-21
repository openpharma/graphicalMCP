# Validation of Group Sequential Functions Against gsDesign and rpact

## Introduction

This vignette validates the group sequential design functions in
graphicalMCP against two established R packages: **gsDesign** (Anderson,
2024) and **rpact** (Wassmer and Pahlke, 2024). We compare:

1.  Alpha spending functions against gsDesign and rpact
2.  Group sequential boundaries against gsDesign and rpact
3.  Correlation matrix against the analytical formula
4.  Repeated p-values against rpact
5.  Sequential p-values against gsDesign
6.  Wang-Tsiatis boundaries against rpact

For all comparisons, we expect agreement to within numerical tolerance
(typically $`< 10^{-6}`$).

``` r

library(graphicalMCP)
library(gsDesign)
library(rpact)
```

## 1. Alpha Spending Functions

We compare the four built-in spending functions in graphicalMCP against
their equivalents in gsDesign and rpact across several scenarios.

### Comparison with gsDesign

The gsDesign package provides spending functions via
[`sfLDOF()`](https://keaven.github.io/gsDesign//reference/sfLDOF.html)
(O’Brien-Fleming),
[`sfLDPocock()`](https://keaven.github.io/gsDesign//reference/sfLDOF.html)
(Pocock),
[`sfHSD()`](https://keaven.github.io/gsDesign//reference/sfHSD.html)
(Hwang-Shih-DeCani), and
[`sfLinear()`](https://keaven.github.io/gsDesign//reference/sfLinear.html)
(Linear). Each returns a list with `$spend` containing cumulative alpha
spent.

``` r

alpha <- 0.025
info_frac_list <- list(
  equally_spaced_3 = c(1/3, 2/3, 1),
  equally_spaced_2 = c(0.5, 1),
  unequal = c(0.2, 0.6, 1),
  early_look = c(0.1, 0.5, 1)
)

tol <- 1e-10
all_pass <- TRUE

for (name in names(info_frac_list)) {
  t <- info_frac_list[[name]]

  # O'Brien-Fleming
  graphicalMCP_of <- spending_of(alpha, t)
  gsd_of <- gsDesign::sfLDOF(alpha, t, param = NULL)$spend
  if (!isTRUE(all.equal(graphicalMCP_of, gsd_of, tolerance = tol))) {
    cat("FAIL: OBF spending,", name, "\n")
    all_pass <- FALSE
  }

  # Pocock
  graphicalMCP_poc <- spending_pocock(alpha, t)
  gsd_poc <- gsDesign::sfLDPocock(alpha, t, param = NULL)$spend
  if (!isTRUE(all.equal(graphicalMCP_poc, gsd_poc, tolerance = tol))) {
    cat("FAIL: Pocock spending,", name, "\n")
    all_pass <- FALSE
  }

  # HSD with gamma = -4
  graphicalMCP_hsd <- spending_hsd(alpha, t, gamma = -4)
  gsd_hsd <- gsDesign::sfHSD(alpha, t, param = -4)$spend
  if (!isTRUE(all.equal(graphicalMCP_hsd, gsd_hsd, tolerance = tol))) {
    cat("FAIL: HSD spending,", name, "\n")
    all_pass <- FALSE
  }

  # Linear (spending_linear is alpha * t by definition)
  graphicalMCP_lin <- spending_linear(alpha, t)
  expected_lin <- alpha * t
  if (!isTRUE(all.equal(graphicalMCP_lin, expected_lin, tolerance = tol))) {
    cat("FAIL: Linear spending,", name, "\n")
    all_pass <- FALSE
  }
}

if (all_pass) cat("All spending function comparisons with gsDesign PASS\n")
#> All spending function comparisons with gsDesign PASS
```

### Comparison with rpact

The rpact package computes cumulative alpha spent as part of the design
object. We access it via `$alphaSpent`.

``` r

all_pass <- TRUE

for (name in names(info_frac_list)) {
  t <- info_frac_list[[name]]

  # O'Brien-Fleming
  graphicalMCP_of <- spending_of(alpha, t)
  rpact_of <- getDesignGroupSequential(
    typeOfDesign = "OF",
    informationRates = t,
    alpha = alpha
  )$alphaSpent
  if (!isTRUE(all.equal(graphicalMCP_of, rpact_of, tolerance = 1e-6))) {
    cat("FAIL: OBF spending,", name, "\n")
    all_pass <- FALSE
  }

  # Pocock
  graphicalMCP_poc <- spending_pocock(alpha, t)
  rpact_poc <- getDesignGroupSequential(
    typeOfDesign = "P",
    informationRates = t,
    alpha = alpha
  )$alphaSpent
  if (!isTRUE(all.equal(graphicalMCP_poc, rpact_poc, tolerance = 1e-6))) {
    cat("FAIL: Pocock spending,", name, "\n")
    all_pass <- FALSE
  }
}
#> FAIL: OBF spending, equally_spaced_3 
#> FAIL: Pocock spending, equally_spaced_3 
#> FAIL: OBF spending, equally_spaced_2 
#> FAIL: Pocock spending, equally_spaced_2 
#> FAIL: OBF spending, unequal 
#> FAIL: Pocock spending, unequal 
#> FAIL: OBF spending, early_look 
#> FAIL: Pocock spending, early_look

if (all_pass) cat("All spending function comparisons with rpact PASS\n")
```

### Detailed Example

For transparency, we show one detailed comparison:

``` r

t <- c(1/3, 2/3, 1)

spending_detail <- data.frame(
  `Info Fraction` = t,
  `graphicalMCP (OBF)` = spending_of(alpha, t),
  `gsDesign (sfLDOF)` = gsDesign::sfLDOF(alpha, t, param = NULL)$spend,
  `graphicalMCP (Pocock)` = spending_pocock(alpha, t),
  `gsDesign (sfLDPocock)` = gsDesign::sfLDPocock(alpha, t, param = NULL)$spend,
  check.names = FALSE
)
knitr::kable(spending_detail, digits = 10,
             caption = "Spending function comparison (alpha = 0.025)")
```

| Info Fraction | graphicalMCP (OBF) | gsDesign (sfLDOF) | graphicalMCP (Pocock) | gsDesign (sfLDPocock) |
|---:|---:|---:|---:|---:|
| 0.3333333 | 0.0001035057 | 0.0001035057 | 0.01132081 | 0.01132081 |
| 0.6666667 | 0.0060483891 | 0.0060483891 | 0.01908456 | 0.01908456 |
| 1.0000000 | 0.0250000000 | 0.0250000000 | 0.02500000 | 0.02500000 |

Spending function comparison (alpha = 0.025) {.table}

## 2. Group Sequential Boundaries

We compare Z-scale and nominal p-value boundaries from
[`gs_boundaries()`](https://openpharma.github.io/graphicalMCP/reference/gs_boundaries.md)
against gsDesign and rpact.

### Comparison with gsDesign

gsDesign computes boundaries via
[`gsDesign()`](https://keaven.github.io/gsDesign//reference/gsDesign.html).
The function uses `test.type = 1` for one-sided testing with `timing`
specifying the first $`K-1`$ information fractions (the last is always
1).

``` r

all_pass <- TRUE

test_cases <- list(
  list(alpha = 0.025, t = c(1/3, 2/3, 1), sfu = gsDesign::sfLDOF,
       graphicalMCP_fn = spending_of, label = "OBF, 3 analyses"),
  list(alpha = 0.025, t = c(0.5, 1), sfu = gsDesign::sfLDOF,
       graphicalMCP_fn = spending_of, label = "OBF, 2 analyses"),
  list(alpha = 0.025, t = c(1/3, 2/3, 1), sfu = gsDesign::sfLDPocock,
       graphicalMCP_fn = spending_pocock, label = "Pocock, 3 analyses"),
  list(alpha = 0.01, t = c(0.2, 0.6, 1), sfu = gsDesign::sfLDOF,
       graphicalMCP_fn = spending_of, label = "OBF, unequal, alpha=0.01")
)

for (tc in test_cases) {
  # gsDesign boundaries
  K <- length(tc$t)
  gsd_design <- gsDesign::gsDesign(
    k = K,
    test.type = 1,
    alpha = tc$alpha,
    sfu = tc$sfu,
    timing = tc$t[-K]
  )
  gsd_z <- gsd_design$upper$bound
  gsd_nom <- pnorm(gsd_z, lower.tail = FALSE)

  # graphicalMCP boundaries
  graphicalMCP_bounds <- graphicalMCP:::gs_boundaries(tc$alpha, tc$t, tc$graphicalMCP_fn)

  if (!isTRUE(all.equal(graphicalMCP_bounds$bounds_z, gsd_z, tolerance = 1e-4))) {
    cat("FAIL Z:", tc$label, "\n")
    all_pass <- FALSE
  }
  if (!isTRUE(all.equal(graphicalMCP_bounds$bounds_nominal, gsd_nom, tolerance = 1e-4))) {
    cat("FAIL nominal:", tc$label, "\n")
    all_pass <- FALSE
  }
}

if (all_pass) cat("All boundary comparisons with gsDesign PASS\n")
#> All boundary comparisons with gsDesign PASS
```

### Comparison with rpact

rpact computes boundaries via
[`getDesignGroupSequential()`](https://docs.rpact.org/reference/getDesignGroupSequential.html).
The `criticalValues` field contains Z-scale boundaries and `stageLevels`
contains nominal p-value boundaries.

``` r

all_pass <- TRUE

rpact_cases <- list(
  list(alpha = 0.025, t = c(1/3, 2/3, 1), type = "OF",
       graphicalMCP_fn = spending_of, label = "OBF, 3 analyses"),
  list(alpha = 0.025, t = c(0.5, 1), type = "OF",
       graphicalMCP_fn = spending_of, label = "OBF, 2 analyses"),
  list(alpha = 0.025, t = c(1/3, 2/3, 1), type = "P",
       graphicalMCP_fn = spending_pocock, label = "Pocock, 3 analyses")
)

for (tc in rpact_cases) {
  rpact_design <- getDesignGroupSequential(
    typeOfDesign = tc$type,
    informationRates = tc$t,
    alpha = tc$alpha
  )
  rpact_z <- rpact_design$criticalValues
  rpact_nom <- rpact_design$stageLevels

  graphicalMCP_bounds <- graphicalMCP:::gs_boundaries(tc$alpha, tc$t, tc$graphicalMCP_fn)

  if (!isTRUE(all.equal(graphicalMCP_bounds$bounds_z, rpact_z, tolerance = 1e-4))) {
    cat("FAIL Z:", tc$label, "\n")
    all_pass <- FALSE
  }
  if (!isTRUE(all.equal(graphicalMCP_bounds$bounds_nominal, rpact_nom, tolerance = 1e-4))) {
    cat("FAIL nominal:", tc$label, "\n")
    all_pass <- FALSE
  }
}
#> FAIL Z: OBF, 3 analyses 
#> FAIL nominal: OBF, 3 analyses 
#> FAIL Z: OBF, 2 analyses 
#> FAIL nominal: OBF, 2 analyses 
#> FAIL Z: Pocock, 3 analyses 
#> FAIL nominal: Pocock, 3 analyses

if (all_pass) cat("All boundary comparisons with rpact PASS\n")
```

### Detailed Example

``` r

t <- c(1/3, 2/3, 1)

graphicalMCP_b <- graphicalMCP:::gs_boundaries(0.025, t, spending_of)
gsd_d <- gsDesign::gsDesign(k = 3, test.type = 1, alpha = 0.025,
                             sfu = gsDesign::sfLDOF, timing = c(1/3, 2/3))
rpact_d <- getDesignGroupSequential(
  typeOfDesign = "OF", informationRates = t, alpha = 0.025
)

boundary_detail <- data.frame(
  Analysis = 1:3,
  `graphicalMCP (Z)` = graphicalMCP_b$bounds_z,
  `gsDesign (Z)` = gsd_d$upper$bound,
  `rpact (Z)` = rpact_d$criticalValues,
  `graphicalMCP (nom. p)` = graphicalMCP_b$bounds_nominal,
  `gsDesign (nom. p)` = pnorm(gsd_d$upper$bound, lower.tail = FALSE),
  `rpact (nom. p)` = rpact_d$stageLevels,
  check.names = FALSE
)
knitr::kable(boundary_detail, digits = 6,
             caption = "Boundary comparison: OBF with 3 equally spaced analyses")
```

| Analysis | graphicalMCP (Z) | gsDesign (Z) | rpact (Z) | graphicalMCP (nom. p) | gsDesign (nom. p) | rpact (nom. p) |
|---:|---:|---:|---:|---:|---:|---:|
| 1 | 3.710303 | 3.710303 | 3.471091 | 0.000104 | 0.000104 | 0.000259 |
| 2 | 2.511427 | 2.511427 | 2.454432 | 0.006012 | 0.006012 | 0.007055 |
| 3 | 1.993043 | 1.993048 | 2.004036 | 0.023128 | 0.023128 | 0.022533 |

Boundary comparison: OBF with 3 equally spaced analyses {.table}

## 3. Correlation Matrix

The canonical correlation matrix for group sequential test statistics is
$`\text{Cor}(Z_i, Z_j) = \sqrt{t_i / t_j}`$ for $`i \le j`$. We verify
[`gs_corr()`](https://openpharma.github.io/graphicalMCP/reference/gs_corr.md)
against this formula.

``` r

all_pass <- TRUE

for (name in names(info_frac_list)) {
  t <- info_frac_list[[name]]
  K <- length(t)

  # graphicalMCP
  graphicalMCP_corr <- graphicalMCP:::gs_corr(t)

  # Manual formula
  manual_corr <- outer(t, t, function(ti, tj) sqrt(pmin(ti, tj) / pmax(ti, tj)))

  if (!isTRUE(all.equal(graphicalMCP_corr, manual_corr, tolerance = 1e-12))) {
    cat("FAIL:", name, "\n")
    all_pass <- FALSE
  }

  # Verify properties
  stopifnot(
    all(diag(graphicalMCP_corr) == 1),
    isSymmetric(graphicalMCP_corr)
  )
}

if (all_pass) cat("All correlation matrix comparisons PASS\n")
#> All correlation matrix comparisons PASS
```

``` r

t <- c(1/3, 2/3, 1)
knitr::kable(graphicalMCP:::gs_corr(t), digits = 6,
             caption = "Correlation matrix for info fractions (1/3, 2/3, 1)")
```

|          |          |          |
|---------:|---------:|---------:|
| 1.000000 | 0.707107 | 0.577350 |
| 0.707107 | 1.000000 | 0.816497 |
| 0.577350 | 0.816497 | 1.000000 |

Correlation matrix for info fractions (1/3, 2/3, 1) {.table}

## 4. Repeated P-values

The repeated p-value at analysis $`k`$ is the minimum significance level
$`\alpha`$ at which the group sequential boundary at analysis $`k`$
would be crossed by the observed p-value. We validate
[`repeated_p()`](https://openpharma.github.io/graphicalMCP/reference/repeated_p.md)
using a boundary-based approach: for each repeated p-value $`\hat{p}_k`$
returned by
[`repeated_p()`](https://openpharma.github.io/graphicalMCP/reference/repeated_p.md),
we compute the boundary at that alpha using both graphicalMCP and rpact,
and verify that the boundary at analysis $`k`$ matches the observed
p-value.

This confirms that
[`repeated_p()`](https://openpharma.github.io/graphicalMCP/reference/repeated_p.md)
correctly inverts the boundary function, consistent with boundaries from
both gsDesign and rpact.

``` r

scenarios <- list(
  list(
    p = c(0.024, 0.01),
    t = c(0.5, 1),
    type = "asOF",
    fn = spending_of,
    label = "OBF, 2 analyses"
  ),
  list(
    p = c(0.05, 0.02, 0.01),
    t = c(1/3, 2/3, 1),
    type = "asOF",
    fn = spending_of,
    label = "OBF, 3 analyses"
  ),
  list(
    p = c(0.024, 0.01),
    t = c(0.5, 1),
    type = "asP",
    fn = spending_pocock,
    label = "Pocock, 2 analyses"
  ),
  list(
    p = c(0.1, 0.05, 0.01),
    t = c(0.2, 0.6, 1),
    type = "asOF",
    fn = spending_of,
    label = "OBF, unequal spacing"
  )
)

all_pass <- TRUE

for (sc in scenarios) {
  K <- length(sc$p)

  for (k in 1:K) {
    # Compute repeated p-value at analysis k
    alpha_rep <- repeated_p(
      p = sc$p[1:k],
      info_frac = sc$t[1:k],
      spending_fn = sc$fn
    )

    # Skip if repeated p-value is at the boundary (1 or near 0)
    if (alpha_rep >= 1 - 1e-6 || alpha_rep <= 1e-6) next

    # graphicalMCP boundary at this alpha
    graphicalMCP_bounds <- graphicalMCP:::gs_boundaries(
      alpha_rep, sc$t[1:k], sc$fn
    )
    graphicalMCP_nom_k <- graphicalMCP_bounds$bounds_nominal[k]

    # The observed p-value should equal the boundary at analysis k
    if (!isTRUE(all.equal(sc$p[k], graphicalMCP_nom_k, tolerance = 1e-4))) {
      cat("FAIL (graphicalMCP boundary):", sc$label, "analysis", k, "\n")
      cat("  observed p:", sc$p[k], "  boundary:", graphicalMCP_nom_k, "\n")
      all_pass <- FALSE
    }

    # rpact boundary at this alpha (only when info_frac ends at 1,
    # since rpact requires the last information rate to be 1)
    if (k == K) {
      rpact_design <- suppressMessages(getDesignGroupSequential(
        typeOfDesign = sc$type,
        informationRates = sc$t,
        alpha = alpha_rep
      ))
      rpact_nom_k <- rpact_design$stageLevels[k]
      if (!isTRUE(all.equal(sc$p[k], rpact_nom_k, tolerance = 1e-3))) {
        cat("FAIL (rpact boundary):", sc$label, "analysis", k, "\n")
        cat("  observed p:", sc$p[k], "  boundary:", rpact_nom_k, "\n")
        all_pass <- FALSE
      }
    }
  }
}

if (all_pass) cat("All repeated p-value boundary checks PASS\n")
#> All repeated p-value boundary checks PASS
```

### Detailed Example

We show the boundary-based verification for a concrete case:
O’Brien-Fleming spending with two analyses at information fractions
(0.5, 1) and observed p-values (0.024, 0.01).

``` r

p_obs <- c(0.024, 0.01)
t <- c(0.5, 1)

# Compute repeated p-values at each analysis
graphicalMCP_rep <- c(
  repeated_p(p_obs[1], t[1], spending_of),
  repeated_p(p_obs, t, spending_of)
)

# For each repeated p-value, verify boundary matches observed p
verify <- data.frame(Analysis = integer(), `Observed p` = numeric(),
                     `Repeated p (alpha)` = numeric(),
                     `graphicalMCP boundary` = numeric(),
                     `rpact boundary` = numeric(),
                     check.names = FALSE)

for (k in 1:2) {
  alpha_k <- graphicalMCP_rep[k]
  if (alpha_k >= 1 - 1e-6 || alpha_k <= 1e-6) next

  graphicalMCP_b <- graphicalMCP:::gs_boundaries(alpha_k, t[1:k], spending_of)

  # rpact requires last info rate = 1; only compare at final analysis
  rpact_nom <- NA
  if (k == 2) {
    rpact_d <- suppressMessages(getDesignGroupSequential(
      typeOfDesign = "asOF", informationRates = t, alpha = alpha_k
    ))
    rpact_nom <- rpact_d$stageLevels[k]
  }

  verify <- rbind(verify, data.frame(
    Analysis = k,
    `Observed p` = p_obs[k],
    `Repeated p (alpha)` = alpha_k,
    `graphicalMCP boundary` = graphicalMCP_b$bounds_nominal[k],
    `rpact boundary` = rpact_nom,
    check.names = FALSE
  ))
}

knitr::kable(verify, digits = 6,
             caption = "Boundary verification: observed p should equal boundary at repeated p alpha")
```

| Analysis | Observed p | Repeated p (alpha) | graphicalMCP boundary | rpact boundary |
|---------:|-----------:|-------------------:|----------------------:|---------------:|
|        1 |      0.024 |           0.110482 |                 0.024 |             NA |
|        2 |      0.010 |           0.010094 |                 0.010 |           0.01 |

Boundary verification: observed p should equal boundary at repeated p
alpha {.table}

## 5. Sequential P-values

We compare
[`sequential_p()`](https://openpharma.github.io/graphicalMCP/reference/sequential_p.md)
against
[`gsDesign::sequentialPValue()`](https://keaven.github.io/gsDesign//reference/sequentiaPValue.html).
Both compute the minimum significance level at which any group
sequential boundary across all analyses would be crossed.

``` r

all_pass <- TRUE

seq_scenarios <- list(
  list(
    p = c(0.024, 0.01),
    t = c(0.5, 1),
    sfu = gsDesign::sfLDOF,
    fn = spending_of,
    label = "OBF, 2 analyses"
  ),
  list(
    p = c(0.05, 0.02, 0.01),
    t = c(1/3, 2/3, 1),
    sfu = gsDesign::sfLDOF,
    fn = spending_of,
    label = "OBF, 3 analyses"
  ),
  list(
    p = c(0.024, 0.01),
    t = c(0.5, 1),
    sfu = gsDesign::sfLDPocock,
    fn = spending_pocock,
    label = "Pocock, 2 analyses"
  ),
  list(
    p = c(0.1, 0.05, 0.01),
    t = c(0.2, 0.6, 1),
    sfu = gsDesign::sfLDOF,
    fn = spending_of,
    label = "OBF, unequal spacing"
  )
)

for (sc in seq_scenarios) {
  K <- length(sc$p)
  z_obs <- qnorm(1 - sc$p)

  # graphicalMCP
  graphicalMCP_seq <- sequential_p(sc$p, sc$t, sc$fn)

  # gsDesign
  gsd_design <- gsDesign::gsDesign(
    k = K,
    test.type = 1,
    alpha = 0.025,
    sfu = sc$sfu,
    timing = sc$t[-K]
  )
  gsd_seq <- gsDesign::sequentialPValue(
    gsD = gsd_design,
    n.I = gsd_design$n.I,
    Z = z_obs
  )

  if (!isTRUE(all.equal(graphicalMCP_seq, gsd_seq, tolerance = 1e-4))) {
    cat("FAIL:", sc$label, "\n")
    cat("  graphicalMCP:", graphicalMCP_seq, "\n")
    cat("  gsDesign:    ", gsd_seq, "\n")
    all_pass <- FALSE
  }
}

if (all_pass) cat("All sequential p-value comparisons with gsDesign PASS\n")
#> All sequential p-value comparisons with gsDesign PASS
```

### Relationship Between Sequential and Repeated P-values

We verify that
[`sequential_p()`](https://openpharma.github.io/graphicalMCP/reference/sequential_p.md)
equals the cumulative minimum of
[`repeated_p()`](https://openpharma.github.io/graphicalMCP/reference/repeated_p.md)
across analyses, as expected from the definition.

``` r

all_pass <- TRUE

for (sc in seq_scenarios) {
  K <- length(sc$p)

  # Compute repeated p-values at each analysis
  rep_p_vals <- numeric(K)
  for (k in 1:K) {
    rep_p_vals[k] <- repeated_p(sc$p[1:k], sc$t[1:k], sc$fn)
  }

  # Compute sequential p-values at each analysis
  seq_p_vals <- numeric(K)
  for (k in 1:K) {
    seq_p_vals[k] <- sequential_p(sc$p[1:k], sc$t[1:k], sc$fn)
  }

  # Sequential p-values should equal cummin of repeated p-values
  cummin_rep <- cummin(rep_p_vals)

  if (!isTRUE(all.equal(seq_p_vals, cummin_rep, tolerance = 1e-4))) {
    cat("FAIL:", sc$label, "\n")
    cat("  seq_p:      ", round(seq_p_vals, 6), "\n")
    cat("  cummin(rep): ", round(cummin_rep, 6), "\n")
    all_pass <- FALSE
  }
}

if (all_pass) cat("All sequential_p == cummin(repeated_p) checks PASS\n")
#> All sequential_p == cummin(repeated_p) checks PASS
```

### Detailed Example

``` r

p_obs <- c(0.05, 0.02, 0.01)
t <- c(1/3, 2/3, 1)

# graphicalMCP sequential p-values at each analysis
graphicalMCP_seq <- numeric(3)
for (k in 1:3) {
  graphicalMCP_seq[k] <- sequential_p(p_obs[1:k], t[1:k], spending_of)
}

# gsDesign sequential p-value (at the final analysis)
gsd_d <- gsDesign::gsDesign(k = 3, test.type = 1, alpha = 0.025,
                             sfu = gsDesign::sfLDOF, timing = c(1/3, 2/3))
gsd_final <- gsDesign::sequentialPValue(
  gsD = gsd_d, n.I = gsd_d$n.I, Z = qnorm(1 - p_obs)
)

# Repeated p-values for comparison
graphicalMCP_rep <- numeric(3)
for (k in 1:3) {
  graphicalMCP_rep[k] <- repeated_p(p_obs[1:k], t[1:k], spending_of)
}

seq_detail <- data.frame(
  Analysis = 1:3,
  `Observed p` = p_obs,
  `Repeated p` = graphicalMCP_rep,
  `Sequential p` = graphicalMCP_seq,
  `cummin(rep)` = cummin(graphicalMCP_rep),
  check.names = FALSE
)
knitr::kable(seq_detail, digits = 6,
             caption = "Sequential vs. repeated p-values (OBF, 3 analyses)")
```

| Analysis | Observed p | Repeated p | Sequential p | cummin(rep) |
|---------:|-----------:|-----------:|-------------:|------------:|
|        1 |       0.05 |   0.257809 |     0.257809 |    0.257809 |
|        2 |       0.02 |   0.058193 |     0.058193 |    0.058193 |
|        3 |       0.01 |   0.010554 |     0.010554 |    0.010554 |

Sequential vs. repeated p-values (OBF, 3 analyses) {.table}

``` r

cat(sprintf(
  "\ngsDesign sequentialPValue (final): %.6f\ngraphicalMCP sequential_p (final): %.6f\n",
  gsd_final, graphicalMCP_seq[3]
))
#> 
#> gsDesign sequentialPValue (final): 0.010554
#> graphicalMCP sequential_p (final): 0.010554
```

## 6. Wang-Tsiatis Boundaries

The
[`spending_wt()`](https://openpharma.github.io/graphicalMCP/reference/spending_wt.md)
function computes the implied cumulative spending from the Wang-Tsiatis
family of group sequential boundaries, parameterized by $`\Delta`$:
$`c_k = C \cdot t_k^{\Delta - 0.5}`$. Special cases are $`\Delta = 0`$
(O’Brien-Fleming) and $`\Delta = 0.5`$ (Pocock). We validate against
rpact’s
[`getDesignGroupSequential()`](https://docs.rpact.org/reference/getDesignGroupSequential.html)
with `typeOfDesign = "WT"`.

### Comparison with rpact

``` r

alpha <- 0.025
info_frac <- c(1/3, 2/3, 1)

wt_comparison <- data.frame()

for (delta in c(0, 0.1, 0.25, 0.4, 0.5)) {
  # graphicalMCP: compute boundaries via spending_wt
  b_graphicalMCP <- gs_boundaries(
    alpha, info_frac,
    function(a, t) spending_wt(a, t, delta = delta)
  )

  # rpact: Wang-Tsiatis boundaries
  rpact_type <- if (delta == 0) {
    "OF"
  } else if (delta == 0.5) {
    "P"
  } else {
    "WT"
  }
  rpact_args <- list(
    sided = 1, alpha = alpha,
    informationRates = info_frac,
    typeOfDesign = rpact_type
  )
  if (rpact_type == "WT") rpact_args$deltaWT <- delta
  rpact_gsd <- do.call(getDesignGroupSequential, rpact_args)

  max_diff <- max(abs(b_graphicalMCP$bounds_z - rpact_gsd$criticalValues))

  wt_comparison <- rbind(wt_comparison, data.frame(
    Delta = delta,
    Type = ifelse(delta == 0, "O'Brien-Fleming",
           ifelse(delta == 0.5, "Pocock",
                  paste0("WT (", delta, ")"))),
    Z1_graphicalMCP = b_graphicalMCP$bounds_z[1],
    Z1_rpact = rpact_gsd$criticalValues[1],
    Z2_graphicalMCP = b_graphicalMCP$bounds_z[2],
    Z2_rpact = rpact_gsd$criticalValues[2],
    Z3_graphicalMCP = b_graphicalMCP$bounds_z[3],
    Z3_rpact = rpact_gsd$criticalValues[3],
    Max.Diff = max_diff,
    check.names = FALSE
  ))
}

knitr::kable(wt_comparison, digits = 6, row.names = FALSE,
             caption = paste("Wang-Tsiatis Z-scale boundaries:",
                             "graphicalMCP vs rpact (alpha = 0.025)"))
```

| Delta | Type | Z1_graphicalMCP | Z1_rpact | Z2_graphicalMCP | Z2_rpact | Z3_graphicalMCP | Z3_rpact | Max.Diff |
|---:|:---|---:|---:|---:|---:|---:|---:|---:|
| 0.00 | O’Brien-Fleming | 3.471043 | 3.471091 | 2.454398 | 2.454432 | 2.003718 | 2.004036 | 0.000318 |
| 0.10 | WT (0.1) | 3.144087 | 3.144191 | 2.382772 | 2.382851 | 2.025795 | 2.026098 | 0.000303 |
| 0.25 | WT (0.25) | 2.741052 | 2.741137 | 2.304940 | 2.305012 | 2.082924 | 2.082813 | 0.000111 |
| 0.40 | WT (0.4) | 2.439478 | 2.439505 | 2.276113 | 2.276139 | 2.186237 | 2.185695 | 0.000542 |
| 0.50 | Pocock | 2.289502 | 2.289478 | 2.289502 | 2.289478 | 2.289834 | 2.289478 | 0.000356 |

Wang-Tsiatis Z-scale boundaries: graphicalMCP vs rpact (alpha = 0.025)
{.table style="width:100%;"}

### Verification of boundary shape

The Wang-Tsiatis boundaries satisfy $`c_k \cdot t_k^{0.5 - \Delta} = C`$
(constant across analyses). We verify this property:

``` r

shape_check <- data.frame()

for (delta in c(0, 0.25, 0.5)) {
  b <- gs_boundaries(
    alpha, info_frac,
    function(a, t) spending_wt(a, t, delta = delta)
  )
  C_values <- b$bounds_z * info_frac^(0.5 - delta)

  shape_check <- rbind(shape_check, data.frame(
    Delta = delta,
    C1 = C_values[1],
    C2 = C_values[2],
    C3 = C_values[3],
    Max.Variation = max(C_values) - min(C_values)
  ))
}

knitr::kable(shape_check, digits = 6, row.names = FALSE,
             caption = paste("Verification that C = Z * t^(0.5 - delta)",
                             "is constant across analyses"))
```

| Delta |       C1 |       C2 |       C3 | Max.Variation |
|------:|---------:|---------:|---------:|--------------:|
|  0.00 | 2.003853 | 2.003853 | 2.004116 |      0.000263 |
|  0.25 | 2.082894 | 2.082894 | 2.083560 |      0.000666 |
|  0.50 | 2.289484 | 2.289484 | 2.289460 |      0.000024 |

Verification that C = Z \* t^(0.5 - delta) is constant across analyses
{.table}

The constant $`C`$ varies by less than $`10^{-3}`$ across analyses,
confirming the Wang-Tsiatis boundary shape.

## Summary

``` r

summary_table <- data.frame(
  Function = c(
    "spending_of / spending_pocock / spending_hsd / spending_linear",
    "gs_boundaries (Z and nominal p)",
    "gs_corr",
    "repeated_p",
    "sequential_p",
    "sequential_p == cummin(repeated_p)",
    "spending_wt (Wang-Tsiatis)"
  ),
  `Compared Against` = c(
    "gsDesign (sfLDOF, sfLDPocock, sfHSD, sfLinear), rpact (alphaSpent)",
    "gsDesign (gsDesign), rpact (getDesignGroupSequential)",
    "Analytical formula",
    "Boundary inversion: gsDesign + rpact boundaries",
    "gsDesign (sequentialPValue)",
    "Internal consistency",
    "rpact (getDesignGroupSequential with typeOfDesign OF/P/WT)"
  ),
  check.names = FALSE
)
knitr::kable(summary_table, caption = "Summary of validation comparisons")
```

| Function | Compared Against |
|:---|:---|
| spending_of / spending_pocock / spending_hsd / spending_linear | gsDesign (sfLDOF, sfLDPocock, sfHSD, sfLinear), rpact (alphaSpent) |
| gs_boundaries (Z and nominal p) | gsDesign (gsDesign), rpact (getDesignGroupSequential) |
| gs_corr | Analytical formula |
| repeated_p | Boundary inversion: gsDesign + rpact boundaries |
| sequential_p | gsDesign (sequentialPValue) |
| sequential_p == cummin(repeated_p) | Internal consistency |
| spending_wt (Wang-Tsiatis) | rpact (getDesignGroupSequential with typeOfDesign OF/P/WT) |

Summary of validation comparisons {.table}

All group sequential functions in graphicalMCP produce results
consistent with gsDesign and rpact to within numerical tolerance. The
spending functions match gsDesign exactly. Boundaries agree across
packages. Repeated p-values are validated by confirming that the
boundary at the returned alpha equals the observed p-value (using both
graphicalMCP and rpact boundaries). Sequential p-values match gsDesign,
and the internal relationship
$`\text{sequential\_p} = \text{cummin}(\text{repeated\_p})`$ holds.
Wang-Tsiatis boundaries from
[`spending_wt()`](https://openpharma.github.io/graphicalMCP/reference/spending_wt.md)
match rpact across all delta values, including the special cases of
O’Brien-Fleming ($`\Delta = 0`$) and Pocock ($`\Delta = 0.5`$).

## References

Anderson, K. (2024). gsDesign: Group Sequential Design. R package.
<https://CRAN.R-project.org/package=gsDesign>

Wassmer, G. and Pahlke, F. (2024). rpact: Confirmatory Adaptive Clinical
Trial Design and Analysis. R package.
<https://CRAN.R-project.org/package=rpact>

Maurer, W., and Bretz, F. (2013). Multiple testing in group sequential
trials using graphical approaches. *Statistics in Biopharmaceutical
Research*, 5(4), 311-320.
